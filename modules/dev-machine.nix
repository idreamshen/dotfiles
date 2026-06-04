{ config, pkgs, llmAgents, inputs, ... }:

let
  inherit (pkgs) lib stdenv;
  llmAgentsPkgs = llmAgents.packages.${pkgs.system};
  opencodeConfigRaw = builtins.fromJSON (builtins.readFile ./opencode/opencode.json);

  collectSopsPlaceholders = value:
    if builtins.isAttrs value then
      lib.concatLists (lib.mapAttrsToList (_: collectSopsPlaceholders) value)
    else if builtins.isList value then
      lib.concatMap collectSopsPlaceholders value
    else if builtins.isString value then
      let match = builtins.match "sops:([A-Za-z0-9_-]+)" value;
      in if match == null then [] else [ (builtins.elemAt match 0) ]
    else [];

  replaceSopsPlaceholders = value:
    if builtins.isAttrs value then
      lib.mapAttrs (_: replaceSopsPlaceholders) value
    else if builtins.isList value then
      map replaceSopsPlaceholders value
    else if builtins.isString value then
      let match = builtins.match "sops:([A-Za-z0-9_-]+)" value;
      in if match == null then value else config.sops.placeholder.${builtins.elemAt match 0}
    else value;

  opencodeSecretNames = lib.unique (collectSopsPlaceholders opencodeConfigRaw);
  opencodeJson = (pkgs.formats.json {}).generate "opencode.json" (replaceSopsPlaceholders opencodeConfigRaw);
  it2ul = pkgs.stdenvNoCC.mkDerivation {
    pname = "it2ul";
    version = "unstable-2026-05-08";

    src = pkgs.fetchurl {
      url = "https://iterm2.com/utilities/it2ul";
      sha256 = "08m9l7rkacm9kyalp4w0f5ppbiafq9jqj6q1rkd1di2jhx8h49m7";
    };

    dontUnpack = true;

    installPhase = ''
      runHook preInstall

      mkdir -p "$out/bin"
      cp "$src" "$out/bin/it2ul"
      chmod +x "$out/bin/it2ul"
      patchShebangs "$out/bin/it2ul"

      runHook postInstall
    '';
  };
  playwrightCli = pkgs.writeShellScriptBin "playwright" ''
    export PLAYWRIGHT_BROWSERS_PATH="${pkgs.playwright-driver.browsers}"
    export PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1
    exec ${pkgs.nodejs_24}/bin/node ${pkgs.playwright}/cli.js "$@"
  '';
  opencodeWebArgs = [
    "--port" "5097"
    "--hostname" "0.0.0.0"
    "--cors" "https://opencode.prod.idreamshen.com:8443"
    "--cors" "https://opencode-fm.prod.idreamshen.com:8443"
  ];
in {
  imports = [
    ./emacs
    inputs.sops-nix.homeManagerModules.sops
  ];

  sops = {
    defaultSopsFile = ../secrets.yaml;
    age.keyFile = "${config.home.homeDirectory}/.config/sops/age/keys.txt";
    secrets =
      (lib.genAttrs opencodeSecretNames (_: {}))
      // {
        opencode_server_password = {};
      };
    templates."opencode.json" = {
      path = "${config.home.homeDirectory}/.config/opencode/opencode.json";
      mode = "0600";
      content = builtins.readFile opencodeJson;
    };
    templates."opencode-web.env" = lib.mkIf (!stdenv.isDarwin) {
      path = "${config.home.homeDirectory}/.config/opencode/web.env";
      mode = "0600";
      content = "OPENCODE_SERVER_PASSWORD=${config.sops.placeholder.opencode_server_password}";
    };
    templates."opencode-web-start" = lib.mkIf stdenv.isDarwin {
      path = "${config.home.homeDirectory}/.local/bin/opencode-web-start";
      mode = "0700";
      content = ''
        #!/bin/sh
        export PATH="${config.home.homeDirectory}/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
        export OPENCODE_SERVER_PASSWORD="${config.sops.placeholder.opencode_server_password}"
        exec ${llmAgentsPkgs.opencode}/bin/opencode web ${lib.escapeShellArgs opencodeWebArgs}
      '';
    };
  };

  home.packages =
    (with pkgs; [
      age
      sops
      argocd
      bun
      dart
      noto-fonts-cjk-sans
      it2ul
      playwrightCli
      playwright-driver.browsers
      typescript-language-server
      yaml-language-server
      fvm
      plantuml
    ])
    ++ (with llmAgentsPkgs; [
      agent-browser
      antigravity
      opencode
      claude-code
      claude-agent-acp
      gemini-cli
      codex
      codex-acp
      copilot-cli
    ]);

  home.file.".config/opencode/plugins/direnv-shell-env.ts".source = ./opencode/plugins/direnv-shell-env.ts;

  home.sessionVariables = {
    PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright-driver.browsers}";
    PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "1";
  };

  systemd.user.services.opencode-web = lib.mkIf (!stdenv.isDarwin) {
    Unit = {
      Description = "OpenCode Web UI";
    };
    Service = {
      Type = "simple";
      ExecStart = "${llmAgentsPkgs.opencode}/bin/opencode web ${lib.escapeShellArgs opencodeWebArgs}";
      EnvironmentFile = "${config.home.homeDirectory}/.config/opencode/web.env";
      Environment = [
        "PATH=${config.home.homeDirectory}/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
      ];
      Restart = "on-failure";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };

  launchd.agents.opencode-web = lib.mkIf stdenv.isDarwin {
    enable = true;
    config = {
      Label = "opencode-web";
      ProgramArguments = [
        "${config.home.homeDirectory}/.local/bin/opencode-web-start"
      ];
      RunAtLoad = true;
      KeepAlive = true;
    };
  };
}

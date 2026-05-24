{ config, pkgs, llmAgents, inputs, ... }:

let
  inherit (pkgs) lib stdenv;
  llmAgentsPkgs = llmAgents.packages.${pkgs.system};
  opencodeConfig = builtins.fromJSON (builtins.readFile ./opencode/opencode.json);
  opencodeJson = (pkgs.formats.json {}).generate "opencode.json" (opencodeConfig // {
    provider = opencodeConfig.provider // {
      openai = opencodeConfig.provider.openai // {
        options = opencodeConfig.provider.openai.options // {
          apiKey = config.sops.placeholder.openai_api_key;
          baseURL = config.sops.placeholder.openai_base_url;
        };
      };
      anthropic = opencodeConfig.provider.anthropic // {
        options = (opencodeConfig.provider.anthropic.options or {}) // {
          apiKey = config.sops.placeholder.anthropic_api_key;
          baseURL = config.sops.placeholder.anthropic_base_url;
        };
      };
    };
  });
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
in {
  imports = [
    ./emacs.nix
    inputs.sops-nix.homeManagerModules.sops
  ];

  sops = {
    defaultSopsFile = ./opencode/secrets.yaml;
    age.keyFile = "${config.home.homeDirectory}/.config/sops/age/keys.txt";
    secrets.openai_api_key = {};
    secrets.openai_base_url = {};
    secrets.anthropic_api_key = {};
    secrets.anthropic_base_url = {};
    secrets.opencode_server_password = {};
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
        export OPENCODE_SERVER_PASSWORD="${config.sops.placeholder.opencode_server_password}"
        exec ${llmAgentsPkgs.opencode}/bin/opencode web
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
      typescript-language-server
      yaml-language-server
      fvm
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

  systemd.user.services.opencode-web = lib.mkIf (!stdenv.isDarwin) {
    Unit = {
      Description = "OpenCode Web UI";
    };
    Service = {
      Type = "simple";
      ExecStart = "${llmAgentsPkgs.opencode}/bin/opencode web";
      EnvironmentFile = "${config.home.homeDirectory}/.config/opencode/web.env";
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

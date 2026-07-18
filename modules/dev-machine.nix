{ config, pkgs, llmAgents, inputs, ... }:

let
  inherit (pkgs) lib stdenv;
  llmAgentsPkgs = llmAgents.packages.${pkgs.system};
  googleWorkspaceCli = inputs.googleworkspace-cli.packages.${pkgs.system}.gws;

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
  trzszGo = pkgs.stdenvNoCC.mkDerivation {
    pname = "trzsz-go";
    version = "1.2.0";

    src = pkgs.fetchurl {
      url = "https://github.com/trzsz/trzsz-go/releases/download/v1.2.0/trzsz_1.2.0_linux_x86_64.tar.gz";
      hash = "sha256-cOPghHF31Me2gajsGfoACS5CKmxijvnYpdtt+/RhKt0=";
    };

    installPhase = ''
      runHook preInstall

      mkdir -p "$out/bin"
      install -m755 trz tsz trzsz "$out/bin/"

      runHook postInstall
    '';
  };
  playwrightCli = pkgs.writeShellScriptBin "playwright" ''
    export PLAYWRIGHT_BROWSERS_PATH="${pkgs.playwright-driver.browsers}"
    export PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1
    exec ${pkgs.nodejs_24}/bin/node ${pkgs.playwright}/cli.js "$@"
  '';
  # No auth by design: access control relies entirely on VPN/firewall.
  ttydArgs = [
    "-W"
    "-i" "0.0.0.0"
    "-p" "7681"
    "-t" "enableTrzsz=true"
    "-t" "enableSixel=true"
    "-t" "macOptionIsMeta=true"
    "-t" "titleFixed=devbox"
    "${config.programs.zsh.package}/bin/zsh" "-l"
  ];
in {
  imports = [
    ./emacs
    ./pi
    inputs.sops-nix.homeManagerModules.sops
  ];

  config = {
    sops = {
      defaultSopsFile = ../secrets.yaml;
      age.keyFile = "${config.home.homeDirectory}/.config/sops/age/keys.txt";
      secrets = lib.optionalAttrs config.programs.emacs.discordBridge.enable {
        discord_bot_token = {};
      };
      templates."discord-authinfo" = lib.mkIf config.programs.emacs.discordBridge.enable {
        path = "${config.home.homeDirectory}/.config/agent-shell-discord/authinfo";
        mode = "0600";
        content = "machine discord.com login bot password ${config.sops.placeholder.discord_bot_token}";
      };
    };

    home.packages =
      (with pkgs; [
        age
        sops
        argocd
        google-cloud-sdk
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
      ++ [
        googleWorkspaceCli
      ]
      ++ (with llmAgentsPkgs; [
        agent-browser
        claude-code
        claude-agent-acp
        codex
        codex-acp
      ])
      ++ lib.optionals (!stdenv.isDarwin) (with pkgs; [
        ttyd
        trzszGo
        chafa
        libsixel
      ]);

    home.file.".pi/agent/AGENTS.md".source = ./agents/AGENTS.md;
    home.file.".claude/CLAUDE.md".source = ./agents/AGENTS.md;
    home.file.".claude/agents/explore.md".source = ./claude/agents/explore.md;
    home.file.".claude/skills/commit".source = ./claude/skills/commit;
    home.file.".claude/skills/pr".source = ./claude/skills/pr;
    home.file.".claude/skills/push".source = ./claude/skills/push;
    home.sessionVariables = {
      PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright-driver.browsers}";
      PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "1";
    };

    systemd.user.services.ttyd = lib.mkIf (!stdenv.isDarwin) {
      Unit = {
        Description = "ttyd Web Terminal";
        X-Restart-Triggers = [ "${pkgs.ttyd}" "${trzszGo}" "${pkgs.chafa}" "${pkgs.libsixel}" ];
      };
      Service = {
        Type = "simple";
        WorkingDirectory = config.home.homeDirectory;
        ExecStart = "${pkgs.ttyd}/bin/ttyd ${lib.escapeShellArgs ttydArgs}";
        Environment = [
          "PATH=${trzszGo}/bin:${pkgs.chafa}/bin:${pkgs.libsixel}/bin:${config.home.homeDirectory}/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
        ];
        Restart = "on-failure";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };

    systemd.user.services.codex-remote-control = lib.mkIf (!stdenv.isDarwin) {
      Unit = {
        Description = "Codex Remote Control";
        X-Restart-Triggers = [ "${llmAgentsPkgs.codex}" ];
      };
      Service = {
        Type = "simple";
        ExecStart = "${llmAgentsPkgs.codex}/bin/codex remote-control";
        Restart = "on-failure";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
}

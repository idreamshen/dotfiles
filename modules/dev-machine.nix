{ config, pkgs, llmAgents, inputs, ... }:

let
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
    templates."opencode.json" = {
      path = "${config.home.homeDirectory}/.config/opencode/opencode.json";
      mode = "0600";
      content = builtins.readFile opencodeJson;
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
      opencode
      claude-code
      claude-agent-acp
      gemini-cli
      codex
      codex-acp
      copilot-cli
    ]);
}

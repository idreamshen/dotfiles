{ config, pkgs, llmAgents, ... }:

let
  llmAgentsPkgs = llmAgents.packages.${pkgs.system};
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
  ];

  xdg.configFile."opencode/opencode.json".source = ./opencode.json;

  home.packages =
    (with pkgs; [
      bun
      dart
      noto-fonts-cjk-sans
      it2ul
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

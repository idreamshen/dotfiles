{ config, pkgs, llmAgents, ... }:

let
  inherit (pkgs) lib stdenv;
  llmAgentsPkgs = llmAgents.packages.${pkgs.system};
in {
  imports = [
    ./emacs.nix
  ];

  home.packages =
    (with pkgs; [
      bun
      dart
      noto-fonts-cjk-sans
      yaml-language-server
      fvm
    ])
    ++ (with llmAgentsPkgs; [
      agent-browser
      claude-code
      claude-agent-acp
      gemini-cli
      codex
      codex-acp
      copilot-cli
    ]);
}

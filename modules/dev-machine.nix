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
    ])
    ++ (with llmAgentsPkgs; [
      agent-browser
      claude-code
      claude-code-acp
      gemini-cli
      codex
      codex-acp
      copilot-cli
    ]);
}

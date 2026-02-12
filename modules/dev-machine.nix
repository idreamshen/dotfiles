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
      copilot-language-server
      chromium
      dart
      gemini-cli
      github-copilot-cli
      yaml-language-server
    ])
    ++ (with llmAgentsPkgs; [
      agent-browser
      opencode
      claude-code
      claude-code-acp
      codex
      codex-acp
    ]);
}

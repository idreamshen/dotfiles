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
      yaml-language-server
    ])
    ++ (with llmAgentsPkgs; [
      agent-browser
      opencode
      claude-code
      claude-code-acp
      codex
      codex-acp
      gemini-cli
      copilot-cli
      copilot-language-server
    ]);
}

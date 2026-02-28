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
      claude-code
      claude-code-acp
      gemini-cli
    ]);
}

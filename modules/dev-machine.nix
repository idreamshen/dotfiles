{ config, pkgs, llmAgents, ... }:

let
  inherit (pkgs) lib stdenv;
  llmAgentsPkgs = llmAgents.packages.${pkgs.system};
in {
  home.packages =
    (with pkgs; [
      bun
      copilot-language-server
      chromium
      dart
      gemini-cli
      github-copilot-cli
      librime
      rime-data
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

  home.file = {
    ".config/emacs/rime/default.custom.yaml".source = ./rime.yaml;
  };

  programs.emacs = {
    enable = true;

    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ./emacs.el;

      package = pkgs.emacs;

      alwaysEnsure = true;
    };
  };

  xdg.configFile."emacs/init.el".source = ./emacs.el;

  services.emacs = lib.mkIf (!stdenv.isDarwin) {
    enable = true;
  };

  # Clone and update emacs-files repository
  home.activation.cloneEmacsFiles = config.lib.dag.entryAfter ["writeBoundary"] ''
    emacs_files_dir="${config.home.homeDirectory}/emacs-files"

    # Set PATH to include SSH
    export PATH="${pkgs.openssh}/bin:$PATH"

    if [ ! -d "$emacs_files_dir" ]; then
      echo "Cloning emacs-files repository..."
      ${pkgs.git}/bin/git clone git@github.com:idreamshen/emacs-files.git "$emacs_files_dir"
    else
      echo "Updating emacs-files repository..."
      (cd "$emacs_files_dir" && ${pkgs.git}/bin/git pull --rebase --autostash)
    fi
  '';
}

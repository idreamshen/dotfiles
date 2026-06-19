{ config, pkgs, lib, cloneEmacsFiles, ... }:

let
  inherit (pkgs) stdenv;
in {
  options = {
    programs.emacs.orgDirectory = lib.mkOption {
      type = lib.types.str;
      default = "~/emacs-files/";
      description = "Directory containing Org files used by Emacs.";
    };
  };

  config = {
  home.packages = with pkgs; [
    librime
    rime-data
    # Add other emacs-specific packages here if needed
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ./init.el;
      package = pkgs.emacs;
      alwaysEnsure = true;
      extraEmacsPackages = epkgs: with epkgs; [
        agent-shell
        agent-shell-attention
        agent-shell-tramp
        dape
      ] ++ lib.optionals stdenv.isDarwin [
        agent-shell-macext
      ];
    };
  };

  xdg.configFile."emacs/init.el".source = ./init.el;
  xdg.configFile."emacs/lisp/my-agent-shell-config.el".source = ./my-agent-shell-config.el;
  xdg.configFile."emacs/lisp/dape-config.el".source = ./dape-config.el;
  xdg.configFile."emacs/lisp/org-task-ai.el".source = ./org-task-ai.el;
  xdg.configFile."emacs/lisp/worktree-manager.el".source = ./worktree-manager.el;
  xdg.configFile."emacs/local.el".text = ''
    (setq my/org-directory ${builtins.toJSON config.programs.emacs.orgDirectory})
  '';
  xdg.configFile."emacs/rime/default.custom.yaml".source = ./rime.yaml;

  services.emacs.enable = true;

  # Clone and update emacs-files repository
  home.activation.cloneEmacsFiles = lib.mkIf cloneEmacsFiles (config.lib.dag.entryAfter ["writeBoundary"] ''
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
  '');
  };
}

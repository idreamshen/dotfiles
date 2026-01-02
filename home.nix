{ config, pkgs, username, homeDirectory, ... }:

let
  inherit (pkgs) lib stdenv;
in {
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = username;
  home.homeDirectory = homeDirectory;

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "25.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
    htop
    gemini-cli
    github-copilot-cli
    claude-code
    ripgrep
    jq
  ] ++ lib.optional stdenv.isDarwin iterm2;

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/idreamshen/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    COLORTERM = "truecolor";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.emacs = {
    enable = true;

    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ./emacs.el;

      package = pkgs.emacs;

      alwaysEnsure = true;

      # extraEmacsPackages = epkgs: [ epkgs.vterm ];
    };
  };

  xdg.configFile."emacs/init.el".source = ./emacs.el;

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.bash = {
    enable = true;
  };

  programs.starship = {
    enable = true;
    settings = {
      add_newline = false;
      line_break.disabled = true;
      aws.disabled = true;
      gcloud.disabled = true;
      character = {
        success_symbol = "[>](green)";
        error_symbol   = "[>](red)";
      };
    };
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    history = {
      size = 4096;
      ignoreAllDups = true;
      path = "$HOME/.zsh_history";
    };
    shellAliases = {
      update-dotfiles = "update_dotfiles";
    };
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "sudo" ];
    };
    initContent = ''
      update_dotfiles() {
        local profile="$1"

        if [ -z "$profile" ]; then
          local profiles
          profiles=$(cd "$HOME/.config/home-manager" && \
            nix --experimental-features 'nix-command flakes' eval .#homeConfigurations --apply builtins.attrNames --json 2>/dev/null | \
            jq -r '.[]' 2>/dev/null)

          if [ -z "$profiles" ]; then
            echo "Error: No profiles found in flake.nix"
            echo "Please check your flake.nix configuration."
            return 1
          fi

          local profile_list=()
          while IFS= read -r p; do
            [ -n "$p" ] && profile_list+=("$p")
          done <<<"$profiles"

          echo "Available profiles:"
          select profile in ''${profile_list[@]}; do
            if [ -n "$profile" ]; then
              break
            else
              echo "Invalid selection. Please try again."
            fi
          done
        fi

        if [ -z "$profile" ]; then
          echo "Error: No profile selected."
          return 1
        fi

        echo "Updating dotfiles with profile: $profile"
        cd "$HOME/.config/home-manager" &&
          git pull --rebase --autostash &&
          home-manager switch --flake ".#''${profile}"
      }
    '';
  };

  services.emacs = lib.mkIf (!stdenv.isDarwin) {
    enable = true;
  };

  programs.git = {
    enable = true;
    settings = {
      user = {
        name = "idreamshen";
        email = "idream.shen@gmail.com";
      };
      init.defaultBranch = "main";
    };
  };

  programs.ssh = {
    enable = true;

    enableDefaultConfig = false;

    matchBlocks = {
      "github.com" = {
        hostname = "ssh.github.com";
        port = 443;
        user = "git";
        identityFile = "~/.ssh/id_ed25519";
      };
    };
  };

  systemd.user = lib.mkIf (!stdenv.isDarwin) {
    startServices = "sd-switch";
  };
}

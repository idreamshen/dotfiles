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
    coreutils
    gnumake
    htop
    jq
    magic-wormhole
    ripgrep
    tree
    nodejs_24
    pnpm
    python315
    gh
    curl
    wget
    openssh
  ];

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
    TZ = "Asia/Shanghai";
    PNPM_HOME = "$HOME/.local/share/pnpm";
    SSH_AUTH_SOCK = "$HOME/.ssh/ssh-agent.sock";
    UV_INDEX_URL = "https://pypi.tuna.tsinghua.edu.cn/simple";
  };

  home.file.".config/pip/pip.conf".text = ''
    [global]
    index-url = https://pypi.tuna.tsinghua.edu.cn/simple
  '';

  nix.package = pkgs.nix;
  nix.settings = {
    substituters = [
      "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
      "https://cache.nixos.org/"
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.tmux = {
    enable = true;
    mouse = true;
    extraConfig = ''
      # Reverse mouse scroll direction (natural scrolling)
      bind-key -T root WheelDownPane if-shell -F -t = "#{mouse_any_flag}" "send-keys -M" "if-shell -Ft= '#{pane_in_mode}' 'send-keys -M' 'copy-mode -et='"
      bind-key -T root WheelUpPane if-shell -F -t = "#{mouse_any_flag}" "send-keys -M" ""
      bind-key -T copy-mode WheelUpPane send-keys -X scroll-down
      bind-key -T copy-mode WheelDownPane send-keys -X scroll-up
      bind-key -T copy-mode-vi WheelUpPane send-keys -X scroll-down
      bind-key -T copy-mode-vi WheelDownPane send-keys -X scroll-up
    '';
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
    config = {
      whitelist = {
        prefix = [ "${homeDirectory}/projects" ];
      };
    };
  };

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.bash = {
    enable = true;
  };

  programs.starship = {
    enable = false;
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
      plugins = [ "git" ];
    };
    initContent = ''
      # Node.js configuration
      export PATH="$HOME/.npm-global/bin:$PNPM_HOME:$PATH"

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

  programs.git = {
    enable = true;
    ignores = [
      ".DS_Store"
      "*~"
      "*.swp"
      "*.~undo-tree~"
    ];
    settings = {
      user = {
        name = "idreamshen";
        email = "idream.shen@gmail.com";
      };
      init.defaultBranch = "main";
      push.autoSetupRemote = true;
      url = {
        "ssh://git@github.com/" = {
          insteadOf = "https://github.com/";
        };
      };
    };
  };

  programs.ssh = {
    enable = true;
    addKeysToAgent = "yes";

    enableDefaultConfig = false;

    matchBlocks = {
      "*" = {
        # 每 60 秒发送一个心跳包
        serverAliveInterval = 60;
        # 如果连续 3 次心跳没响应才断开
        serverAliveCountMax = 3;
        # 开启 TCP KeepAlive
        extraOptions = {
          TCPKeepAlive = "yes";
        };
      };
      "github.com" = {
        hostname = "ssh.github.com";
        port = 443;
        user = "git";
        identityFile = "~/.ssh/id_ed25519";
      };
    };
  };
  services.ssh-agent.enable = false;

  systemd.user = lib.mkIf (!stdenv.isDarwin) {
    startServices = "sd-switch";
    services.ssh-agent = {
      Unit = {
        Description = "SSH Authentication Agent";
        Documentation = "man:ssh-agent(1)";
      };
      Service = {
        Type = "simple";
        ExecStartPre = "${pkgs.coreutils}/bin/rm -f %h/.ssh/ssh-agent.sock";
        ExecStart = "${pkgs.openssh}/bin/ssh-agent -D -a %h/.ssh/ssh-agent.sock";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
    services.ssh-add-keys = {
      Unit = {
        Description = "Add SSH keys to agent";
        After = [ "ssh-agent.service" ];
        Requires = [ "ssh-agent.service" ];
      };
      Service = {
        Type = "oneshot";
        Environment = "SSH_AUTH_SOCK=%h/.ssh/ssh-agent.sock";
        ExecStart = "${pkgs.openssh}/bin/ssh-add %h/.ssh/id_ed25519";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
}

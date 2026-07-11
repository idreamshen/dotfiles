{ config, pkgs, lib, cloneEmacsFiles, ... }:

let
  inherit (pkgs) stdenv;

  trampHlo = epkgs: epkgs.trivialBuild {
    pname = "tramp-hlo";
    version = "0.0.2";
    src = pkgs.fetchFromGitHub {
      owner = "jsadusk";
      repo = "tramp-hlo";
      rev = "b726b4042e96ac5cead396c8d12c01e6bad2bd78";
      sha256 = "154w75nh2i58fs7qw4b3rc4j224pnxfbh326h4fbl9kpf9rz9qk5";
    };
    packageRequires = [ ];
  };
in {
  options = {
    programs.emacs.orgDirectory = lib.mkOption {
      type = lib.types.str;
      default = "~/emacs-files/";
      description = "Directory containing Org files used by Emacs.";
    };

    programs.emacs.fmEmacsFiles.enable =
      lib.mkEnableOption "clone fm-emacs-files locally and add it to org-agenda";

    programs.emacs.discordBridge = {
      enable = lib.mkEnableOption "Discord bridge for agent-shell sessions";
      guildId = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "Discord guild (server) id the bridge manages channels in.";
      };
      authorizedUsers = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Discord user ids allowed to drive agents (security allowlist).";
      };
      categoryId = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Optional Discord category id for auto-created project channels.";
      };
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
        (trampHlo epkgs)
      ] ++ lib.optionals stdenv.isDarwin [
        agent-shell-macext
      ] ++ lib.optionals config.programs.emacs.discordBridge.enable [
        websocket
      ];
    };
  };

  xdg.configFile."emacs/init.el".source = ./init.el;
  xdg.configFile."emacs/lisp/my-agent-shell-config.el".source = ./my-agent-shell-config.el;
  xdg.configFile."emacs/lisp/dape-config.el".source = ./dape-config.el;
  xdg.configFile."emacs/lisp/org-task-ai.el".source = ./org-task-ai.el;
  xdg.configFile."emacs/lisp/agent-hub.el".source = ./agent-hub.el;
  xdg.configFile."emacs/lisp/agent-shell-discord.el".source = ./agent-shell-discord.el;
  xdg.configFile."emacs/local.el".text = let
    fmDir = "${config.home.homeDirectory}/projects/feedme/fm-emacs-files/";
    recruitDir = "${config.home.homeDirectory}/projects/feedme/recruit/emacs-files/";
    agendaDirs = [ config.programs.emacs.orgDirectory ]
      ++ lib.optional config.programs.emacs.fmEmacsFiles.enable fmDir
      ++ lib.optional config.programs.emacs.fmEmacsFiles.enable recruitDir;
  in ''
    (setq my/org-directory ${builtins.toJSON config.programs.emacs.orgDirectory})
    (setq my/org-agenda-directories
          (list ${lib.concatMapStringsSep " " builtins.toJSON agendaDirs}))
    ${lib.optionalString config.programs.emacs.fmEmacsFiles.enable
      "(setq my/org-fm-directory ${builtins.toJSON fmDir})"}
    ${lib.optionalString config.programs.emacs.discordBridge.enable ''
      (setq agent-shell-discord-guild-id ${builtins.toJSON config.programs.emacs.discordBridge.guildId})
      (setq agent-shell-discord-authorized-users
            (list ${lib.concatMapStringsSep " " builtins.toJSON config.programs.emacs.discordBridge.authorizedUsers}))
      (setq agent-shell-discord-category-id ${
        if config.programs.emacs.discordBridge.categoryId == null
        then "nil"
        else builtins.toJSON config.programs.emacs.discordBridge.categoryId})
      (setq agent-shell-discord-auto-start t)''}
  '';
  xdg.configFile."emacs/rime/default.custom.yaml".source = ./rime.yaml;

  services.emacs.enable = true;

  # Raise the open-file limit for the launchd-managed Emacs daemon.
  # On macOS, file-notify uses kqueue, which consumes one file descriptor per
  # watched directory. eglot watching large generated trees (e.g. the feedme
  # proto/vendor zod TypeScript output) can exhaust the default limit inherited
  # from launchd and trigger "Opening directory: Too many open files".
  launchd.agents.emacs.config = lib.mkIf stdenv.isDarwin {
    SoftResourceLimits.NumberOfFiles = 65536;
    HardResourceLimits.NumberOfFiles = 65536;
  };

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

  # Clone and update fm-emacs-files repository (company-mbp only)
  home.activation.cloneFmEmacsFiles = lib.mkIf config.programs.emacs.fmEmacsFiles.enable (config.lib.dag.entryAfter ["writeBoundary"] ''
    fm_dir="${config.home.homeDirectory}/projects/feedme/fm-emacs-files"

    # Set PATH to include SSH
    export PATH="${pkgs.openssh}/bin:$PATH"

    if [ ! -d "$fm_dir" ]; then
      echo "Cloning fm-emacs-files repository..."
      mkdir -p "$(dirname "$fm_dir")"
      ${pkgs.git}/bin/git clone git@github.com:idreamshen/fm-emacs-files.git "$fm_dir"
    else
      echo "Updating fm-emacs-files repository..."
      (cd "$fm_dir" && ${pkgs.git}/bin/git pull --rebase --autostash)
    fi
  '');
  };
}

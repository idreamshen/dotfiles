{ ... }:

{
  imports = [
    ./modules/common.nix
    ./modules/dev-machine.nix
    ./modules/osx.nix
  ];

  programs.emacs.orgDirectory = "/ssh:devbox:/home/idreamshen/emacs-files/";
  programs.emacs.fmEmacsFiles.enable = true;
  programs.emacs.discordBridge = {
    enable = false;
    guildId = "1091002018920923220";
    authorizedUsers = [ "*" ];
  };
}

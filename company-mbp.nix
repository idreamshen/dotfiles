{ ... }:

{
  imports = [
    ./modules/common.nix
    ./modules/dev-machine.nix
    ./modules/osx.nix
  ];

  programs.opencode.anthropicApiKeySecret = "sub2api_company_anthropic_api_key";
  programs.emacs.orgDirectory = "/ssh:devbox:/home/idreamshen/emacs-files/";
  programs.emacs.fmEmacsFiles.enable = true;
  programs.emacs.discordBridge = {
    enable = true;
    guildId = "1091002018920923220";
    authorizedUsers = [ "*" ];
  };
}

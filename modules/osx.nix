{ pkgs, lib, ... }:

let
  inherit (pkgs) stdenv;
in {
  home.packages = lib.mkIf stdenv.isDarwin [
    pkgs.iterm2
    pkgs.flameshot
    pkgs.maccy
  ];

  launchd.agents.maccy = lib.mkIf stdenv.isDarwin {
    enable = true;
    config = {
      ProgramArguments = [ "${pkgs.maccy}/Applications/Maccy.app/Contents/MacOS/Maccy" ];
      RunAtLoad = true;
      KeepAlive = false;
    };
  };
}

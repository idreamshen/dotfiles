{ pkgs, lib, ... }:

let
  inherit (pkgs) stdenv;
in {
  home.packages = lib.mkIf stdenv.isDarwin [
    pkgs.iterm2
    pkgs.flameshot
  ];
}

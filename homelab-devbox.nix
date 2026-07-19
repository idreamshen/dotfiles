{ ... }:

{
  imports = [
    ./modules/common.nix
    ./modules/dev-machine.nix
  ];

  programs.pi.projectScopedCpa.enable = true;
}

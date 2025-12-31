{
  description = "Home Manager configuration";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs =
    { nixpkgs, home-manager, emacs-overlay, ... }:
    let
      mkHome = { system, username, homeDirectory }:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [
              (import emacs-overlay)
            ];
          };
        in
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          modules = [ ./home.nix ];
          extraSpecialArgs = {
            inherit username homeDirectory;
          };
        };
    in
    {
      homeConfigurations."homelab-devbox" = mkHome {
        system = "x86_64-linux";
        username = "idreamshen";
        homeDirectory = "/home/idreamshen";
      };
      homeConfigurations."company-mbp" = mkHome {
        system = "aarch64-darwin";
        username = "shenxingyu";
        homeDirectory = "/Users/shenxingyu";
      };
    };
}

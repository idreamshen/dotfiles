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
              (final: prev: {
                emacsPackagesFor = emacs: (prev.emacsPackagesFor emacs).overrideScope (self: super: {
                  agent-shell = self.trivialBuild {
                    pname = "agent-shell";
                    version = "0.29.1-git";
                    src = final.fetchFromGitHub {
                      owner = "xenodium";
                      repo = "agent-shell";
                      rev = "1f60199b34eac48fe31f4c8839c5b0cef933e07e";
                      sha256 = "1fr7vgfkaalm0zfa79l32m164afwcdm6jh6b0nrddd9ilzsaxhmi";
                    };
                    packageRequires = with self; [ shell-maker acp ];
                  };
                  shell-maker = self.trivialBuild {
                    pname = "shell-maker";
                    version = "0.84.4-git";
                    src = final.fetchFromGitHub {
                      owner = "xenodium";
                      repo = "shell-maker";
                      rev = "26fd77a01ca15b441fd13186cb89f6b845a08e7c";
                      sha256 = "0x10qjhcym4nnv5zh4nwy7d46lp158q06w2iwgi265nl1bz9s7yq";
                    };
                    packageRequires = with self; [ ];
                  };
                  acp = self.trivialBuild {
                    pname = "acp";
                    version = "0.8.2-git";
                    src = final.fetchFromGitHub {
                      owner = "xenodium";
                      repo = "acp.el";
                      rev = "7b67facc657a7388a53ea8bba5d6e7eba20fa3e0";
                      sha256 = "0znm5qihx2qy3hgw0idg8j7bnhz8k3yaadff3y6696qckdh0qlnr";
                    };
                    packageRequires = with self; [ ];
                  };
                });
              })
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

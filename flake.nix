{
  description = "Home Manager configuration";

  nixConfig = {
    extra-substituters = [ "https://cache.numtide.com" ];
    extra-trusted-public-keys = [ "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g=" ];
  };

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    llm-agents.url = "github:numtide/llm-agents.nix";
    go-overlay.url = "github:purpleclay/go-overlay";
  };

  outputs =
    { nixpkgs, home-manager, emacs-overlay, llm-agents, go-overlay, ... }:
    let
      mkHome = { system, username, homeDirectory, modules }:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [
              go-overlay.overlays.default
              (import emacs-overlay)
              (final: prev: {
                emacsPackagesFor = emacs: (prev.emacsPackagesFor emacs).overrideScope (self: super: {
                  agent-shell-attention = self.trivialBuild {
                    pname = "agent-shell-attention";
                    version = "0.1.0";
                    src = final.fetchFromGitHub {
                      owner = "ultronozm";
                      repo = "agent-shell-attention.el";
                      rev = "db89dc71e6e2ca5f0a6859ea9e9b183391614cea";
                      sha256 = "1swz0aqdylqbrfd134mxy9r2p544fm9n4wmvn1jm1749y6747kkd";
                    };
                    packageRequires = with self; [ agent-shell ];
                  };
                });
              })
            ];
          };
        in
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs modules;

          extraSpecialArgs = {
            inherit username homeDirectory;
            llmAgents = llm-agents;
          };
        };
    in
    {
      homeConfigurations."homelab-devbox" = mkHome {
        system = "x86_64-linux";
        username = "idreamshen";
        homeDirectory = "/home/idreamshen";
        modules = [ ./homelab-devbox.nix ];
      };
      homeConfigurations."company-mbp" = mkHome {
        system = "aarch64-darwin";
        username = "shenxingyu";
        homeDirectory = "/Users/shenxingyu";
        modules = [ ./company-mbp.nix ];
      };
      homeConfigurations."homelab-openclaw" = mkHome {
        system = "x86_64-linux";
        username = "idreamshen";
        homeDirectory = "/home/idreamshen";
        modules = [ ./homelab-openclaw.nix ];
      };
      homeConfigurations."home-mbp" = mkHome {
        system = "x86_64-darwin";
        username = "shenxingyu";
        homeDirectory = "/Users/shenxingyu";
        modules = [ ./home-mbp.nix ];
      };
    };
}

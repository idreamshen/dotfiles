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
    llm-agents.url = "github:numtide/llm-agents.nix";
  };

  outputs =
    { nixpkgs, home-manager, emacs-overlay, llm-agents, ... }:
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
                    version = "0.31.1";
                    src = final.fetchFromGitHub {
                      owner = "xenodium";
                      repo = "agent-shell";
                      rev = "v0.31.1";
                      sha256 = "0arp30sbsigiic2r8vg50ygba26rhghjylr7vzxb80nihbfsd8qj";
                    };
                    packageRequires = with self; [ shell-maker acp ];
                  };
                  shell-maker = self.trivialBuild {
                    pname = "shell-maker";
                    version = "0.84.8";
                    src = final.fetchFromGitHub {
                      owner = "xenodium";
                      repo = "shell-maker";
                      rev = "v0.84.8";
                      sha256 = "1knswylikwipg8aqb7dip2jm1l8q3sxj8q0af31ipn92v6wh9bks";
                    };
                    packageRequires = with self; [ ];
                  };
                  acp = self.trivialBuild {
                    pname = "acp";
                    version = "0.8.3";
                    src = final.fetchFromGitHub {
                      owner = "xenodium";
                      repo = "acp.el";
                      rev = "v0.8.3";
                      sha256 = "0lg5rli3xvkfp0gvpz1bmdv8m8h8abkn1zklxrpgfisxb5axyzii";
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
            llmAgents = llm-agents;
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

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
      mkHome = { system, username, homeDirectory, modules }:
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
        username = "openclaw";
        homeDirectory = "/home/openclaw";
        modules = [ ./homelab-openclaw.nix ];
      };
    };
}

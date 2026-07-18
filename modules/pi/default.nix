{ config, pkgs, lib, llmAgents, ... }:

let
  llmAgentsPkgs = llmAgents.packages.${pkgs.system};

  piAcp = pkgs.buildNpmPackage {
    pname = "pi-acp";
    version = "0.0.31";

    src = pkgs.fetchFromGitHub {
      owner = "svkozak";
      repo = "pi-acp";
      rev = "v0.0.31";
      hash = "sha256-bM3V/3fxkY2Ib+OyfT82StIIRSLXGDuYUbt1CZKpTuo=";
    };

    npmDepsHash = "sha256-qN+b/tMbnJLkWjotl3XrA0nfZ3KT/mT6gM+n3Qiz8Wk=";
  };

  extensionsDir = ".pi/agent/extensions";

  # Static (secret-free) extensions: drop any `.ts` file into ./extensions/ and it
  # is linked into `~/.pi/agent/extensions/` automatically.
  staticExtensions = lib.mapAttrs'
    (name: _: lib.nameValuePair "${extensionsDir}/${name}" {
      source = ./extensions/${name};
    })
    (lib.filterAttrs (name: type: type == "regular" && lib.hasSuffix ".ts" name)
      (builtins.readDir ./extensions));
in {
  config = {
    home.packages = [
      llmAgentsPkgs.pi
      piAcp
    ];

    home.file = staticExtensions;

    # Secret-bearing extensions are rendered via sops templates (values never hit
    # the Nix store as plaintext).
    sops.secrets.pi_anthropic_base_url = {};
    sops.templates."pi-anthropic-base-url" = {
      path = "${config.home.homeDirectory}/${extensionsDir}/anthropic-base-url.ts";
      mode = "0600";
      content = ''
        import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

        export default function (pi: ExtensionAPI) {
          pi.registerProvider("anthropic", {
            baseUrl: "${config.sops.placeholder.pi_anthropic_base_url}",
          });
        }
      '';
    };
  };
}

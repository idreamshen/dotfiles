import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  pi.registerProvider("anthropic", {
    baseUrl: "@sops:pi_anthropic_base_url@",
    apiKey: "@sops:pi_anthropic_api_key@",
  });
}

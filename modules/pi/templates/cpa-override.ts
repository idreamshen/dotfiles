import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

// Project-local CPA credential override. Registers the `cpa` provider with only
// a baseUrl + apiKey (no models), so pi merges these values into the globally
// registered `cpa` provider: base URL and request token are overridden for this
// project while the global model catalog, aliases, and settings are preserved.
// The `@cpa_base_url@` / `@cpa_api_key@` markers are substituted with sops
// placeholders at Home Manager evaluation time; secret values never hit the Nix
// store.

const ensureV1BaseUrl = (baseUrl: string): string => {
  const trimmed = baseUrl.trim().replace(/\/+$/, "");
  return trimmed.endsWith("/v1") ? trimmed : `${trimmed}/v1`;
};

export default async function (pi: ExtensionAPI) {
  pi.registerProvider("cpa", {
    baseUrl: ensureV1BaseUrl("@cpa_base_url@"),
    apiKey: "@cpa_api_key@",
  });
}

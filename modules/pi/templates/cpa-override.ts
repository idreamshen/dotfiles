import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

// Project-local CPA credential override. Registers the `cpa` provider with only
// a baseUrl + apiKey (no models), so pi merges these values into the globally
// registered `cpa` provider: base URL and request token are overridden for this
// project while the global model catalog, aliases, and settings are preserved.
//
// The override is (re)applied on `session_start`, which fires after every
// extension has loaded. Project-local extensions load *before* the global
// `~/.pi/agent/extensions/cpa-provider.ts`, and that global extension registers
// the full `cpa` model catalog with a full replacement that would otherwise
// clobber a load-time override. Re-registering on session_start guarantees this
// project's base URL + token win regardless of extension load order.
//
// The `@cpa_base_url@` / `@cpa_api_key@` markers are substituted with sops
// placeholders at Home Manager evaluation time; secret values never hit the Nix
// store.

const ensureV1BaseUrl = (baseUrl: string): string => {
  const trimmed = baseUrl.trim().replace(/\/+$/, "");
  return trimmed.endsWith("/v1") ? trimmed : `${trimmed}/v1`;
};

const baseUrl = ensureV1BaseUrl("@cpa_base_url@");
const apiKey = "@cpa_api_key@";

export default async function (pi: ExtensionAPI) {
  const applyOverride = () => {
    pi.registerProvider("cpa", { baseUrl, apiKey });
  };

  applyOverride();
  pi.on("session_start", applyOverride);
}

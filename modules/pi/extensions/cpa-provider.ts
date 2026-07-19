import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const DEFAULT_CONTEXT_WINDOW = 128000;
const DEFAULT_MAX_TOKENS = 64000;

const ensureV1BaseUrl = (baseUrl: string): string => {
  const trimmed = baseUrl.trim().replace(/\/+$/, "");
  return trimmed.endsWith("/v1") ? trimmed : `${trimmed}/v1`;
};

type ThinkingLevel = "minimal" | "low" | "medium" | "high" | "xhigh" | "max";

type DiscoveredModel = {
  slug: string;
  display_name?: string;
  context_window?: number;
  max_context_window?: number;
  input_modalities?: string[];
  supported_in_api?: boolean;
  supported_reasoning_levels?: Array<{ effort?: string }>;
};

type ModelsResponse = {
  models?: DiscoveredModel[];
};

const baseUrl = ensureV1BaseUrl("@sops:pi_cpa_base_url@");
const apiKey = "@sops:pi_cpa_api_key@";

const fetchModels = async () => {
  const response = await fetch(`${baseUrl}/models?client_version`, {
    headers: {
      Authorization: `Bearer ${apiKey}`,
    },
  });

  if (!response.ok) {
    throw new Error(`Failed to discover CPA models: ${response.status} ${await response.text()}`);
  }

  const payload = (await response.json()) as ModelsResponse;
  const models = payload.models ?? [];

  if (models.length === 0) {
    throw new Error("Failed to discover CPA models: /models?client_version returned no models");
  }

  return models.filter((model) => model.supported_in_api !== false).map((model) => {
    const supportedThinkingLevels = new Set(
      model.supported_reasoning_levels?.map(({ effort }) => effort).filter(Boolean) ?? [],
    );
    const thinkingLevelMap = Object.fromEntries(
      (["minimal", "low", "medium", "high", "xhigh", "max"] as ThinkingLevel[]).map((level) => [
        level,
        supportedThinkingLevels.has(level) ? level : null,
      ]),
    );
    const reasoning = supportedThinkingLevels.size > 0;
    const supportsImages = model.input_modalities?.some((modality) => modality.toLowerCase() === "image") ?? false;

    return {
      id: model.slug,
      name: model.display_name ?? model.slug,
      reasoning,
      ...(reasoning ? { thinkingLevelMap } : {}),
      input: supportsImages ? (["text", "image"] as ["text", "image"]) : (["text"] as ["text"]),
      cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
      contextWindow: model.context_window ?? model.max_context_window ?? DEFAULT_CONTEXT_WINDOW,
      maxTokens: DEFAULT_MAX_TOKENS,
      compat: {
        supportsDeveloperRole: false,
        supportsReasoningEffort: reasoning,
        maxTokensField: "max_tokens" as const,
      },
    };
  });
};

export default async function (pi: ExtensionAPI) {
  pi.registerProvider("cpa", {
    name: "CPA",
    baseUrl,
    apiKey,
    api: "openai-completions",
    models: await fetchModels(),
  });
}

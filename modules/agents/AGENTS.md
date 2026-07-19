# AGENTS.md

## Do not

- Run broad searches (grep/find/glob/ripgrep) against high-level system directories — the filesystem root `/`, `/root`, the home directory `/Users/shenxingyu`, or `/nix`. These are huge and irrelevant; searching them blows up time and pollutes results.

## Do

- Scope every search to the specific project subdirectory you're working in.
- When delegating to a subagent, restate the relevant constraint rules from every level (CLAUDE.md / AGENTS.md) into its prompt.

## Subagent delegation policy (Pi Agent)

Prefer using `pi-subagents` for non-trivial work instead of doing everything in the parent session.

Use subagents especially for:

- Planning or implementation tasks with multiple steps.
- Codebase exploration before planning.
- External documentation, package, or library research.
- Implementation handoffs.
- Code review after edits.
- Validation, risk, or regression analysis.
- Ambiguous tasks where independent context building would improve correctness.

Default orchestration patterns:

- For unclear or broad tasks, launch fresh-context `context-builder` or `scout` first.
- For external docs/API/library questions, use `researcher`, or combine `researcher` with local `context-builder`.
- For implementation, prefer `planner -> worker -> fresh reviewers -> fix worker`.
- For review, use fresh-context `reviewer` agents with distinct angles such as correctness, tests, simplicity, security, or maintainability.
- For complex planning handoff, use chain mode with one parallel `context-builder` step and distinct output paths under the temporary chain directory.

Safety rules:

- The parent session owns orchestration. Ordinary child subagents must not run subagents.
- Keep exactly one writer per active worktree.
- Parallel subagents should normally be read-only unless isolated worktrees are explicitly used.
- Use `context: "fresh"` for review, validation, research, and independent context building.
- Use forked context only when decision continuity from the parent session is required, such as `oracle` or `worker` handoffs.
- Do not use subagents for tiny direct questions, trivial one-line edits, or cases where delegation would add more overhead than value.
- For review-only tasks, explicitly say: “Do not modify project/source files; returning findings via the configured output artifact is allowed.”
- For large outputs, prefer `outputMode: "file-only"` and named outputs via `as` / `{outputs.name}`.

For non-trivial changes in this Home Manager repo, prefer:

1. Fresh-context `context-builder` or `scout` to inspect relevant Nix modules.
2. `planner` for implementation sequence.
3. One `worker` as the only writer.
4. Fresh-context `reviewer` for Nix style, safety, and validation review.

Do not run parallel writers in this repository. Keep all broad searches scoped to this repo.

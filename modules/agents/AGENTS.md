# AGENTS.md

## Do not

- Run broad searches (grep/find/glob/ripgrep) against high-level system directories — the filesystem root `/`, `/root`, the home directory `/Users/shenxingyu`, `/nix` or `/nix/store`. These are huge and irrelevant; searching them blows up time and pollutes results.

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

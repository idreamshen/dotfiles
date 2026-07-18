# AGENTS.md

## Do not

- Run broad searches (grep/find/glob/ripgrep) against high-level system directories — the filesystem root `/`, `/root`, the home directory `/Users/shenxingyu`, or `/nix`. These are huge and irrelevant; searching them blows up time and pollutes results.
- Read files in `.agent-shell/transcripts`

## Do

- Scope every search to the specific project subdirectory you're working in.
- When delegating to a subagent, restate the relevant constraint rules from every level (CLAUDE.md / AGENTS.md) into its prompt.

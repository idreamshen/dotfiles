---
description: create or return existing PR with conventional title and structured body
agent: build
---

check if a PR already exists for the current branch using `gh pr view --json url --jq .url`. If found, return its URL. If not, inspect the branch commits and diff, then create a PR with a well-formatted title and body.

PR title format:
`(feat|fix|chore|docs|refactor|perf|test|style|ci|build|revert): <concise description>`

Rules:
- Use conventional commits style.
- Keep the title concise, present-tense, and under 72 characters when possible.
- Pick the type based on the main change.
- Do not use vague titles like "update files" or "misc changes".

PR body format:
```
## Summary
- <high-level change>
- <notable behavior/config impact>

## Validation
- <commands run, or "Not run (reason)">
```

Create the PR with `gh pr create --title "$title" --body "$body"` and return the URL.

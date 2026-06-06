---
description: commit and push with auto rebase on failure
agent: build
---

commit using conventional commits format `(feat|fix|chore|docs|refactor|perf|test|style|ci|build|revert): <description>` then push. If push fails due to divergence, fetch + rebase and retry push.

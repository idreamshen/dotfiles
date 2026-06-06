---
description: commit with conventional commit format
agent: build
subtask: true
---

Stage and commit with a conventional commit message.

1. Run `git add` with appropriate files (or `git add -A` if not specified).
2. Run `git commit -m "<type>: <description>"` where type is one of `feat|fix|chore|docs|refactor|perf|test|style|ci|build|revert`.

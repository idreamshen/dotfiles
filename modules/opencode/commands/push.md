---
description: commit and push with auto rebase on failure
agent: build
subtask: true
---

1. Stage and commit with a conventional commit message:
   - `git add` appropriate files (or `git add -A`).
   - `git commit -m "<type>: <description>"` where type is one of `feat|fix|chore|docs|refactor|perf|test|style|ci|build|revert`.
2. Push with `git push`. If push fails due to divergence, run `git fetch && git rebase` then retry push.

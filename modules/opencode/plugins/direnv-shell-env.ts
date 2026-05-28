import type { Plugin } from "@opencode-ai/plugin"
import { execFile } from "node:child_process"
import { promisify } from "node:util"

const execFileP = promisify(execFile)

export const DirenvShellEnv: Plugin = async ({ client }) => {
  return {
    "shell.env": async (input, output) => {
      const cwd = input.cwd
      if (!cwd) return

      try {
        const { stdout } = await execFileP("direnv", ["export", "json"], {
          cwd,
          env: process.env,
          timeout: 60_000,
          maxBuffer: 4 * 1024 * 1024,
        })
        const env = stdout.trim() ? (JSON.parse(stdout) as Record<string, string>) : {}

        for (const [k, v] of Object.entries(env)) {
          if (v != null) output.env[k] = v
        }
      } catch (err) {
        await client.app.log({
          body: {
            service: "direnv-shell-env",
            level: "warn",
            message: "direnv export failed",
            extra: { cwd, error: String(err) },
          },
        })
      }
    },
  }
}

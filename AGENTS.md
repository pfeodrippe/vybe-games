# Interacting with the Clojure REPL

This project uses a live Clojure REPL for iterative development. Short notes on how to interact with it from this repository:

- Reload a namespace (from the REPL):

  (require 'noel :reload)

- Call an init function (non-blocking preferred):

  ;; in the `noel` namespace (after require) you can call:
  (noel/init)

- If you need to call the main entry (may start threads / UI):

  ;; this typically starts the app and can block or spawn threads
  (noel/-main)

- To inspect REPL output/logs programmatically in this assistant environment, use the provided tooling to fetch the REPL log since a line (tool: clojure_repl_output_log).

- When making changes to files, prefer `require` with `:reload` or use your editor REPL tooling to reload the namespace before calling functions.

Note about workflow for code changes

- For this project, when the game is already running you generally don't need to call `(init)` or restart the app after editing a `.clj` file. Instead, reload the namespace that you edited in the running REPL with:

  (require 'your.namespace :reload)

  This re-evaluates the namespace and redefines the vars (functions, defs, etc.). Use this for any `.clj` file in the project (not only `noel.clj`) to get your changes picked up immediately.

- Caveat: reloading a namespace will re-run top-level code in that namespace (including side-effects like audio init). If you want to avoid re-running side-effects, prefer evaluating individual function forms in the REPL or use your editor's "eval top-level form" feature to target only the changed defs.

Flecs-safe REPL rule

- If you evaluate any code that interacts with Flecs (the `vybe.flecs` APIs or the running world), wrap the entire evaluation with `vybe.raylib/t` so it runs safely in the game's runtime context. Example:

  (vybe.raylib/t
    ;; your Flecs-interacting forms here
    (require 'vybe.flecs)
    (let [w (var-get #'noel/w)]
      ;; safe inspection/manipulation...
      ))

  This prevents assertion failures and other unsafe interactions when the native Flecs world is being used by the running game.

Notes
- Avoid calling blocking UI main loops in shared automated environments unless you expect them to run (they may hang the REPL).
- Prefer calling `init` when you want to initialize state without taking over the main thread.

REPL run recording rule (MANDATORY)

- Purpose: create a complete, auditable trail of every snippet the assistant or a developer evaluates in the running REPL. This must be kept up-to-date for reproducibility and debugging.

- Before-eval (required): ALWAYS append a pre-run entry to `REPL_RUNS.md` _before_ executing code via the assistant REPL tooling. The pre-run entry must contain:
  - UTC timestamp in ISO8601 format.
  - The exact code snippet that will be evaluated (byte-for-byte).
  - A one-line intent note (why the snippet will be executed).

  Example pre-run entry:

  ```markdown
  ## Run at 2025-11-08T13:40:00Z  # UTC

  ```clojure
  ;; intent: count entities with :vf/observer
  (vybe.raylib/t
    (require 'vybe.flecs)
    ;; ... exact snippet ...)
  ```
  ```

- Execution: When running code that interacts with Flecs or the running world, the snippet MUST be wrapped with `vybe.raylib/t` as described above. If you forget to wrap it, stop and update the code to include the wrapper.

- After-eval (required): Immediately after the evaluation finishes, append the evaluation output to the same `REPL_RUNS.md` entry. Include:
  - The exact printed stdout and stderr (as captured by the REPL).
  - The returned value (if any) or an explicit note that the evaluation returned nil/failed.
  - Any thrown exception message and stack summary (if present).

  Example post-run addition to the same entry:

  ```markdown
  Output:

  with-query :vf/observer count: 0
  sample: ()
  ```

- Failure policy: If the assistant cannot edit files via the usual editing tool (apply_patch) due to an internal error, it MUST fall back to appending the entry using a terminal append and then report the fallback in the conversation. The assistant must not proceed with an evaluation until the pre-run entry is recorded one way or another.

- Responsibility: The assistant is responsible for performing the pre- and post-run recording on every REPL snippet it executes. If a human runs snippets directly in the REPL, they should follow the same recording rules.

- Checklist to follow for every REPL-driven evaluation:
  1. Prepare exact snippet and intent note.
 2. Append pre-run entry to `REPL_RUNS.md` (apply_patch preferred, terminal fallback allowed).
 3. Execute snippet (wrap with `vybe.raylib/t` if interacting with Flecs).
 4. Append execution output/result to the same entry.
 5. Mention in the conversation that the audit file was updated and where to find the entry.

Note: This rule is enforced for the assistant's runs. Humans are asked to follow it when making reproductions.

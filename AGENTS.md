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

- Execution: When running code that interacts with Flecs or the running world, the snippet MUST be wrapped with `vybe.raylib/t` as described above. If you forget to wrap it, stop and update the code to include the wrapper.

- Failure policy: If the assistant cannot edit files via the usual editing tool (apply_patch) due to an internal error, it MUST fall back to appending the entry using a terminal append and then report the fallback in the conversation. The assistant must not proceed with an evaluation until the pre-run entry is recorded one way or another.

- Checklist to follow for every REPL-driven evaluation:
  1. Prepare exact snippet and intent note.
  2. Report it back to the user.
  3. Execute snippet (wrap with `vybe.raylib/t` if interacting with Flecs).
  4. Append execution output/result to the same entry.
  5. Mention in the conversation that the audit file was updated and where to find the entry.

Note: This rule is enforced for the assistant's runs. Humans are asked to follow it when making reproductions.

Bracket tracking and iterative function development

- When replacing or creating complex functions, follow these steps STRICTLY:
  1. **First evaluation step**: Define a simplified version and test it in the REPL with clojure_evaluate_code
  2. **Verify step**: After each modification, use clojure_evaluate_code to test the function
  3. **Bracket safety**: Always use clojure_balance_brackets for the complete function before finalizing
  4. **Small increments**: Build functions incrementally, not all at once
  5. **Test modifications**: After each file edit, reload and test in REPL before proceeding

CRITICAL: After every file change
- ALWAYS reload the namespace using clojure_evaluate_code with: `(require 'noel :reload)`
- This ensures changes are picked up by the running REPL
- NEVER skip this step - it's essential for testing changes immediately
- Note: The REPL must be connected (game must be running). If REPL is not connected, notify user to start the game first

Example workflow for complex function changes:
  ```
  Step 1: Create base function structure → evaluate in REPL
  Step 2: Add first feature → evaluate in REPL
  Step 3: Add animation logic → evaluate in REPL
  Step 4: Add color transitions → evaluate in REPL
  Step 5: Final balance check → use bracket balancer
  Step 6: Reload namespace and confirm visual output
  ```

- Never attempt multiple large modifications in one edit. Always verify incrementally.

```

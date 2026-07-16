# CLAUDE.md

Guidance for Claude Code when working in this Emacs configuration repo.

## Purpose & philosophy

- This is a hand-built Emacs config ("built from the ground up, so that I know
  what it's doing"). Understandability beats cleverness — every piece should be
  explainable.
- **Prefer built-in Emacs features over third-party packages.** Many third-party
  packages are currently in use; when reviewing or extending the config, look
  for opportunities to replace them with built-ins (e.g. project.el, eglot,
  flymake, which-key) where the built-in is good enough. A third-party package
  must earn its place.
- This config is under **continual improvement** — it is iterated on, never
  "finished". Small, reviewable changes are preferred over large rewrites.
- Part of the goal is developing better editing/development practices generally,
  not just accumulating features. Suggestions that improve workflow habits are
  welcome.

## Hard constraints

- **No modal editing.** Never suggest evil, viper, meow, or vim-style bindings.
- **Must work everywhere it's deployed:**
  - CLI-only (terminal) Linux servers — every feature must degrade gracefully
    or be guarded when there's no GUI (`display-graphic-p` / `window-system`).
  - Graphical workstations on Linux and macOS.
  - No Windows support needed.
- **Shared via GitHub.** The personal repo is upstream; a work fork maintains a
  branch with work-specific customizations on top. Keep `main` free of anything
  machine- or employer-specific, and keep changes merge-friendly for that
  downstream branch (avoid gratuitous reformatting or reordering).
- **Emacs 30.2 is the minimum version** across all deployments — features from
  29/30 (built-in use-package, eglot, which-key, editorconfig, treesit, etc.)
  can be relied on unconditionally.

## Structure

- `early-init.el` — GC/file-handler startup tweaks, disable GUI chrome early.
- `init.el` — sets `custom-file`, adds `lisp/` to load-path, `require`s the
  modules in order. Keep it minimal; real config lives in modules.
- `lisp/mod-*.el` — one module per concern (ui, window, keys, help, project,
  completion, git, shell, programming, markdown, ai, cpp, python). `mod-init`
  bootstraps the package manager; `mod-last` runs final setup.
- New functionality goes in the appropriate `mod-*.el` (or a new module wired
  into `init.el`), not in `init.el` itself.

## Conventions

- Package manager: straight.el with `use-package` (`straight-use-package-by-default t`).
  **straight.el is a deliberate, permanent choice — do not propose migrating to
  package.el.** The work environment's proxy blocks the ELPA/MELPA package
  archives, but `git clone` from GitHub is allowed, which is exactly straight's
  model (and it's the familiar tool). Consequence: built-in packages must be
  declared with `:straight nil`, otherwise straight clones the upstream repo
  and shadows the built-in.
- All elisp files use `lexical-binding: t` and end with `provide` + a
  `;;; ... ends here` footer.
- Personal functions/variables are prefixed `ar/`.
- Customize output is quarantined in `custom.el` (gitignored); don't put
  configuration there.
- Prefer `use-package` blocks with `:hook`/`:custom`/`:bind` over raw
  `add-hook`/`setq` scattered around. This applies to built-in packages too:
  one greppable declaration per package, with `:straight nil` marking
  "use the built-in". Prefer deferred loading (`:hook`/`:bind`/`:mode`/
  `:commands`) whenever the package isn't needed at startup.
- Exception: `mod-last.el`-style final setup may use bare function calls.
- When a package becomes built-in in a *newer* Emacs than some instances run
  (e.g. internalized in 31 while others are on 30.x), don't fork the
  `use-package` block. Instead, version-gate it centrally in `mod-init.el` via
  `straight-built-in-pseudo-packages` — straight skips installing any package
  on that list, so the plain declaration uses the built-in where available and
  clones from GitHub elsewhere:

  ```elisp
  ;; Packages built into newer Emacsen than our oldest deployment.
  (dolist (pkg '((foo . "31.1")))
    (when (version<= (cdr pkg) emacs-version)
      (add-to-list 'straight-built-in-pseudo-packages (car pkg))))
  ```

  Once every deployment ships the package, drop it from the list and put
  `:straight nil` in its block.

## Primary use cases

- **C++ development is the main focus** (LSP via clangd, CMake, clang-tidy,
  clang-format). Python, shell, markdown/mermaid, YAML, Docker, Nix, TypeScript
  are secondary but real.
- Magit for git, vterm/eshell for shells, vertico/consult/orderless for
  minibuffer completion.

## Verifying changes

- Syntax/quick check: `emacs --batch -l lisp/mod-<name>.el` (byte-compile
  warnings are worth surfacing).
- Full check: `emacs --debug-init` in a fresh instance; startup must be clean
  on both TTY and GUI.
- Remember changes must not break the CLI-only servers: anything GUI-, font-,
  or icon-related needs a graceful terminal fallback.

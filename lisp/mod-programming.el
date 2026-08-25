;;; mod-programming.el --- Cross-language tools -*- lexical-binding: t; -*-

;; Never use tabs by default
(setq-default indent-tabs-mode nil)

;; -- tree-sitter
;;
;; Emacs 31 does natively what treesit-auto did for us: it already pairs
;; every classic mode with its ts-mode in `treesit-major-mode-remap-alist',
;; each ts-mode registers its own commit-pinned grammar recipe when its
;; library loads, and a missing grammar gets built on first use. All we
;; add is the nix recipe, since nix-ts-mode is third-party.
(use-package treesit
  :straight nil
  :when (and (treesit-available-p) (>= emacs-major-version 31))
  :demand t
  :custom
  (treesit-enabled-modes t)
  (treesit-auto-install-grammar 'always)
  :config
  (add-to-list 'treesit-language-source-alist
               '(nix "https://github.com/nix-community/tree-sitter-nix")))

;; Emacs 30 has neither `treesit-enabled-modes' nor grammar auto-install,
;; so treesit-auto stays until every deployment is on 31.
(use-package treesit-auto
  :when (and (treesit-available-p) (< emacs-major-version 31))
  :custom
  (treesit-auto-install t)
  :config
  (add-to-list 'treesit-auto-recipe-list
	       (make-treesit-auto-recipe
		:lang 'nix
		:ts-mode 'nix-ts-mode
		:remap 'nix-mode
		:url "https://github.com/nix-community/tree-sitter-nix"
		:ext "\\.nix\\'"))
  (global-treesit-auto-mode))

;; elisp-flymake-byte-compile refuses to run on files outside
;; trusted-content (macroexpansion can execute code), so trust our own
;; config. Deliberately excludes straight/ -- visiting third-party
;; sources shouldn't macroexpand them.
(setq trusted-content '("~/.emacs.d/init.el"
                        "~/.emacs.d/early-init.el"
                        "~/.emacs.d/lisp/"))

;; Our config modules can't be byte-compiled standalone (use-package,
;; straight etc. only exist once a session is bootstrapped), so the
;; byte-compile backend yields only false positives there. Keep checkdoc.
(defun ar/elisp-flymake-skip-byte-compile ()
  (when (and buffer-file-name
             (file-in-directory-p buffer-file-name user-emacs-directory))
    (remove-hook 'flymake-diagnostic-functions #'elisp-flymake-byte-compile t)))

(use-package flymake
  :straight nil
  :hook ((prog-mode . flymake-mode)
         (emacs-lisp-mode . ar/elisp-flymake-skip-byte-compile))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;; LSP servers (clangd, pyright) stream large JSON responses; a bigger read
;; buffer means fewer read syscalls and a snappier eglot.
(setq read-process-output-max (* 4 1024 1024))

(use-package eglot
  :straight nil
  :hook (((c++-mode c++-ts-mode
           cmake-mode cmake-ts-mode
           python-mode python-ts-mode
           typescript-mode typescript-ts-mode) . eglot-ensure))
  :custom
  ;; on-type formatting fights with electric-indent; same setting as before
  (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
  :bind (:map eglot-mode-map
              ;; keep the lsp-mode-era C-c l prefix for muscle memory
              ;; (deliberately no eglot-format binding: indentation is
              ;; handled by our c-ts-mode style, not clang-format)
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l h" . eglot-inlay-hints-mode)))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  )

(use-package compile
  :straight nil
  :custom
  ;; On a fresh build, jump to and scroll to the first error rather than
  ;; parking at the top or tailing the very end of *compilation*.
  (compilation-scroll-output 'first-error)
  :config
  (push `(gcc-strict
	  ,(rx
	    bol
	    (group-n 1
	      ;; File name group.
	      (+ (not (any ":\n")))
	      )
	    ":"
	    (group-n 2
	      ;; Line number
	      (+ (in "0-9")))
	    ":"
	    (group-n 3
	      ;; Column number
              (+ (in "0-9")))
	    ": "
	    (or (group-n 4 "error")
		(group-n 5 "warning")
		(group-n 6
		  (or "note"
		      (: (+ " ")
			 (or "required from"
			     "required by substitution"
			     "In instantiation of"
			     "in 'constexpr' expansion of"
			     ))))
		)
	    )
	    1 2 3 (5 . 6))
	compilation-error-regexp-alist-alist)
  (setq compilation-error-regexp-alist (remove 'gnu compilation-error-regexp-alist))
  (push 'gcc-strict compilation-error-regexp-alist))

(use-package editorconfig
  :straight nil ;; built-in since Emacs 30.1
  ;; Enable eagerly, NOT on after-init: desktop.el's restore also runs
  ;; from after-init-hook and, being added last (mod-last), runs first
  ;; -- buffers would be restored before editorconfig-mode is on, and
  ;; its settings only apply during a buffer's dir-locals pass, leaving
  ;; restored buffers at default indentation until reverted.
  :demand t
  :config (editorconfig-mode 1))

;; Emacs 31 ships auto-mode-alist entries for YAML and Dockerfiles --
;; the yaml-ts-mode-maybe / dockerfile-ts-mode-maybe dispatchers, which
;; honour `treesit-enabled-modes' and install the grammar on first use.
;; Emacs 30 has the ts modes but nothing mapping files to them, so the
;; third-party packages stay there. Drop both blocks once every
;; deployment is on 31.
(use-package yaml-mode
  :when (< emacs-major-version 31)
  :mode "\\.ya?ml\\'" "\\.clang-tidy\\'")

(use-package dockerfile-mode
  :when (< emacs-major-version 31))

;; .clang-tidy is YAML, but carries no extension the built-in entry
;; matches, so it needs saying on 31 too.
(when (>= emacs-major-version 31)
  (add-to-list 'auto-mode-alist '("\\.clang-tidy\\'" . yaml-ts-mode-maybe)))

(defun ar/nix-ts-mode ()
  "Build the nix grammar if it's missing, then enable `nix-ts-mode'.
nix-ts-mode predates `treesit-ensure-installed', so it only warns about
a missing grammar where Emacs' own ts-modes would offer to build one.
Mirrors what the built-in `*-ts-mode-maybe' dispatchers do."
  (interactive)
  (when (fboundp 'treesit-ensure-installed)
    (treesit-ensure-installed 'nix))
  (nix-ts-mode))

(use-package nix-ts-mode
  :mode ("\\.nix\\'" . ar/nix-ts-mode))

;; -- typescript
;;
;; Emacs 31 maps .ts and .tsx itself (typescript-ts-mode-maybe /
;; tsx-ts-mode-maybe, which install the grammars too), so nothing is
;; needed there. Emacs 30 has both ts modes but no auto-mode entries.
;; The third-party typescript-mode is gone entirely: its old
;; `:unless (fboundp 'typescript-ts-mode)' guard already made it dead on
;; 29+, while its own `\\.tsx?\\'' autoload was quietly winning .tsx
;; back off tsx-ts-mode.
(when (< emacs-major-version 31)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))


(provide 'mod-programming)
;;; mod-programming.el ends here

;;; mod-programming.el --- Cross-language tools -*- lexical-binding: t; -*-

;; Never use tabs by default
(setq-default indent-tabs-mode nil)

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))   ;; tabs required in Makefiles


(use-package treesit-auto
  :if (and (fboundp 'treesit-available-p) (treesit-available-p)) ;; FIXME
  :custom
  (treesit-auto-install 't)
  :config
  (add-to-list 'treesit-auto-recipe-list
	       (make-treesit-auto-recipe
		:lang 'nix
		:ts-mode 'nix-ts-mode
		:remap 'nix-mode
		:url "https://github.com/nix-community/tree-sitter-nix"
		:ext "\\.nix\\'"))
  (global-treesit-auto-mode)
  )

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

(with-eval-after-load "compilation"
  (setq compilation-scroll-output 'first-error)
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
  (push 'gcc-strict compilation-error-regexp-alist)
)

(use-package editorconfig
  :straight nil ;; built-in since Emacs 30.1
  ;; Enable eagerly, NOT on after-init: desktop.el's restore also runs
  ;; from after-init-hook and, being added last (mod-last), runs first
  ;; -- buffers would be restored before editorconfig-mode is on, and
  ;; its settings only apply during a buffer's dir-locals pass, leaving
  ;; restored buffers at default indentation until reverted.
  :demand t
  :config (editorconfig-mode 1))

(use-package yaml-mode
  :mode "\\.ya?ml\\'" ".clang-tidy\\'")

(use-package dockerfile-mode)

(use-package nix-ts-mode
  :mode "\\.nix\\'")

;; -- typescript

(use-package typescript-mode
  :unless (fboundp 'typescript-ts-mode)
  :mode "\\.ts\\'"
  )
(when (fboundp 'typescript-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))


(provide 'mod-programming)
;;; mod-programming.el ends here

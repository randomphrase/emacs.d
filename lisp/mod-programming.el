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

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :config
  (push "[/\\\\]\\.cache\\'" lsp-file-watch-ignored-directories) ;; clangd stores its index here
  (push "[/\\\\]build\\(\\.[^/\\\\]+\\)?\\'" lsp-file-watch-ignored-directories) ;; ignore build(.foo) directories too
  :custom
  (lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-enable-on-type-formatting nil)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c++-mode . lsp-deferred)
	 (c++-ts-mode . lsp-deferred)
	 (cmake-mode . lsp-deferred)
	 (cmake-ts-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
	 (typescript-mode . lsp-deferred)
	 (typescript-ts-mode . lsp-deferred)
	 )
  :commands (lsp lsp-deferred))

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
  :hook (after-init . editorconfig-mode))

(use-package yaml-mode
  :mode "\\.ya?ml\\'" ".clang-tidy\\'")

(use-package markdown-mode
  :ensure t)

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

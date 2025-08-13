;; -*- lexical-binding: t -*-

(setq native-comp-async-report-warnings-errors 'silent)

(setq straight-use-package-by-default t)
(setq straight-fix-flycheck t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(straight-use-package 'use-package)


;; custom stuff goes into custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))


;; -- user interface

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;; see https://emacsredux.com/blog/2023/03/16/setting-the-default-font-for-emacs/
(set-frame-font (format "%s-14" (cl-find-if (lambda (fn) (find-font (font-spec :name fn)))
					    '("Cascadia Code" "Menlo" "DejaVu Sans Mono" "Inconsolata")))
		nil t)

(setq split-height-threshold 100)

(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  )

(use-package ef-themes
  :init (load-theme 'ef-night :no-confirm)
  ;; :custom
  ;; (ef-themes-common-palette-overrides '((bg-region bg-cyan-subtle)))
  )

(setq frame-title-format '("Emacs - " user-login-name "@" system-name ":%f"))

(use-package pulsar
  :ensure t
  :init
  (pulsar-global-mode 1)
  :hook
  (next-error . pulsar-pulse-line)
  (consult-after-jump . pulsar-recenter-top)
  (consult-after-jump . pulsar-reveal-entry)
  (imenu-after-jump-hook . pulsar-recenter-top)
  (imenu-after-jump-hook . pulsar-reveal-entry)
  )


;; -- window management

;; stolen from https://karthinks.com/software/emacs-window-management-almanac/

(advice-add 'other-window :before
            (defun other-window-split-if-single (&rest _)
              "Split the frame if there is a single window."
              (when (one-window-p) (split-window-sensibly))))

(defalias 'other-window-alternating
    (let ((direction 1))
      (lambda (&optional arg)
        "Call `other-window', switching directions each time."
        (interactive)
        (if (equal last-command 'other-window-alternating)
            (other-window (* direction (or arg 1)))
          (setq direction (- direction))
          (other-window (* direction (or arg 1)))))))

(keymap-global-set "M-o" 'other-window-alternating)

(defun move-buffer-to-window (wnum)
  "Moves the current buffer to window `WNUM'."
  ;; stolen from spacemacs, see https://github.com/syl20bnr/spacemacs/blob/195090a247496d44907084a3ee1d128f54622216/layers/%2Bspacemacs/spacemacs-defaults/funcs.el#L297
  (interactive)
  (let ((b (current-buffer))
        (w1 (selected-window))
        (w2 (winum-get-window-by-number wnum)))
    (unless (eq w1 w2)
      (set-window-buffer w2 b)
      (switch-to-prev-buffer)
      (unrecord-window-buffer w1 b))
    ;; (when follow-focus-pa
    ;;   (select-window (winum-get-window-by-number windownum))))))
    ))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
	  helpful-mode
	  (special-mode . hide) ;; used for Warnings
	  flycheck-error-list-mode flycheck-verify-mode
	  "^\\*vterm.*\\*$"  vterm-mode
	  ))
  (popper-mode +1)
  (popper-echo-mode +1)
  (setq popper-window-height  (lambda (win)
				(fit-window-to-buffer
				 win
				 (floor (frame-height) 3)
				 (floor (frame-height) 3)
				 )))
  )

;; (use-package ace-window
;;   :bind ("M-o" . ace-window)
;;   )

(defun winum-select-or-move (wnum &optional arg)
  (interactive "P")
  (if arg (move-buffer-to-window wnum)
    (winum-select-window-by-number wnum)))

;; define winum-select-or-move-N functions
(dotimes (i 10)
  (eval `(defun ,(intern (format "winum-select-or-move-%i" i)) (&optional arg)
           ,(format "Select window %i or (with ARG) move the current buffer to that window" i)
           (interactive "P")
           (winum-select-or-move ,i arg))))

(use-package winum
  :bind
  (:map winum-keymap
	("M-0" . winum-select-window-0-or-10)
	("M-1" . winum-select-or-move-1)
	("M-2" . winum-select-or-move-2)
	("M-3" . winum-select-or-move-3)
	("M-4" . winum-select-or-move-4)
	("M-5" . winum-select-or-move-5)
	("M-6" . winum-select-or-move-6)
	("M-7" . winum-select-or-move-7)
	("M-8" . winum-select-or-move-8)
	("M-9" . winum-select-or-move-9))
  :init
  (winum-mode)
  )

;; -- external

(use-package exec-path-from-shell
  :when (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  ;; 
  )

;; -- global keys

(use-package which-key
  :hook
  (after-init . which-key-mode)
)

(use-package delsel
  :hook
  (after-init . delete-selection-mode))

;; Get rid of overwrite-mode, this one is far more useful
(global-set-key [remap overwrite-mode] 'copy-from-above-command)

;; on macos I sometimes hit this by mistake
(global-set-key (kbd "s-<backspace>") 'backward-kill-word)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package mwim
  :bind (("<home>" . mwim-beginning)
	 ("<end>" . mwim-end))
  )

;; my fingers are used to meta as 'the one next to the space bar', so this is to not disappoint them:
(customize-set-variable 'ns-command-modifier 'meta)
(customize-set-variable 'ns-alternate-modifier 'super)

;; -- help

(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-command]  . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key]      . helpful-key)
         ([remap describe-symbol]   . helpful-symbol)
         ("C-c C-d"                 . helpful-at-point)
         :map helpful-mode-map
         ("r"                       . remove-hook-at-point))
  )

(use-package info+
  :ensure t)

(use-package devdocs
  :bind ("C-h D" . devdocs-lookup))

(use-package casual
  :after (dired)
  :bind (:map dired-mode-map ("?" . casual-dired-tmenu)
	 :map ibuffer-mode-map ("?" . casual-ibuffer-tmenu) ;; 'h' and '?' both bound to describe-mode, we'll just take the latter
	 :map Info-mode-map ("?" . casual-info-tmenu)
  ))

;; -- projectile

(use-package savehist
  :init
  (savehist-mode))

(use-package projectile
  :after (savehist)
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)
	      ("<f12>" . projectile-compile-project)
	      ("C-<f12>" . projectile-test-project)
	      ("M-C-<f12>" . recompile)
	      ))

(use-package rg
  :bind ("C-c s" . rg-menu))

(recentf-mode 1)

;; ibuffer is more full featured than list-buffers
(use-package ibuffer-vc
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root)
  :bind ([remap list-buffers] . ibuffer)
  )

;; (use-package which-key-posframe
;;   :after which-key
;;   :init
;;   (setq which-key-posframe-poshandler #'posframe-poshandler-frame-bottom-center)
;;   )

;; -- completion

;; code stolen from https://kristofferbalintona.me/posts/202202270056/

(use-package vertico
  ;; special recipe to load extensions
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :init (vertico-mode)
  :bind (:map vertico-map
	      ("<prior>" . vertico-scroll-down)
	      ("<next>" . vertico-scroll-up)
	      )
  )

(use-package marginalia
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :ensure t
  :config
  (nerd-icons-completion-mode))

(use-package company
  :init
  (global-company-mode)
  )

(use-package company-box
  :hook (company-mode . company-box-mode))

;; (use-package corfu
;;   :when (display-graphic-p)
;;   :hook (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
;;   ;;:ensure t
;;   :custom
;;   ;(tab-always-indent 'complete)
;;   (corfu-auto t)
;;   (corfu-auto-prefix 2)
;;   (corfu-auto-delay 0.25)
;;   (lsp-completion-provider :none)       ; Use corfu instead for lsp completions
;;   :init
;;   (global-corfu-mode)
;;   :config
;;   ;; Setup lsp to use corfu for lsp completion
;;   (defun kb/corfu-setup-lsp ()
;;     "Use orderless completion style with lsp-capf instead of the
;;   default lsp-passthrough."
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless)))
  
;;   )

;; (use-package corfu-terminal
;;   :unless (display-graphic-p)
;;   :init
;;   (corfu-terminal-mode +1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
    :bind (;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ;; ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ("C-x C-r" . consult-recent-file) ;; was find-file-read-only
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ;;("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ("C-." . consult-imenu)
         ;; ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ;; ("M-s d" . consult-find)
         ;; ("M-s D" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ;; ("M-s l" . consult-line)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ;; ("M-s e" . consult-isearch-history)
         ;; :map isearch-mode-map
         ;; ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ;; ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ;; ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ;; ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
	 )
  ;; enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  ;; (setq register-preview-delay 0.5
  ;;       register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  ;; (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  ;; :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;;  consult-theme :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-file-register
  ;;  consult--source-recent-file consult--source-project-recent-file
  ;;  ;; :preview-key "M-."
  ;;  :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  ;; (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)


;; -- magit

(use-package magit)

(use-package magit-delta
  :if (executable-find "delta")
  :hook (magit-mode . magit-delta-mode))

;; -- shell

(use-package ansi-color
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (ansi-color-for-compilation-mode t)
  )

;; -- programming

(use-package treesit-auto
  :if (and (fboundp 'treesit-available-p) (treesit-available-p)) ;; FIXME
  :custom
  (treesit-auto-install 'prompt)
  :config
  (add-to-list 'treesit-auto-recipe-list
	       (make-treesit-auto-recipe
		:lang 'nix
		:ts-mode 'nix-ts-mode
		:remap 'nix-mode
		:url "https://github.com/nix-community/tree-sitter-nix"
		:ext "\\.nix\\'"))
  ;(push 'nix treesit-auto-langs) ;; needed?
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
  :mode "\\.ya?ml\\'")

(use-package markdown-mode
  :ensure t)

(use-package dockerfile-mode)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :custom
  (copilot-max-char 200000)
  :config
  (add-to-list 'copilot-indentation-alist
	       '(emacs-lisp-mode . 2)
	       '(makefile-mode . 4)
	       )
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-mode-map
              ("M-C-<return>" . copilot-accept-completion)
              ("M-C-;" . copilot-accept-completion-by-word)
              ("M-C-'" . copilot-accept-completion-by-line)
	      ))


;; -- C++

(defconst ar-c-style
  '((c-basic-offset . 4)
    (c-offsets-alist . ((inlambda . 0 )
                        (innamespace . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (arglist-cont-nonempty . +)
                        ))
    )
  "My C++ Indentation Style")

(defun my-c-initialization-hook ()
  (c-add-style "ar" ar-c-style)
  (setq c-default-style "ar")
  )
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

;; .ipp files are common in boost
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode) t)

(use-package cmake-mode)

(use-package meson-mode
  :mode "meson.build\\'")


;; -- python

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; -- typescript

(use-package typescript-mode
  :unless (fboundp 'typescript-ts-mode)
  :mode "\\.ts\\'"
  )
(when (fboundp 'typescript-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))

;; -- nix

(use-package nix-ts-mode
  :mode "\\.nix\\'")

;; -- envrc
;; As advised by the doco, this is initialized late in the startup sequence

(use-package envrc
  :if (executable-find "direnv")
  :config (envrc-global-mode)
  :bind ("C-c e" . envrc-command-map)
  )

;; save/restore desktop - do last to ensure all modes are ready
(desktop-save-mode 1)

;; -- server

(server-mode 1)


;;; mod-help.el --- Help & discovery -*- lexical-binding: t; -*-

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

(use-package devdocs
  :bind ("C-h D" . devdocs-lookup))

(use-package casual
  :after (dired)
  :bind (:map dired-mode-map
	 ("?" . casual-dired-tmenu)
	 :map ibuffer-mode-map
	 ("?" . casual-ibuffer-tmenu) ;; 'h' and '?' both bound to describe-mode, we'll just take the latter
	 :map Info-mode-map
	 ("?" . casual-info-tmenu)
	 ("M-[" . Info-history-back)
	 ("M-]" . Info-history-forward)
  ))

;; (use-package which-key :init (which-key-mode 1))
;; (use-package helpful
;;   :commands (helpful-callable helpful-variable helpful-key helpful-command))

;; ;; Handy remaps
;; (global-set-key [remap describe-function] #'helpful-callable)
;; (global-set-key [remap describe-variable] #'helpful-variable)
;; (global-set-key [remap describe-key]      #'helpful-key)

(provide 'mod-help)
;;; mod-help.el ends here

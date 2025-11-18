;;; mod-keys.el --- Global keys -*- lexical-binding: t; -*-


(use-package which-key
  :hook
  (after-init . which-key-mode)
  :custom
  (which-key-max-description-length nil) ;; dont truncate function names
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

;; (use-package which-key-posframe
;;   :after which-key
;;   :init
;;   (setq which-key-posframe-poshandler #'posframe-poshandler-frame-bottom-center)
;;   )

(provide 'mod-keys)
;;; mod-keys.el ends here

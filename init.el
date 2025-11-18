;;; init.el --- Entry point -*- lexical-binding: t; -*-

;; Restore GC and file handlers after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist ar/default-file-name-handler-alist)))

;; Keep Customize out of your code
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; Load-path for ~/ .emacs.d/lisp
(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path lisp-dir))

;; Core load order
(mapc #'require
      '(mod-init         ; package mgr + sane defaults
        mod-ui
        mod-window
        mod-keys
        mod-help
        mod-project
        mod-completion
        mod-git
        mod-shell
        mod-programming
        mod-ai
        mod-cpp
        mod-python
        mod-last))

;;; init.el ends here

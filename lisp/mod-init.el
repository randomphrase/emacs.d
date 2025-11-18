;;; mod-init.el --- Bootstrap & defaults -*- lexical-binding: t; -*-

(setq native-comp-async-report-warnings-errors 'silent)

(setq straight-use-package-by-default t)
;(setq straight-fix-flycheck t)
;(setq straight-cache-autoloads nil)

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

(straight-use-package 'use-package)

;; Files, backups, recentf, etc.
;; (setq inhibit-startup-screen t
;;       ring-bell-function 'ignore
;;       create-lockfiles nil
;;       sentence-end-double-space nil
;;       backup-directory-alist `(("." . ,(locate-user-emacs-file "backups/")))
;;       auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "autosaves/") t)))


(use-package exec-path-from-shell
  :when (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  ;; 
  )

(provide 'mod-init)
;;; mod-init.el ends here

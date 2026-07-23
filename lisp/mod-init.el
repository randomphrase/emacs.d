;;; mod-init.el --- Bootstrap & defaults -*- lexical-binding: t; -*-

(setq native-comp-async-report-warnings-errors 'silent)

(setq straight-use-package-by-default t)
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

;; Built into Emacs 30+ but also declared as a dependency by third-party
;; packages (copilot → editorconfig), which would make straight clone and
;; shadow the built-in. Listing here makes straight skip them everywhere.
(add-to-list 'straight-built-in-pseudo-packages 'editorconfig)

;; Files, backups, recentf, etc.
(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      create-lockfiles nil
      sentence-end-double-space nil
      backup-directory-alist `(("." . ,(locate-user-emacs-file "backups/")))
      auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "autosaves/") t)))
;; Auto-save needs its target directory to exist (backups creates its own)
(make-directory (locate-user-emacs-file "autosaves/") t)

;; Redisplay performance. This config only ever shows left-to-right text, so
;; the bidirectional-display engine never needs to scan for right-to-left
;; runs. (bidi-display-reordering is deliberately left alone -- it's
;; documented as internal-use and setting it directly is discouraged.)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t                       ;; skip bidi paren-matching scans
      redisplay-skip-fontification-on-input t) ;; defer fontification while typing

(use-package exec-path-from-shell
  :when (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  ;; 
  )

(provide 'mod-init)
;;; mod-init.el ends here

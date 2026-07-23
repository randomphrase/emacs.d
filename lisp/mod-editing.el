;;; mod-editing.el --- Editing & interaction defaults -*- lexical-binding: t; -*-

;; A home for small, built-in editing/interaction defaults that don't belong
;; to any single feature module. Several cribbed from
;; https://emacsredux.com/blog/2026/04/07/stealing-from-the-best-emacs-configs/

;; -- kill ring / clipboard

;; Push the system clipboard onto the kill ring before a kill would overwrite
;; it, so an external copy is never silently lost.
(setq save-interprogram-paste-before-kill t)

;; Don't let consecutive identical kills pile up as separate entries.
(setq kill-do-not-save-duplicates t)

;; -- mark

;; After C-u C-SPC, keep popping the mark ring with a bare C-SPC.
(setq set-mark-command-repeat-pop t)

;; -- search

;; Show "match N of M" in the isearch prompt (built-in; obviates anzu).
(setq isearch-lazy-count t)

;; -- files

;; Saving a file that starts with a shebang line makes it executable.
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; Remember point in visited files, and recenter after jumping back to it.
(use-package saveplace
  :straight nil
  :hook (after-init . save-place-mode)
  :config
  (advice-add 'save-place-find-file-hook :after
              (lambda (&rest _)
                (when (and buffer-file-name (get-buffer-window))
                  (recenter)))))

;; -- misc

;; re-builder: type regexps in normal string syntax, not doubled-escaped Lisp
;; read syntax.
(with-eval-after-load 're-builder
  (setq reb-re-syntax 'string))

;; find-file-at-point shouldn't treat the word under point as a hostname and
;; try to ping it.
(with-eval-after-load 'ffap
  (setq ffap-machine-p-known 'reject))

(provide 'mod-editing)
;;; mod-editing.el ends here

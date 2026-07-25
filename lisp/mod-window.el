;;; mod-window.el --- Window management -*- lexical-binding: t; -*-


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

;; When a window is split or deleted, resize all siblings proportionally
;; rather than taking the space from a single neighbour.
(setq window-combination-resize t)

;; Undo/redo window-configuration changes (C-c <left> / C-c <right>).
(use-package winner
  :straight nil
  :hook (after-init . winner-mode))

;; Make C-x 1 reversible: from a single window it restores the prior layout
;; via winner instead of being a dead end.
(defun ar/toggle-delete-other-windows ()
  "Delete other windows; if already the sole window, restore the prior layout."
  (interactive)
  (if (and (bound-and-true-p winner-mode) (one-window-p))
      (winner-undo)
    (delete-other-windows)))
(keymap-global-set "C-x 1" #'ar/toggle-delete-other-windows)

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

;; *Warnings* used to pop up as a normal bottom window; being neither
;; dedicated nor a side window, later display-buffer calls (e.g. a magit
;; diff) would reuse or split it, squeezing the new buffer into two
;; lines. A dedicated bottom *side* window can't be split or reused for
;; other buffers. (Handling *Warnings* via popper was tried and reverted:
;; popper switches the current buffer, which error handlers running at
;; warning time don't tolerate.) Dismiss with q or C-x 0.
(add-to-list 'display-buffer-alist
             '("\\`\\*Warnings\\*\\'"
               (display-buffer-in-side-window)
               (side . bottom)
               (window-height . 8)
               (dedicated . t)))

;; Transient output/diagnostic buffers share one bottom side window --
;; the job popper used to do, now via built-in window placement. A side
;; window is never split or reused for an unrelated buffer, so these
;; panels can't disturb the editing windows (the disease the *Warnings*
;; rule above also cures); sharing slot 0 means only one shows at a
;; time. `derived-mode' conditions match subclasses too, so grep/rg
;; (rg-mode derives from compilation-mode) land here for free.
(add-to-list
 'display-buffer-alist
 '((or (derived-mode . help-mode)
       (derived-mode . helpful-mode)
       (derived-mode . compilation-mode)
       (derived-mode . flymake-diagnostics-buffer-mode)
       (derived-mode . flymake-project-diagnostics-mode)
       "\\`\\*Messages\\*\\'"
       "\\`\\*Async Shell Command\\*\\'"
       "Output\\*\\'")
   (display-buffer-in-side-window)
   (side . bottom)
   (slot . 0)
   (window-height . 0.33)
   (preserve-size . (nil . t))))

;; C-` hides/restores every side window at once -- the popper-toggle
;; analogue. (popper's M-` per-popup cycling is deliberately not
;; replaced; "show/hide my panels" is the operation actually wanted.)
(keymap-global-set "C-`" #'window-toggle-side-windows)

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

(provide 'mod-window)

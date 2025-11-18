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
	  ;; (special-mode . hide) ;; used for Warnings
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

(provide 'mod-window)

;;; mod-ai.el --- LLM/AI setup -*- lexical-binding: t; -*-

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :custom
  (copilot-max-char 200000)
  :config
  ;; Decision: copilot warnings (size limit etc.) stay visible -- the
  ;; *Warnings* side-window rule in mod-window makes them unobtrusive.
  ;; To silence them entirely instead, uncomment this; matching is
  ;; prefix-based, so (copilot) covers every (copilot ...) subtype.
  ;; (Also: copilot-max-char-warning-disable kills just the size nag.)
  ;; (add-to-list 'warning-suppress-types '(copilot))
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


(provide 'mod-ai)
;;; mod-ai.el ends here

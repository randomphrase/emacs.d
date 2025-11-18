;;; mod-ai.el --- LLM/AI setup -*- lexical-binding: t; -*-

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


(provide 'mod-ai)
;;; mod-ai.el ends here

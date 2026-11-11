;;; mod-ai.el --- LLM/AI setup -*- lexical-binding: t; -*-

(defun ar/copilot-server-available-p ()
  "Return non-nil when a local Copilot server is installed.
Check PATH and the directory used by `copilot-install-server' without
loading Copilot.  Respect its customized paths when already set."
  (let ((default-directory (expand-file-name "~/"))
        (server (or (bound-and-true-p copilot-server-executable)
                    "copilot-language-server"))
        (install-dir (or (bound-and-true-p copilot-install-dir)
                         (locate-user-emacs-file ".cache/copilot/"))))
    (or (executable-find server)
        (executable-find (expand-file-name server
                                           (expand-file-name "bin/" install-dir))))))

(defun ar/copilot-mode-if-installed ()
  "Enable Copilot in this buffer only when its server is installed."
  (when (ar/copilot-server-available-p)
    (copilot-mode 1)))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  ;; Keep installation and login available even on hosts without a server.
  :commands (copilot-install-server copilot-login copilot-mode)
  :custom
  (copilot-max-char 200000)
  :config
  ;; Decision: copilot warnings (size limit etc.) stay visible -- the
  ;; *Warnings* side-window rule in mod-window makes them unobtrusive.
  ;; To silence them entirely instead, uncomment this; matching is
  ;; prefix-based, so (copilot) covers every (copilot ...) subtype.
  ;; (Also: copilot-max-char-warning-disable kills just the size nag.)
  ;; (add-to-list 'warning-suppress-types '(copilot))
  (add-to-list 'copilot-indentation-alist '(c-ts-base-mode c-ts-mode-indent-offset))
  :hook (prog-mode . ar/copilot-mode-if-installed)
  :bind (:map copilot-mode-map
              ("M-C-<return>" . copilot-accept-completion)
              ("M-C-;" . copilot-accept-completion-by-word)
              ("M-C-'" . copilot-accept-completion-by-line)
	      ))


(provide 'mod-ai)
;;; mod-ai.el ends here

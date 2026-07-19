;;; mod-markdown.el --- Markdown configuration via go-grip -*- lexical-binding: t; -*-

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :custom
  ;; Fontify ``` fenced blocks with each language's real major mode.
  (markdown-fontify-code-blocks-natively t)
  :bind (:map markdown-mode-map
              ([remap markdown-live-preview-mode] . grip-mode)))

(use-package mermaid-mode
  :mode "\\.mermaid\\'")

(use-package grip-mode
  :straight (grip-mode :type git :host github :repo "randomphrase/grip-mode")
  :after (markdown-mode)
  :custom
  (grip-command 'go-grip)
)

(provide 'mod-markdown)
;;; mod-markdown.el ends here

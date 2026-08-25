;;; mod-cpp.el --- C/C++ setup -*- lexical-binding: t; -*-

(defconst ar-c-style
  '((c-basic-offset . 4)
    (c-offsets-alist . ((inlambda . 0 )
                        (innamespace . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (arglist-cont-nonempty . +)
                        ))
    )
  "My C++ Indentation Style")

(defun my-c-initialization-hook ()
  (c-add-style "ar" ar-c-style)
  (setq c-default-style "ar")
  )
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

;; -- tree-sitter indentation for C/C++
;;
;; Our own preferences, layered over the gnu base style.
;;
;; The two Emacsen need different mechanisms, and neither works on the
;; other:
;;
;; - Emacs 31 has `treesit-simple-indent-override-rules', a list consulted
;;   *before* the mode's own rules, so the rules below are all that needs
;;   stating. It does not exist on 30.
;; - Emacs 30 has no such list, so there we hand `c-ts-mode-indent-style'
;;   a function returning our rules followed by an entire base style, dug
;;   out of the private `c-ts-mode--indent-styles'. That function was
;;   REMOVED in 31 (superseded by `c-ts-mode--simple-indent-rules'), so on
;;   31 the old approach signals void-function on every indent.
;;
;; Drop `ar/c-ts-indent-style' and both `boundp' conditionals once every
;; deployment is on 31.
;; See also https://emacs.stackexchange.com/a/78291
(defconst ar/c-ts-indent-rules
  '(;; dont indent the body of a namespace
    ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)

    ;; use 'normal' indenting of parameters, arguments, etc
    ((parent-is "parenthesized_expression") parent-bol c-ts-mode-indent-offset)
    ((parent-is "argument_list") parent-bol c-ts-mode-indent-offset)
    ((parent-is "parameter_list") parent-bol c-ts-mode-indent-offset)

    ;; opening bracket on the next line after an if/for/while etc
    ((node-is "compound_statement") standalone-parent 0))
  "Indentation rules layered over the base `c-ts-mode' style.")

(defun ar/c-ts-indent-style ()
  "Emacs 30 path: `ar/c-ts-indent-rules' ahead of the gnu base style."
  (append ar/c-ts-indent-rules
          (alist-get 'gnu (c-ts-mode--indent-styles 'cpp))))

;; NB: must be defined *before* the use-package block below -- use-package
;; autoloads any :hook function it finds unbound, from the package's own
;; library, and c-ts-mode.el would never define this one.
(defun ar/c-ts-indent-overrides ()
  "Apply `ar/c-ts-indent-rules' in the current C/C++ buffer.
Both languages are covered because a `.h' visited as `c-or-c++-ts-mode'
may parse as either. No-op before Emacs 31, where `ar/c-ts-indent-style'
carries the same rules instead."
  (when (boundp 'treesit-simple-indent-override-rules)
    (setq treesit-simple-indent-override-rules
          (list (cons 'c ar/c-ts-indent-rules)
                (cons 'cpp ar/c-ts-indent-rules)))))

(use-package c-ts-mode
  :straight nil
  ;; Deliberately NO `(treesit-language-available-p 'c)' guard. It is
  ;; evaluated at startup, but `treesit-auto-install-grammar' is `always'
  ;; (mod-programming), so grammars install lazily -- on the first startup
  ;; after the grammar directory is empty the guard reads nil and silently
  ;; drops this whole block, indent style included. That is exactly what
  ;; happened across the Emacs 31 migration, and it would recur on any
  ;; fresh machine or grammar rebuild. Setting a defcustom in a built-in
  ;; library is harmless whether or not the grammar exists.
  :custom
  (c-ts-mode-indent-style
   (if (boundp 'treesit-simple-indent-override-rules) 'gnu 'ar/c-ts-indent-style))
  :hook ((c-ts-mode c++-ts-mode) . ar/c-ts-indent-overrides))


;; .ipp files are common in boost
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode) t)

(use-package cmake-mode
  :init
  (unless (executable-find "cmake-language-server")
    ;; note workaround for #101
    (shell-command "uv tool install --with \"pygls>= 1.1.1, <2.0.0\" cmake-language-server"))
  )

(provide 'mod-cpp)
;;; mod-cpp.el ends here

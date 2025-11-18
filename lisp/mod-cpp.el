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

;; tree-sit configuration for c++
;; see also https://emacs.stackexchange.com/a/78291
(defun ar-cpp-indent-style()
  "Override C++ indentation style to my preferred one."
  `(
    ;; dont indent the body of a namespace
    ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)

    ;; use 'normal' indenting of parameters, arguments, etc
    ((parent-is "parenthesized_expression") parent-bol c-ts-mode-indent-offset)
    ((parent-is "argument_list") parent-bol c-ts-mode-indent-offset)
    ((parent-is "parameter_list") parent-bol c-ts-mode-indent-offset)

    ;; opening bracket on the next line after an if/for/while etc
    ((node-is "compound_statement") standalone-parent 0)
    
    ;; Append here the indent style you want as base
   ,@(alist-get 'gnu (c-ts-mode--indent-styles 'cpp)))
  )

(use-package c-ts-mode
  :if (treesit-language-available-p 'c)
  :custom
  (c-ts-mode-indent-style 'ar-cpp-indent-style)
  )


;; .ipp files are common in boost
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode) t)

(use-package cmake-mode)

(use-package meson-mode
  :mode "meson.build\\'")


(provide 'mod-cpp)
;;; mod-cpp.el ends here

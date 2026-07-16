;;; mod-project.el --- Project navigation/search -*- lexical-binding: t; -*-

(use-package savehist
  :straight nil
  :init
  (savehist-mode))

(defvar ar/project-test-history nil
  "Minibuffer history for `ar/project-test'.")

(defun ar/project-test ()
  "Run a test command from the project root (cf. `project-compile')."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (command (read-shell-command "Test command: "
                                      (car ar/project-test-history)
                                      'ar/project-test-history)))
    (compile command)))

(use-package project
  :straight nil
  :bind-keymap
  ("C-c p" . project-prefix-map)
  ("s-p" . project-prefix-map)
  :bind (("<f12>" . project-compile)
         ("C-<f12>" . ar/project-test)
         ("M-C-<f12>" . recompile)))

;; Header/impl/test switching (projectile-find-other-file replacement)

(defun ar/find-other-file ()
  "Jump between header and implementation.
Ask clangd when it manages this buffer (it knows the real answer);
otherwise fall back to suffix matching via `ff-find-other-file'."
  (interactive)
  (if (and (bound-and-true-p lsp-mode)
           (derived-mode-p 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode)
           (fboundp 'lsp-clangd-find-other-file))
      (lsp-clangd-find-other-file)
    (ff-find-other-file)))

(defvar ar/test-file-suffix "_tests"
  "Suffix distinguishing a test file from the implementation it tests.")

(defun ar/project-toggle-test-file ()
  "Switch between a file and its test counterpart within the project."
  (interactive)
  (let* ((base (file-name-base buffer-file-name))
         (ext (file-name-extension buffer-file-name))
         (target (concat (if (string-suffix-p ar/test-file-suffix base)
                             (string-remove-suffix ar/test-file-suffix base)
                           (concat base ar/test-file-suffix))
                         "." ext))
         (matches (seq-filter
                   (lambda (f) (equal (file-name-nondirectory f) target))
                   (project-files (project-current t)))))
    (cond ((null matches) (user-error "No %s in project" target))
          ((cdr matches) (find-file (completing-read "File: " matches nil t)))
          (t (find-file (car matches))))))

(keymap-global-set "C-c o" #'ar/find-other-file)
(keymap-global-set "C-c t" #'ar/project-toggle-test-file)

(use-package rg
  :bind ("C-c s" . rg-menu))

(recentf-mode 1)

;; ibuffer is more full featured than list-buffers
(use-package ibuffer-vc
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root)
  :bind ([remap list-buffers] . ibuffer)
  )

(provide 'mod-project)
;;; mod-project.el ends here

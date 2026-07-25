;;; mod-project.el --- Project navigation/search -*- lexical-binding: t; -*-

(use-package savehist
  :straight nil
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  :init
  (savehist-mode))

(use-package project
  :straight nil
  :bind-keymap
  ("C-c p" . project-prefix-map)
  ("s-p" . project-prefix-map)
  :bind ("M-C-<f12>" . recompile))

;; Per-project-type configure/build/test/run commands on top of
;; project.el -- the one projectile feature project.el lacks. Knows
;; CMake (incl. presets and ctest), bazel, make, etc.; for unknown
;; project types the commands prompt once and remember, which also
;; retires the old ar/project-test shim.
(use-package projection
  :bind-keymap ("C-c P" . projection-map)
  :bind (("<f12>" . projection-commands-build-project)
         ("C-<f12>" . projection-commands-test-project))
  :config
  ;; Upstream's bazel predicate only knows WORKSPACE; bzlmod projects
  ;; have MODULE.bazel instead.
  (oset projection-project-type-bazel predicate
        '("WORKSPACE" "WORKSPACE.bazel" "MODULE.bazel"))
  ;; Upstream bug: SPC is bound to '("Extensions" . (SYMBOL)) -- the cdr
  ;; is a *list*, not a keymap, so the whole per-project-type sub-keymap
  ;; (cmake presets etc.) is unreachable. Rebind to the keymap's value.
  (define-key projection-map (kbd "SPC")
              (cons "Extensions" projection-per-project-type-map)))

;; Header/impl/test switching (projectile-find-other-file replacement)

(defun ar/find-other-file ()
  "Jump between header and implementation.
Ask clangd when it manages this buffer (it knows the real answer);
otherwise fall back to suffix matching via `ff-find-other-file'."
  (interactive)
  (let* ((server (and (derived-mode-p 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode)
                      (fboundp 'eglot-current-server)
                      (eglot-current-server)))
         (other (and server
                     (jsonrpc-request server :textDocument/switchSourceHeader
                                      (list :uri (eglot-path-to-uri buffer-file-name))))))
    (if (and other (not (string-empty-p other)))
        (find-file (eglot-uri-to-path other))
      (ff-find-other-file))))

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

;;; early-init.el --- Preload tweaks -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil
      load-prefer-newer t)

;; Startup speed: relax GC & handlers during init
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(defvar ar/default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Reduce UI chrome during startup
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; early-init.el ends here

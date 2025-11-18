;;; early-init.el --- Preload tweaks -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Startup speed: relax GC & handlers during init
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(defvar ar/default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;; early-init.el ends here

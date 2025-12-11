;;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

;; remove searching for .gz files (from geza-herman)
(setq jka-compr-load-suffixes nil)
(jka-compr-update)

;; must after jka-compr
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; disable UI components
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(setq-default mode-line-format nil)

;; startup screen and mesages
(setq inhibit-splash-screen t
      inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)
(advice-add 'display-startup-echo-area-message :override #'ignore)

;; encoding
(set-language-environment "UTF-8")
(setq default-input-method nil)
(setq-default bidi-display-reordering  'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(setq package-enable-at-startup nil)

;; do not byte compile at startup (prepare-user-lisp manually)
(setq user-lisp-auto-scrape nil)

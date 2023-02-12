;; disable GC during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; say no to `package.el'
(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(setq mode-line-format nil)

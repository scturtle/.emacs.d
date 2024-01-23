;;; -*- lexical-binding: t; -*-

;; disable UI components
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(setq mode-line-format nil)

;; disable GC during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024)
                           gc-cons-percentage 0.1)))

;; show startup time
(add-hook 'window-setup-hook
          (lambda () (message "startup in %.2f ms"
                              (* 1000.0 (float-time (time-since before-init-time))))))

;; say no to `package.el'
(setq package-enable-at-startup nil)

;; disable `file-name-handler-alist' during startup
(defvar file-name-handler-alist-backup file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun reset-file-handler-alist-h ()
  (dolist (handler file-name-handler-alist)
    (add-to-list 'file-name-handler-alist-backup handler))
  (setq file-name-handler-alist file-name-handler-alist-backup))
(add-hook 'emacs-startup-hook #'reset-file-handler-alist-h)

;; handy definitions
(defun emacs.d (path) (expand-file-name path user-emacs-directory))
(defconst IS-MAC (eq system-type 'darwin))

;; straight
(setq straight-vc-git-default-clone-depth 1)
(setq straight-check-for-modifications nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; relationship with `use-package'
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-compute-statistics t) ;; M-x use-package-report
(setq use-package-always-defer t)

;; put autogen files into ~/.emacs.d/etc and ~/.emacs.d/var
(use-package no-littering :demand)

(use-package benchmark-init
  ;; :demand
  :config (add-hook 'window-setup-hook 'benchmark-init/deactivate))

(use-package nerd-icons :demand)

;; load custom funcs and UIs
(add-to-list 'custom-theme-load-path (emacs.d "lisp"))
;; (load-theme 'aura t)
(load-theme 'catppuccin-mocha t)
(add-to-list 'load-path (emacs.d "lisp"))
(require 'funcs)
(require 'mudline)
(mudline-mode)

;; defaults
(use-package emacs
  :straight nil
  :init
  (setq user-full-name "scturtle"
        user-mail-address "hi@scturtle.me")

  ;; sensible defaults
  (setq inhibit-splash-screen t)
  (setq initial-scratch-message nil)
  (setq initial-major-mode 'fundamental-mode) ; for *scratch*
  (setq use-short-answers t) ;; y-or-n-p for >= 28
  (delete-selection-mode +1)
  (setq vc-follow-symlinks t)
  (setq apropos-do-all t)
  (setq require-final-newline t)
  (show-paren-mode t)
  (setq show-paren-delay 0.0)

  ;; save history of minibuffer, recent files, last place
  (setq recentf-auto-cleanup nil
        recentf-max-saved-items 400)
  (savehist-mode)
  (recentf-mode)
  (save-place-mode)
  (add-hook 'kill-emacs-hook #'recentf-cleanup)

  ;; enable mouse in terminal
  (add-hook 'tty-setup-hook #'xterm-mouse-mode)

  ;; override the env SHELL for fish is slow to start
  (setq-default shell-file-name "/bin/bash")

  ;; encoding
  (set-charset-priority 'unicode)
  (prefer-coding-system 'utf-8)
  (setq default-input-method nil)
  (setq bidi-inhibit-bpa t)
  (setq-default bidi-display-reordering  'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; IO-related tunings
  (setq read-process-output-max (* 1024 1024))
  (when IS-MAC (setq process-adaptive-read-buffering nil)) ;; eshell

  ;; custom
  (setq custom-file (emacs.d "custom.el"))
  (when (file-exists-p custom-file)
    (load-file custom-file))

  ;; from doom-ui
  ;; scroll in small step not half page
  (setq hscroll-margin 2
        hscroll-step 1
        scroll-conservatively 101
        scroll-margin 0
        scroll-preserve-screen-position t
        auto-window-vscroll nil)
  (blink-cursor-mode -1)
  (setq blink-matching-paren nil)

  ;; from doom-editor
  (setq-default indent-tabs-mode nil
                tab-width 2)
  (setq tab-always-indent nil
        sentence-end-double-space nil)
  (setq create-lockfiles nil
        make-backup-files nil
        auto-save-default nil)

  ;; for prog mode
  (add-hook 'prog-mode-hook 'electric-pair-mode) ;; TODO: smartparens?
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  ;; tab bar
  (setq tab-bar-separator ""
        tab-bar-tab-hints t  ;; show index number
        tab-bar-close-button-show nil
        tab-bar-format '(tab-bar-format-tabs)
        tab-bar-tab-name-truncated-max 16
        tab-bar-tab-name-function 'tab-bar-tab-name-truncated
        tab-bar-tab-name-ellipsis "…")

  ;; display-time-mode
  (setq display-time-format "%a %H:%M"
        display-time-default-load-average nil)

  ;; do not show line number in modeline
  (setq line-number-mode nil)

  ;; diff-mode
  (setq diff-refine nil)  ;; no hunk refinement
  )

(use-package gcmh
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto) ; default is 15s
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  )

(use-package evil
  :demand
  :config (evil-mode)
  :custom
  (evil-want-keybinding nil) ; for evil-collection
  (evil-want-C-g-bindings t)
  (evil-want-C-i-jump nil) ;; unbind tab for neotree/org-mode
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  (evil-want-C-w-delete t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-respect-visual-line-mode nil)
  (evil-symbol-word-search t)
  (evil-want-fine-undo t)
  (evil-undo-system 'undo-fu)
  (evil-ex-interactive-search-highlight 'selected-window) ;; all-windows
  (evil-visual-update-x-selection-p nil)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  )

(use-package evil-collection
  :demand
  :custom
  ;; (evil-collection-mode-list '(custom consult dired ediff flycheck ibuffer info magit minibuffer neotree))
  (evil-collection-magit-want-horizontal-movement t)
  (evil-collection-magit-use-z-for-folds t)
  :config
  (evil-collection-init)
  ;; disable bindings that I don't want
  (advice-add #'evil-collection-neotree-setup :after
              (lambda (&rest _) (evil-collection-define-key 'normal 'neotree-mode-map "z" nil)))
  (advice-add #'evil-collection-view-setup :after
              (lambda (&rest _) (evil-collection-define-key 'normal 'view-mode-map "0" nil)))
  )

;; put after evil for perf issue https://github.com/noctuid/general.el/issues/180
(use-package general
  :demand
  :config

  ;; fix message buffer https://github.com/noctuid/general.el/issues/493
  (add-hook 'after-init-hook
            (lambda (&rest _)
              (when-let ((messages-buffer (get-buffer "*Messages*")))
                (with-current-buffer messages-buffer
                  (evil-normalize-keymaps)))))

  (general-create-definer global-leader
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :keymaps 'override)

  ;; Make SPC u SPC u [...] possible
  (define-key universal-argument-map " u" #'universal-argument-more)

  (global-leader
    "SPC" '(execute-extended-command :wk "execute command")
    [?\t] '(evil-switch-to-windows-last-buffer :wk "prev buffer")
    "u" '(universal-argument :wk "universal")
    ";" '(pp-eval-expression :wk "eval expr")
    ":" '(pp-eval-expression :wk "eval expr")
    "," 'switch-to-buffer
    "*" '(+search-project-for-symbol-at-point :wk "search symbol in project")
    "qq" 'save-buffers-kill-terminal

    "b" '(:ignore t :wk "buffer")
    "bb" 'switch-to-buffer
    "br" 'revert-buffer
    "bd" 'kill-current-buffer
    "bi" 'ibuffer
    "bm" '((lambda () (interactive) (switch-to-buffer "*Messages*")) :wk "message buffer")
    "bk" 'kill-current-buffer
    "bf" #'lsp-format-buffer

    "c" '(:ignore t :wk "code")
    "ca" #'lsp-execute-code-action
    "cr" #'lsp-rename
    "cw" 'delete-trailing-whitespace

    "e" '(:ignore t :wk "error")
    "el" #'flycheck-list-errors
    "ep" #'flycheck-previous-error
    "en" #'flycheck-next-error

    "f" '(:ignore t :wk "file")
    "ff" 'find-file
    "fs" 'save-buffer
    "fr" #'recentf-open-files
    "fp" '((lambda () (interactive) (find-file user-init-file)) :wk "edit init.el")

    "g" '(:ignore t :wk "git")
    "gg" #'magit-status
    "gb" #'magit-blame-addition
    "gp" #'diff-hl-previous-hunk
    "gn" #'diff-hl-next-hunk
    "cp" #'smerge-prev
    "cn" #'smerge-next

    "h" '(:ignore t :wk "help")
    "he" 'view-echo-area-messages
    "hf" 'describe-function
    "hF" 'describe-face
    "hl" 'view-lossage
    "hm" 'describe-mode
    "hk" 'describe-key
    "hp" 'describe-package
    "hv" 'describe-variable

    "n" '(:ignore t :wk "note")
    "nd" #'deft

    "p" '(:ignore t :wk "project")
    "pp" #'projectile-switch-project
    "pr" #'projectile-recentf
    "pi" #'projectile-invalidate-cache
    "pf" #'projectile-find-file
    "ga" #'projectile-find-other-file
    "op" #'neotree-toggle
    "oP" #'+neotree/find-this-file

    "s" '(:ignore t :wk "search")
    "sh" #'+symbol-highlight
    "sb" #'consult-line
    "sp" '(consult-ripgrep :wk "search in project")

    "t"  '(:ignore t :wk "toggle")
    "tl" '(display-line-numbers-mode :wk "toggle line numbers")
    "tt" '(toggle-truncate-lines :wk "toggle truncate lines")
    "tn" #'tab-bar-new-tab
    "tm" #'tab-bar-move-tab
    "1"  '((lambda() (interactive) (tab-bar-select-tab 1)) :wk "switch tab 1")
    "2"  '((lambda() (interactive) (tab-bar-select-tab 2)) :wk "switch tab 2")
    "3"  '((lambda() (interactive) (tab-bar-select-tab 3)) :wk "switch tab 3")
    "4"  '((lambda() (interactive) (tab-bar-select-tab 4)) :wk "switch tab 4")
    "5"  '((lambda() (interactive) (tab-bar-select-tab 5)) :wk "switch tab 5")
    "6"  '((lambda() (interactive) (tab-bar-select-tab 6)) :wk "switch tab 6")
    "7"  '((lambda() (interactive) (tab-bar-select-tab 7)) :wk "switch tab 7")
    "8"  '((lambda() (interactive) (tab-bar-select-tab 8)) :wk "switch tab 8")
    "9"  '((lambda() (interactive) (tab-bar-select-tab 9)) :wk "switch tab 9")

    "w" '(:ignore t :wk "window")
    "wd" #'evil-window-delete
    "wo" 'delete-other-windows
    "wk" #'evil-window-up
    "wj" #'evil-window-down
    "wl" #'evil-window-right
    "wh" #'evil-window-left
    "ww" #'evil-window-next
    )

  ;; override "gd" to `evil-goto-definition'
  (evil-define-key 'normal 'global
    "gd" #'+goto-definition
    "gD" #'lsp-ui-peek-find-references
    "gb" #'xref-go-back
    "gf" #'xref-go-forward)

  ;; mimic emacs key bindings
  (general-define-key
   :states 'insert
   "C-a" 'move-beginning-of-line
   "C-e" 'move-end-of-line
   "C-p" 'previous-line
   "C-n" 'next-line
   "C-d" 'delete-forward-char
   "C-h" 'delete-backward-char
   "C-k" 'kill-line
   "C-g" 'evil-normal-state)

  (general-define-key
   :keymaps 'minibuffer-mode-map
   "<escape>" 'abort-recursive-edit
   "C-h"      'delete-backward-char
   "C-u"      'evil-delete-back-to-indentation)

  ;; oevrride evil ex mode (in ":" "/")
  (general-define-key
   :keymaps '(evil-ex-completion-map evil-ex-search-keymap)
   "C-a" 'move-beginning-of-line
   "C-f" 'forward-char
   "C-b" 'backward-char
   "C-d" 'delete-forward-char
   "C-h" 'delete-backward-char
   "C-u" 'evil-delete-back-to-indentation
   "C-k" 'kill-line)

  ;; don't leave visual mode after shifting
  (evil-define-key 'visual 'global
    "<" #'+evil/shift-left
    ">" #'+evil/shift-right)
  )

;; OSC52 FTW
(use-package clipetty
  :demand
  :config (global-clipetty-mode))

;; display cursor as bar instread of box in terminal
(use-package evil-terminal-cursor-changer
  :demand
  :config (evil-terminal-cursor-changer-activate)
  :custom
  (evil-insert-state-cursor 'bar)
  (evil-visual-state-cursor 'hollow))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO" warning bold)
          ("FIXME" error bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("HACK" font-lock-constant-face bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("NOTE" success bold)
          ("XXX" font-lock-constant-face bold))))

(use-package golden-ratio
  :commands golden-ratio-mode
  :custom
  (golden-ratio-extra-commands '(evil-window-left evil-window-right evil-window-up
                                                  evil-window-down evil-window-next evil-window-prev)))

(use-package evil-nerd-commenter
  :bind
  ("M-;" . 'comment-line)
  ([remap comment-line] . #'evilnc-comment-or-uncomment-lines))

(use-package evil-surround
  :commands evil-surround-region
  :init (evil-define-key 'visual global-map "S" #'evil-surround-region))

(use-package undo-fu
  :demand
  :config
  (setq undo-limit         400000   ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)
  )

(use-package undo-fu-session
  :demand
  :config (undo-fu-session-global-mode)
  :config (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  ;; (which-key-idle-delay 1.5)
  )

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :hook (after-init . vertico-mode)
  :custom
  (vertico-resize nil)
  (vertico-count 17)
  (vertico-cycle t))

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :bind
  (:map minibuffer-local-map
        ("M-A" . #'marginalia-cycle))
  )

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-ignore-case t)
  )

(use-package consult
  :bind
  ([remap apropos]                       . #'consult-apropos)
  ([remap bookmark-jump]                 . #'consult-bookmark)
  ([remap evil-show-marks]               . #'consult-mark)
  ([remap evil-show-registers]           . #'consult-register)
  ([remap goto-line]                     . #'consult-goto-line)
  ([remap imenu]                         . #'consult-imenu)
  ([remap locate]                        . #'consult-locate)
  ([remap load-theme]                    . #'consult-theme)
  ([remap man]                           . #'consult-man)
  ([remap recentf-open-files]            . #'consult-recent-file)
  ([remap switch-to-buffer]              . #'consult-buffer)
  ([remap switch-to-buffer-other-window] . #'consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame]  . #'consult-buffer-other-frame)
  ([remap yank-pop]                      . #'consult-yank-pop)
  ([remap projectile-ripgrep]            . #'consult-ripgrep)
  :custom
  (consult-project-function #'projectile-project-root)
  (consult-narrow-key "<") ;; restrict results to certain groups
  (consult-line-numbers-widen t)
  (consult-async-min-input 2)
  (consult-async-refresh-delay  0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  (completion-in-region-function #'consult-completion-in-region) ;; in `Eval:'
  )

(use-package iedit :commands iedit-start)

(use-package evil-multiedit
  :commands evil-multiedit-mode
  :custom
  (evil-multiedit-follow-matches t))

(use-package treesit
  :demand t
  :straight nil
  :custom
  (treesit-font-lock-level 4)
  (major-mode-remap-alist
   '((c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (python-mode . python-ts-mode))))

(use-package cmake-ts-mode
  :straight nil
  :mode "\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'")

(use-package treesit-auto
  :commands treesit-auto-install-all
  :custom (treesit-auto-langs '(c cpp python rust cmake)))

(use-package treesit-fold
  :straight (:host github :repo "abougouffa/treesit-fold")
  :hook ((c-ts-mode c++-ts-mode python-ts-mode rust-ts-mode) . treesit-fold-mode))


(use-package lsp-mode
  :hook ((c-ts-mode c++-ts-mode python-ts-mode rust-ts-mode) . lsp-deferred)
  :custom
  (lsp-idle-delay 0)
  (eldoc-idle-delay 0)
  (lsp-lens-enable nil)
  (lsp-enable-snippet t) ;; to insert func params
  (lsp-enable-file-watchers nil)
  (lsp-signature-render-documentation nil)
  (lsp-client-packages '(ccls lsp-rust lsp-pyright))
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :none) ;; use corfu
  (lsp-modeline-diagnostics-enable nil) ;; use checker segment
  :custom-face
  ;; remove underline in hover
  (lsp-face-highlight-read ((t :underline nil)))
  :config
  ;; fix modeline progress report, see `mudline-segment-misc-info'
  (advice-add #'lsp--progress-status :filter-return
              (lambda (s) (unless (null s) (replace-regexp-in-string "%" "%%%%" s))))
  ;; for corfu, original style is lsp-passthrough
  (defun +lsp-orderless () (setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (orderless)))))
  (add-hook 'lsp-completion-mode-hook #'+lsp-orderless)
  ;; setup the orderless-flex style for its 1st search term
  (defun +orderless-flex-first (_pat idx _tot) (if (eq idx 0) 'orderless-flex 'orderless-literal))
  (add-hook 'orderless-style-dispatchers #'+orderless-flex-first nil 'local)
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil)
  :bind
  (:map lsp-ui-peek-mode-map
        ("k" . #'lsp-ui-peek--select-prev)
        ("j" . #'lsp-ui-peek--select-next)))

(use-package yasnippet
  :hook (lsp-mode . yas-minor-mode) ;; for yas-enable-snippet
  )

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :hook (prog-mode . corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-preview-current nil)
  (corfu-on-exact-match 'show)
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  ;; instead of using nerd-icons, why not just a simple space margin
  (defun +simple-margin-formatter (_) (lambda (_) " "))
  (add-to-list 'corfu-margin-formatters #'+simple-margin-formatter)
  :bind
  (:map corfu-map
        ("M-SPC" . 'corfu-insert-separator)
        ("M-m"   . 'corfu-move-to-minibuffer)))

(use-package dabbrev
  :straight nil
  :custom
  (dabbrev-case-replace nil) ;; do not downcase
  (dabbrev-check-other-buffers nil) ;; only in this buffer
  :bind (("M-/" . dabbrev-completion)))

(use-package corfu-terminal
  :straight (:host github :repo "scturtle/corfu-terminal")
  :hook (corfu-mode . corfu-terminal-mode))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-indication-mode 'left-margin)
  (flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-display-errors-delay '0.2)
  ;; less warnings (for editing config like this init.el)
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (setq flycheck-emacs-lisp-check-form
        (prin1-to-string
         `(progn
            (setq byte-compile-warnings '(not free-vars noruntime unresolved))
            ,(read (default-toplevel-value 'flycheck-emacs-lisp-check-form)))))
  )

(use-package c-ts-mode
  :straight nil
  :config
  (defun +my-indent-style()
    `(((parent-is "declaration_list") parent-bol 0) ;; namespace
      ((match nil "argument_list" nil 0 1) parent-bol c-ts-mode-indent-offset)
      ((parent-is "argument_list") c-ts-mode--first-sibling 0)
      ((match nil "parameter_list" nil 0 1) parent-bol c-ts-mode-indent-offset)
      ((parent-is "parameter_list") c-ts-mode--first-sibling 0)
      ,@(alist-get 'k&r (c-ts-mode--indent-styles 'cpp))
      ))
  (setq c-ts-mode-indent-style #'+my-indent-style)
  )

(use-package rust-ts-mode
  :straight nil
  :mode "\\.rs\\'"
  :config
  ;; do not cache the shitty result from rust-analyzer
  (advice-add #'lsp-eldoc-function :after (lambda (&rest _) (setq lsp--hover-saved-bounds nil)))
  ;; extract and show short signature for rust-analyzer
  (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
    (let* ((value (if lsp-use-plists (plist-get contents :value) (gethash "value" contents)))
           (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
           (mod-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-third-item groups))
                            ((s-equals? "```rust" (car (-third-item groups))) (-first-item groups))
                            (t nil)))
           (cmt (if (null mod-group) "" (concat " // " (cadr mod-group))))
           (sig-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-fifth-item groups))
                            ((s-equals? "```rust" (car (-third-item groups))) (-third-item groups))
                            (t (-first-item groups))))
           (sig (->> sig-group
                     (--drop-while (s-equals? "```rust" it))
                     (--take-while (not (s-equals? "```" it)))
                     (--map (s-replace-regexp "//.*" "" it))
                     (--map (s-trim it))
                     (s-join " "))))
      (lsp--render-element (concat "```rust\n" sig cmt "\n```"))))
  )

(use-package ccls
  :custom
  (ccls-args '("--log-file=/tmp/ccls.log"))
  :config
  (setq ccls-initialization-options
        `(:index (:trackDependency 1 :threads ,(min 32 (num-processors)))
                 :completion (:caseSensitivity 0 :filterAndSort t :maxNum 500)))
  (when IS-MAC
    (setq ccls-initialization-options
          (append ccls-initialization-options
                  `(:clang ,(list :extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                                              "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                                              "-isystem/usr/local/include"]
                                  :resourceDir (string-trim (shell-command-to-string "clang -print-resource-dir")))))))
  )

(use-package elisp-def)

(use-package magit
  :custom
  (transient-default-level 7) ;; for --autostash
  (transient-display-buffer-action '(display-buffer-below-selected))
  (magit-save-repository-buffers nil)
  (magit-revision-insert-related-refs nil)
  (magit-display-buffer-function '+magit-display-buffer-fn)
  ;; (magit-auto-revert-mode nil) ;; too slow for tramp
  :config
  (evil-define-key 'normal magit-mode-map
    "zt" #'evil-scroll-line-to-top
    "zz" #'evil-scroll-line-to-center
    "zb" #'evil-scroll-line-to-bottom)
  (evil-define-key 'normal magit-status-mode-map
    "gt" #'tab-bar-switch-to-next-tab)
  (evil-define-key 'normal magit-diff-mode-map
    "w" #'evil-forward-word-begin
    "b" #'evil-backward-word-begin)
  (define-key transient-map [escape] #'transient-quit-one)
  (add-hook 'git-commit-setup-hook #'evil-insert-state)
  )

(use-package diff-hl
  :hook (find-file . diff-hl-mode)
  :hook (diff-hl-mode . diff-hl-margin-mode)
  ;; :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :commands diff-hl-next-hunk diff-hl-previous-hunk
  :custom (vc-handled-backends '(Git))
  )

(use-package tramp
  :straight nil
  :custom
  (tramp-default-method "ssh")
  :config
  ;; do not move these to `custom'
  (setq tramp-verbose 1)
  (setq tramp-ssh-controlmaster-options "-o ControlPath=~/.ssh/master-%%h:%%p -o ControlMaster=auto -o ControlPersist=yes")
  ;; for magit to use newer git
  (add-to-list 'tramp-remote-path "~/.local/bin")
  )

(use-package projectile
  :config (projectile-mode)
  ;; only load projectile when it is needed
  :autoload projectile-project-p projectile-project-root projectile-acquire-root
  :custom
  (projectile-auto-discover nil)
  (projectile-globally-ignored-files '(".DS_Store" "TAGS"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
  (projectile-ignored-projects '("~/"))
  (projectile-enable-caching (not noninteractive))
  (projectile-kill-buffers-filter 'kill-only-files)
  (projectile-indexing-method 'alien)
  :preface
  ;; disable checking and cleaning especially for remote projects
  (advice-add #'projectile--cleanup-known-projects :override #'ignore)
  ;; disable serialize to `projectile-cache-file', may be large to load
  (advice-add #'projectile-serialize-cache :override #'ignore)
  :config
  (add-to-list 'projectile-globally-ignored-directories "^build$")
  (add-to-list 'projectile-globally-ignored-directories "^\\.ccls-cache$")
  (setq projectile-project-root-files '())
  (setq projectile-project-root-files-bottom-up '(".git" ".projectile"))
  (setq projectile-project-root-files-top-down-recurring '( "Makefile" "compile_commands.json"))
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name
        compilation-save-buffers-predicate #'projectile-current-project-buffer-p)
  )

(use-package deft
  :straight (:host github :repo "scturtle/deft")
  :custom
  (deft-extensions '("org"))
  (deft-directory "~/code/notes")
  :config
  (evil-set-initial-state 'deft-mode 'insert)
  )

(use-package neotree
  :hook (neotree-mode . hl-line-mode)
  :custom
  (neo-show-hidden-files t)
  (neo-window-width 30)
  (neo-vc-integration '(face))
  :config
  ;; use nerd-icons
  (advice-add 'neo-buffer--insert-fold-symbol :override #'+neotree/insert-symbol)
  (evil-define-key 'motion neotree-mode-map "w" #'+neotree/set-width)
  )

(use-package org
  :straight nil
  :hook (org-mode . evil-org-mode)
  ;; :hook (org-mode . corfu-mode) ;; terminal not compat with org-indent-mode
  :custom
  (org-modules nil)
  (org-startup-indented t) ;; org-indent-mode
  (org-startup-folded nil) ;; hide drawers
  (org-list-allow-alphabetical t)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-cycle-separator-lines 1) ;; empty lines to keep between collapsed trees
  (org-src-preserve-indentation t)
  (org-html-head-include-default-style nil) ;; org-html-style-default
  (org-return-follows-link t) ;; org-open-at-point (C-c C-o)
  ;; (org-hide-emphasis-markers t) ;; hide ==/++/**
  (org-src-lang-modes '(("C" . c-ts) ("c" . c-ts) ("C++" . c++-ts) ("c++" . c++-ts)
                        ("rust" . rust-ts) ("python" . python-ts)))
  :config
  (general-define-key
   :keymaps 'org-mode-map
   "C-c s" #'org-insert-structure-template
   )
  ;; open file links in current window, rather than split window
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  )

(use-package htmlize)

(use-package evil-org
  :custom
  (evil-org-retain-visual-state-on-shift t)
  ;; remove `return'/`insert' to fix ret/C-d, others are not useful
  (evil-org-key-theme '(navigation textobjects todo heading))
  )

(use-package lsp-pyright
  :custom
  ;; (lsp-pyright-use-library-code-for-types nil)
  (lsp-pyright-python-executable-cmd "python3")
  (lsp-pyright-typechecking-mode "off"))

;; setup llvm
(defvar +llvm-dir nil)
(cl-loop for dir in '("~/workspace/llvm-utils" "~/code/llvm-utils")
         when (file-directory-p dir)
         do (setq +llvm-dir dir))

(use-package tablegen-mode
  :if +llvm-dir
  :straight nil
  :load-path (lambda () (concat +llvm-dir "/llvm/utils/emacs"))
  :mode "\\.td\\'"
  :hook (tablegen-mode . lsp-deferred)
  :config
  (with-eval-after-load 'lsp-mode
    (let ((lsp-cmds '("tblgen-lsp-server" "--tablegen-compilation-database=tablegen_compile_commands.yml")))
      (add-to-list 'lsp-language-id-configuration '(tablegen-mode . "tablegen"))
      (lsp-register-client
       (make-lsp-client :new-connection (lsp-stdio-connection lsp-cmds)
                        :major-modes '(tablegen-mode)
                        :server-id 'tblgenls)))))

(use-package mlir-mode
  :if +llvm-dir
  :straight nil
  :load-path (lambda () (concat +llvm-dir "/mlir/utils/emacs"))
  :mode "\\.mlir\\'")

(use-package rainbow-mode)

(use-package web-mode
  :mode "\\.\\(html?\\|css\\|js\\)\\'"
  :custom
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  )

;; https://github.com/blahgeek/emacs-lsp-booster
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (funcall bytecode)))
   (apply old-fn args)))

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)
             (not (file-remote-p default-directory))
             lsp-use-plists)
        (cons "emacs-lsp-booster" orig-result)
      orig-result)))

(when (executable-find "emacs-lsp-booster")
  (advice-add 'json-parse-buffer :around #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

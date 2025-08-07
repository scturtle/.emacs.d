;;; -*- lexical-binding: t; -*-

;; disable UI components
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(setq mode-line-format nil)

;; disable GC during startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold (* 64 1024 1024)))) ; 64 mb

;; show startup time
(add-hook 'window-setup-hook
          (lambda () (message "startup in %.2f ms"
                              (* 1000.0 (float-time (time-since before-init-time))))))

;; remove searching for .gz files (from geza-herman)
(setq jka-compr-load-suffixes nil)
(jka-compr-update)

;; say no to `package.el'
(setq package-enable-at-startup nil)

;; disable `file-name-handler-alist' during startup
(defvar file-name-handler-alist-bak file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'after-init-hook
          (lambda () (setq file-name-handler-alist file-name-handler-alist-bak)))

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

;; declare built-in packages
(setq straight-built-in-pseudo-packages
      (append straight-built-in-pseudo-packages
              '(compat dabbrev eglot eldoc flymake jsonrpc org project seq tramp
                       treesit use-package which-key xref c-ts-mode)))

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
(setq custom-theme-directory (emacs.d "lisp"))
;; (load-theme 'aura t)
;; (load-theme 'catppuccin-mocha t)
(load-theme 'catppuccin-latte t)
(add-to-list 'load-path (emacs.d "lisp"))
(require 'funcs)
(require 'mudline)
(mudline-mode)
(require 'linkding)

;; defaults
(use-package emacs
  :init
  (setq user-full-name "scturtle"
        user-mail-address "hi@scturtle.me")

  ;; startup
  (setq inhibit-splash-screen t)
  (setq initial-scratch-message nil)
  (setq initial-major-mode 'fundamental-mode) ; for *scratch*
  ;; remove "For information about GNU Emacs..."
  (advice-add 'display-startup-echo-area-message :override #'ignore)

  ;; general
  (setq use-short-answers t)
  (delete-selection-mode +1)
  (setq apropos-do-all t)
  (setq shell-file-name "/bin/bash") ;; override fish
  (setq xterm-extra-capabilities '(setSelection)) ;; OSC 52
  (add-hook 'tty-setup-hook #'xterm-mouse-mode) ;; mouse in terminal
  (when IS-MAC (setq process-adaptive-read-buffering nil)) ;; eshell

  ;; perf (from doom-start)
  (setq auto-mode-case-fold nil)
  (setq redisplay-skip-fontification-on-input t)
  (setq-default cursor-in-non-selected-windows nil)
  (setq highlight-nonselected-windows nil)
  (setq fast-but-imprecise-scrolling t)
  (setq read-process-output-max (* 1024 1024))

  ;; custom
  (setq custom-file (emacs.d "custom.el"))
  (when (file-exists-p custom-file)
    (load-file custom-file))

  ;; encoding
  (set-language-environment "UTF-8")
  (setq default-input-method nil)
  (setq bidi-inhibit-bpa t)
  (setq-default bidi-display-reordering  'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; from doom-ui
  (setq hscroll-margin 2
        hscroll-step 1
        scroll-conservatively 101
        scroll-margin 0
        scroll-preserve-screen-position t
        auto-window-vscroll nil)
  (blink-cursor-mode -1)
  (setq blink-matching-paren nil)

  ;; from doom-editor
  (setq vc-follow-symlinks t)
  (setq create-lockfiles nil
        make-backup-files nil)
  (setq auto-save-default nil)
  (setq-default indent-tabs-mode nil
                tab-width 4)
  (setq-default tab-always-indent nil)
  (setq-default fill-column 80)
  (setq sentence-end-double-space nil)
  (setq-default require-final-newline nil)

  ;; history
  (setq recentf-auto-cleanup nil
        recentf-max-saved-items 200)
  (add-hook 'kill-emacs-hook #'recentf-cleanup)
  (recentf-mode)
  (savehist-mode)
  (save-place-mode)

  ;; paren
  (setq blink-paren-function nil)
  (setq show-paren-delay 0.0)
  (show-paren-mode 1)
  (add-hook 'prog-mode-hook 'electric-pair-mode)

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

  ;; show line number
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  ;; do not show line number in modeline
  (setq line-number-mode nil)

  ;; diff-mode
  (setq diff-refine nil)  ;; no hunk refinement

  ;; highlight TODO/FIXME/NOTE
  (add-hook 'prog-mode-hook 'highlight-codetags-watchwords)
  )

(use-package gcmh
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto) ; default is 15s
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 64 1024 1024)) ; 64 mb
  )

(use-package evil
  :demand
  :init
  (setq evil-disable-insert-state-bindings t)
  :custom
  (evil-want-keybinding nil) ; for evil-collection
  (evil-want-C-g-bindings t)
  (evil-want-C-i-jump nil) ;; unbind tab for neotree/org-mode
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-symbol-word-search t)
  (evil-want-fine-undo t)
  (evil-undo-system 'undo-fu)
  (evil-ex-interactive-search-highlight 'selected-window) ;; all-windows
  (evil-visual-update-x-selection-p nil)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode)

  (evil-set-leader '(normal visual) (kbd "SPC"))

  (evil-define-key '(normal visual) 'global
    ;; top-level
    (kbd "<leader>SPC") #'execute-extended-command
    (kbd "<leader>TAB") #'evil-switch-to-windows-last-buffer
    (kbd "<leader>u")  #'universal-argument
    (kbd "<leader>;")  #'pp-eval-expression
    (kbd "<leader>:")  #'pp-eval-expression
    (kbd "<leader>,")  #'switch-to-buffer
    (kbd "<leader>i")  #'consult-imenu
    (kbd "<leader>*")  #'+search-project-for-symbol-at-point
    (kbd "<leader>qq") #'save-buffers-kill-terminal

    ;; buffer
    (kbd "<leader>bb") #'switch-to-buffer
    (kbd "<leader>br") #'revert-buffer
    (kbd "<leader>bd") #'kill-current-buffer
    (kbd "<leader>bi") #'ibuffer
    (kbd "<leader>bm") #'(lambda () (interactive) (switch-to-buffer "*Messages*"))
    (kbd "<leader>bk") #'kill-current-buffer
    (kbd "<leader>bf") #'eglot-format-buffer

    ;; code
    (kbd "<leader>ca") #'eglot-code-actions
    (kbd "<leader>cr") #'eglot-rename
    (kbd "<leader>cw") #'delete-trailing-whitespace

    ;; error
    (kbd "<leader>el") #'flycheck-list-errors
    (kbd "<leader>ep") #'flycheck-previous-error
    (kbd "<leader>en") #'flycheck-next-error

    ;; file
    (kbd "<leader>ff") #'find-file
    (kbd "<leader>fs") #'save-buffer
    (kbd "<leader>fr") #'recentf-open-files
    (kbd "<leader>fp") #'(lambda () (interactive) (find-file user-init-file))

    ;; git
    (kbd "<leader>gg") #'magit-status
    (kbd "<leader>gb") #'magit-blame-addition
    (kbd "<leader>gp") #'diff-hl-previous-hunk
    (kbd "<leader>gn") #'diff-hl-next-hunk
    (kbd "<leader>cp") #'smerge-prev
    (kbd "<leader>cn") #'smerge-next

    ;; help
    (kbd "<leader>hf") #'describe-function
    (kbd "<leader>hF") #'describe-face
    (kbd "<leader>hm") #'describe-mode
    (kbd "<leader>hk") #'describe-key
    (kbd "<leader>hp") #'describe-package
    (kbd "<leader>hv") #'describe-variable
    (kbd "<leader>hh") #'hi-lock-symbol-at-point

    ;; note
    (kbd "<leader>nd") #'deft

    ;; project
    (kbd "<leader>pp") #'projectile-switch-project
    (kbd "<leader>pr") #'projectile-recentf
    (kbd "<leader>pi") #'projectile-invalidate-cache
    (kbd "<leader>pf") #'projectile-find-file
    (kbd "<leader>pd") #'consult-fd
    (kbd "<leader>ga") #'projectile-find-other-file
    (kbd "<leader>op") #'neotree-toggle
    (kbd "<leader>oP") #'+neotree/find-this-file

    ;; search
    (kbd "<leader>sh") #'+symbol-highlight
    (kbd "<leader>sb") #'consult-line
    (kbd "<leader>sp") #'consult-ripgrep

    ;; toggle
    (kbd "<leader>tl") #'display-line-numbers-mode
    (kbd "<leader>tt") #'toggle-truncate-lines
    (kbd "<leader>tn") #'tab-bar-new-tab
    (kbd "<leader>tm") #'tab-bar-move-tab
    (kbd "<leader>1") #'(lambda() (interactive) (tab-bar-select-tab 1))
    (kbd "<leader>2") #'(lambda() (interactive) (tab-bar-select-tab 2))
    (kbd "<leader>3") #'(lambda() (interactive) (tab-bar-select-tab 3))
    (kbd "<leader>4") #'(lambda() (interactive) (tab-bar-select-tab 4))
    (kbd "<leader>5") #'(lambda() (interactive) (tab-bar-select-tab 5))
    (kbd "<leader>6") #'(lambda() (interactive) (tab-bar-select-tab 6))
    (kbd "<leader>7") #'(lambda() (interactive) (tab-bar-select-tab 7))
    (kbd "<leader>8") #'(lambda() (interactive) (tab-bar-select-tab 8))
    (kbd "<leader>9") #'(lambda() (interactive) (tab-bar-select-tab 9))

    ;; window
    (kbd "<leader>wd") #'evil-window-delete
    (kbd "<leader>wo") #'delete-other-windows
    (kbd "<leader>wk") #'evil-window-up
    (kbd "<leader>wj") #'evil-window-down
    (kbd "<leader>wl") #'evil-window-right
    (kbd "<leader>wh") #'evil-window-left
    (kbd "<leader>ww") #'evil-window-next
    )

  ;; override "gd" to `evil-goto-definition'
  (evil-define-key 'normal 'global
    "gd" #'+goto-definition
    "gD" #'lsp-ui-peek-find-references
    "gb" #'xref-go-back
    "gf" #'xref-go-forward)

  ;; mimic emacs key bindings
  (evil-define-key 'insert 'global
    (kbd "C-h") 'delete-backward-char
    (kbd "C-g") #'evil-normal-state)

  (with-eval-after-load 'minibuffer
    (let ((map minibuffer-mode-map))
      (define-key map (kbd "<escape>") 'abort-recursive-edit)
      (define-key map (kbd "C-h")      'delete-backward-char)
      (define-key map (kbd "C-u")      #'evil-delete-back-to-indentation)))

  ;; oevrride evil ex mode (in ":" "/")
  (with-eval-after-load 'evil-ex
    (dolist (map (list evil-ex-completion-map evil-ex-search-keymap))
      (define-key map (kbd "C-a") 'move-beginning-of-line)
      (define-key map (kbd "C-f") 'forward-char)
      (define-key map (kbd "C-b") 'backward-char)
      (define-key map (kbd "C-d") 'delete-forward-char)
      (define-key map (kbd "C-h") 'delete-backward-char)
      (define-key map (kbd "C-u") 'evil-delete-back-to-indentation)
      (define-key map (kbd "C-k") 'kill-line)))

  ;; don't leave visual mode after shifting
  (evil-define-key 'visual 'global
    (kbd "<") #'+evil/shift-left
    (kbd ">") #'+evil/shift-right)

  ;; make SPC u SPC u [...] possible
  (define-key universal-argument-map " u" #'universal-argument-more)

  ;; rime
  (global-unset-key (kbd "C-@"))
  (global-unset-key (kbd "C-SPC"))

  ;; magit
  (with-eval-after-load 'magit
    (define-key magit-status-mode-map (kbd "SPC") nil))
  )

(use-package evil-collection
  :demand
  :custom
  ;; (evil-collection-mode-list '(custom consult dired ediff flycheck ibuffer info magit minibuffer neotree))
  (evil-collection-magit-want-horizontal-movement t)
  (evil-collection-magit-use-z-for-folds t)
  :config
  (delete 'eglot evil-collection-mode-list)
  (evil-collection-init)
  ;; disable bindings that I don't want
  (advice-add #'evil-collection-neotree-setup :after
              (lambda (&rest _) (evil-collection-define-key 'normal 'neotree-mode-map "z" nil)))
  (advice-add #'evil-collection-view-setup :after
              (lambda (&rest _) (evil-collection-define-key 'normal 'view-mode-map "0" nil)))
  )

;; display cursor as bar instread of box in terminal
(use-package evil-terminal-cursor-changer
  :demand
  :config (evil-terminal-cursor-changer-activate)
  :custom
  (evil-insert-state-cursor 'bar)
  (evil-visual-state-cursor 'hollow))

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
  :init (evil-define-key 'visual 'global "S" #'evil-surround-region)
  :config
  ;; use non-spaced pairs when surrounding with an opening brace
  (setf (alist-get ?\( evil-surround-pairs-alist) '("(" . ")"))
  (setf (alist-get ?\[ evil-surround-pairs-alist) '("[" . "]"))
  (setf (alist-get ?\{ evil-surround-pairs-alist) '("{" . "}")))

;; use s/S for 2-character motions
(use-package evil-snipe
  :hook (after-init . evil-snipe-mode)
  :hook (after-init . evil-snipe-override-mode)
  )

(use-package undo-fu
  :straight (:host github :repo "emacsmirror/undo-fu") ;; codeberg
  :demand
  :config
  (setq undo-limit         400000   ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)
  )

(use-package undo-fu-session
  :straight (:host github :repo "emacsmirror/undo-fu-session") ;; codeberg
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
  :config
  (which-key-add-key-based-replacements
    "<SPC> b" "buffer"
    "<SPC> c" "code"
    "<SPC> e" "error"
    "<SPC> f" "file"
    "<SPC> g" "git"
    "<SPC> h" "help"
    "<SPC> n" "note"
    "<SPC> p" "project"
    "<SPC> s" "search"
    "<SPC> t" "toggle"
    "<SPC> w" "window"
    ))

;; use tty-child-frames
(use-package posframe)

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :hook (after-init . vertico-mode)
  :hook (minibuffer-setup . vertico-repeat-save)
  :custom
  (vertico-resize nil)
  (vertico-count 17)
  (vertico-cycle t)
  (vertico-scroll-margin 0)
  :bind
  (:map vertico-map
        ("M-."       . #'vertico-repeat)
        ("M-p"       . #'vertico-scroll-down)
        ("M-n"       . #'vertico-scroll-up)
        ("<tab>"     . #'vertico-next-group)
        ("<backtab>" . #'vertico-previous-group)
        )
  )

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
  (xref-show-xrefs-function #'consult-xref) ;; for xref-find-references
  )

(use-package iedit :commands iedit-start)

(use-package evil-multiedit
  :commands evil-multiedit-mode
  :custom
  (evil-multiedit-follow-matches t))

(use-package treesit
  :demand t
  :custom
  (treesit-font-lock-level 4))

(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-langs '(c cpp python rust cmake yaml))
  :config
  (treesit-auto-add-to-auto-mode-alist treesit-auto-langs)
  ;; not listed in `treesit-auto-recipe-list'
  (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-ts-mode))
  )

(use-package treesit-fold
  :straight (:host github :repo "emacs-tree-sitter/treesit-fold")
  :hook ((c-ts-mode c++-ts-mode python-ts-mode rust-ts-mode) . treesit-fold-mode))

(use-package yasnippet
  :hook (eglot-managed-mode . yas-minor-mode))

(use-package eldoc
  :custom
  (eldoc-idle-delay 0.0)
  ;; when `eldoc-doc-buffer' is opened, do not show in echo area
  (eldoc-echo-area-prefer-doc-buffer t)
  )

(use-package markdown-mode)

(use-package elisp-def)

(use-package eglot
  :hook ((c-ts-mode c++-ts-mode python-ts-mode rust-ts-mode) . eglot-ensure)
  :custom
  (eglot-sync-connect nil) ;; blocking
  (eglot-autoshutdown t)
  (eglot-autoreconnect nil)
  (eglot-events-buffer-config (list :size 0 :format 'full)) ;; set size to nil for debug
  (eglot-menu-string "e")
  (eglot-mode-line-format '(eglot-mode-line-session eglot-mode-line-progress))
  ;; (eglot-send-changes-idle-time 0.0)
  (eglot-ignored-server-capabilities '(:inlayHintProvider :signatureHelpProvider))
  :custom-face
  (eglot-highlight-symbol-face ((t :inherit highlight)))
  (eglot-mode-line ((t :inherit font-lock-string-face :bold nil)))
  :config
  ;; ccls
  (setq ccls-initialization-options
        `(:index (:trackDependency 1 :threads ,(min 32 (num-processors)) :comments 0)
                 :completion (:caseSensitivity 0 :filterAndSort t :maxNum 500)))
  (when IS-MAC
    (setq ccls-initialization-options
          (append ccls-initialization-options
                  `(:clang (:extraArgs
                            ["-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/c++/v1"
                             "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                             "-isystem/usr/local/include"
                             "-isystem/opt/homebrew/include"]
                            :resourceDir ,(string-trim (shell-command-to-string "clang -print-resource-dir")))))))
  (add-to-list 'eglot-server-programs
               `((c-ts-mode c++-ts-mode) . ("ccls" "--log-file=/tmp/ccls.log"
                                            :initializationOptions ,ccls-initialization-options)))
  ;; prefer directory with compile_commands.json for project root
  (advice-add 'eglot--current-project :around
              (lambda (orig &rest args)
                (if (memq major-mode '(c-ts-mode c++-ts-mode))
                    (if-let* ((json (locate-dominating-file default-directory "compile_commands.json")))
                        `(transient . ,(expand-file-name json)) (apply orig args))
                  (apply orig args))))
  ;; FIXME: hotfix pyright
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("basedpyright-langserver" "--stdio")))
  (advice-add 'file-notify-add-watch :override #'ignore)
  (advice-add 'file-notify-rm-watch :override #'ignore)
  ;; fix progress report
  (advice-add 'eglot--mode-line-props :filter-args
              (lambda (args) (cons (replace-regexp-in-string "%%" "٪" (car args)) (cdr args))))
  ;; use orderless
  (setq completion-category-defaults nil)
  ;; setup the orderless-flex style for its 1st search term
  (defun +orderless-flex-first (_pat idx _tot) (if (eq idx 0) 'orderless-flex 'orderless-literal))
  (add-hook 'orderless-style-dispatchers #'+orderless-flex-first nil 'local)
  ;; extract and show short signature for rust-analyzer
  (with-eval-after-load 'rust-ts-mode
    (require 's)
    (require 'dash)
    (advice-add #'eglot--hover-info :override #'+eglot-rust-hover-info))
  )

(use-package lsp-ui-peek
  :straight (:host github :repo "scturtle/lsp-ui-peek")
  :commands lsp-ui-peek-find-definitions lsp-ui-peek-find-references
  :bind
  (:map lsp-ui-peek-mode-map
        ("k" . #'lsp-ui-peek--select-prev)
        ("j" . #'lsp-ui-peek--select-next))
  )

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-indication-mode 'left-margin)
  ;; (flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-display-errors-delay '0.2)
  ;; less warnings (for editing config like this init.el)
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  )

(use-package flycheck-eglot
  :hook (eglot-managed-mode . flycheck-eglot-mode))

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
  ;; use tty-child-frames instead of corfu-terminal
  (setf (alist-get 'internal-border-width corfu--frame-parameters) 0)
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (corfu--popup-hide) ;; NOTE: hide the terminal popup
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  ;; instead of using nerd-icons, why not just a simple space margin
  ;; (defun +simple-margin-formatter (_) (lambda (_) ""))
  ;; (add-to-list 'corfu-margin-formatters #'+simple-margin-formatter)
  :bind
  (:map corfu-map
        ("M-SPC" . 'corfu-insert-separator)
        ("M-m"   . 'corfu-move-to-minibuffer)))

(use-package dabbrev
  :custom
  (dabbrev-case-replace nil) ;; do not downcase
  (dabbrev-check-other-buffers nil) ;; only in this buffer
  :bind (("M-/" . dabbrev-completion)))

(use-package c-ts-mode
  :config
  (defun +my-indent-style()
    (if (version< emacs-version "31")
        `(;; ((parent-is "declaration_list") parent-bol 0) ;; namespace
          ((match nil "argument_list" nil 0 1) parent-bol c-ts-mode-indent-offset)
          ((parent-is "argument_list") c-ts-mode--first-sibling 0)
          ((match nil "parameter_list" nil 0 1) parent-bol c-ts-mode-indent-offset)
          ((parent-is "parameter_list") c-ts-mode--first-sibling 0)
          ,@(alist-get 'k&r (c-ts-mode--indent-styles 'cpp)))
      `((cpp
         ;; rules copied from 42yeah
         ((n-p-gp nil nil "namespace_definition") grand-parent 0)
         ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
         ((parent-is "argument_list") (nth-sibling 1) 0)
         ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
         ((parent-is "parameter_list") (nth-sibling 1) 0)
         ,@(alist-get 'cpp (c-ts-mode--simple-indent-rules 'cpp 'k&r))))
      ))
  (setq c-ts-mode-indent-style #'+my-indent-style)
  )

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
  (deft-directory "~/code/notes")
  :config
  (evil-set-initial-state 'deft-mode 'insert)
  (advice-add #'deft-mode :after #'hl-line-mode)
  )

(use-package neotree
  :hook (neotree-mode . hl-line-mode)
  :custom
  (neo-show-hidden-files t)
  (neo-window-width 30)
  (neo-vc-integration '(face))
  (neo-theme 'nerd-icons)
  :config
  (evil-define-key 'motion neotree-mode-map "w" #'+neotree/set-width)
  )

(use-package org
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
                        ("rust" . rust-ts) ("python" . python-ts) ("cmake" . cmake-ts)))
  (org-fontify-quote-and-verse-blocks t)
  ;; for org-agenda
  (org-agenda-files (list "~/gtd.org"))
  (org-log-done 'time)
  (org-agenda-window-setup 'current-window)
  (org-agenda-span 'day)
  (org-agenda-compact-blocks t)
  :bind
  (:map org-mode-map
        ("C-c s" . #'org-insert-structure-template)
        ("C-c a" . #'org-agenda)
        :map org-agenda-mode-map
        ("j" . #'org-agenda-next-line)
        ("k" . #'org-agenda-previous-line))
  :config
  ;; open file links in current window, rather than split window
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  )

(use-package htmlize)

;; setup llvm
(defvar +llvm-dir (emacs.d "lisp/llvm-utils"))
(unless (file-directory-p +llvm-dir)
  (setq +llvm-dir nil))

(use-package tablegen-mode
  :if +llvm-dir
  :straight nil
  :load-path +llvm-dir
  :mode "\\.td\\'"
  :hook (tablegen-mode . eglot-ensure)
  :hook (tablegen-mode . prog-mode)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(tablegen-mode . ("tblgen-lsp-server" "--tablegen-compilation-database=tablegen_compile_commands.yml")))))

(use-package mlir-mode
  :if +llvm-dir
  :straight nil
  :load-path +llvm-dir
  :mode "\\.mlir\\'")

(use-package rainbow-mode
  :custom
  (rainbow-x-colors nil)
  )

(use-package web-mode
  :mode "\\.\\(html?\\|css\\|js\\)\\'"
  :custom
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  )

(use-package plz)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; -*- lexical-binding: t; -*-

;; GC is disabled during startup, reset it back
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024)
                           gc-cons-percentage 0.1)))

;; show startup time
(add-hook 'emacs-startup-hook
          (lambda () (message "startup in %.2f ms"
                              (* 1000.0 (float-time (time-subtract after-init-time before-init-time))))))

;; disable `file-name-handler-alist' during startup
(defvar doom--initial-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun doom-reset-file-handler-alist-h ()
  (dolist (handler file-name-handler-alist)
    (add-to-list 'doom--initial-file-name-handler-alist handler))
  (setq file-name-handler-alist doom--initial-file-name-handler-alist))
(add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h)

;; handy definitions
(defun emacsd (path) (expand-file-name path user-emacs-directory))
(defconst IS-MAC (eq system-type 'darwin))

;; straight
(setq straight-vc-git-default-clone-depth 1)
(setq straight-check-for-modifications nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
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

;; (add-to-list 'load-path (emacsd "lisp"))
(load-file (emacsd "funcs.el"))

;; defaults
(use-package emacs
  :init
  (setq user-full-name "scturtle"
        user-mail-address "hi@scturtle.me")

  ;; hrs/sensible-defaults.el
  (setq inhibit-splash-screen t)
  (setq initial-scratch-message nil)
  (setq use-short-answers t) ;; y-or-n-p for >= 28
  (delete-selection-mode +1)
  (setq vc-follow-symlinks t)
  (setq require-final-newline t)
  (show-paren-mode t)
  (setq show-paren-delay 0.0)

  ;; enable mouse in terminal
  (add-hook 'tty-setup-hook #'xterm-mouse-mode)

  ;; from doom-start
  (set-language-environment "UTF-8")
  (setq default-input-method nil)

  ;; for lsp
  (setq read-process-output-max (* 1024 1024))

  ;; custom
  (setq custom-file (emacsd "custom.el"))
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
        auto-save-list-file-prefix (emacsd "cache/autosave/")
        auto-save-default nil)

  ;; TODO: smartparens?
  (add-hook 'prog-mode-hook 'electric-pair-mode)

  ;; TODO: treesitter
  ;; (setq treesit-extra-load-path '("~/code/repos/tree-sitter-module/dist"))

  ;; tab bar
  (setq tab-bar-separator ""
        tab-bar-close-button-show nil
        tab-bar-format '(tab-bar-format-tabs)
        tab-bar-tab-name-function 'tab-bar-tab-name-truncated
        tab-bar-tab-name-ellipsis "…")

  ;; display-time-mode
  (setq display-time-format "%a %H:%M"
        display-time-default-load-average nil)
  )

;; packages
(use-package gcmh
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto) ; default is 15s
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  )

(use-package general
  :demand
  :config
  (general-evil-setup)

  (general-create-definer define-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer define-localleader-keys
    :states '(normal visual)
    :keymaps 'override
    :prefix ","
    :global-prefix "SPC m")

  (define-leader-keys
    "SPC" '(execute-extended-command :wk "execute command")
    [?\t] '(evil-switch-to-windows-last-buffer :wk "prev buffer")
    ";" '(pp-eval-expression :wk "eval expr")
    ":" '(pp-eval-expression :wk "eval expr")
    "," 'switch-to-buffer
    "/" #'projectile-find-file

    "b" '(:ignore t :wk "buffer")
    "bb" 'switch-to-buffer
    "br" 'revert-buffer
    "bd" 'kill-current-buffer
    "bi" 'ibuffer
    "bm" '((lambda () (interactive) (switch-to-buffer "*Messages*")) :wk "message buffer")
    "bk" 'kill-current-buffer

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
    "fp" '((lambda () (interactive) (find-file (emacsd "init.el"))) :wk "edit init.el")

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
    "pf" #'projectile-find-file
    "ga" #'projectile-find-other-file
    "op" #'neotree-toggle
    "oP" #'+neotree/find-this-file
    "p/" #'consult-ripgrep

    "qq" 'save-buffers-kill-terminal

    "s" '(:ignore t :wk "search")
    "sh" #'my/symbol-highlight
    "sp" #'consult-ripgrep

    "t"  '(:ignore t :wk "toggle")
    "tl" '(display-line-numbers-mode :wk "line numbers")
    "tn" #'tab-bar-new-tab
    "1"  '((lambda() (interactive) (tab-bar-select-tab 1)) :wk "switch tab 1")
    "2"  '((lambda() (interactive) (tab-bar-select-tab 2)) :wk "switch tab 2")
    "3"  '((lambda() (interactive) (tab-bar-select-tab 3)) :wk "switch tab 3")
    "4"  '((lambda() (interactive) (tab-bar-select-tab 4)) :wk "switch tab 4")
    "5"  '((lambda() (interactive) (tab-bar-select-tab 5)) :wk "switch tab 5")

    "u" '(universal-argument :wk "universal")

    "w" '(:ignore t :wk "window")
    "wd" #'evil-window-delete
    "wo" 'delete-other-windows
    "wk" #'evil-window-up
    "wj" #'evil-window-down
    "wl" #'evil-window-right
    "wh" #'evil-window-left
    "ww" #'evil-window-next
    )
  )

;; better `describe-*' functions
(use-package helpful
  :hook (helpful-mode . visual-line-mode)
  :custom
  (apropos-do-all t) ;; ???
  :general
  ([remap describe-function] 'helpful-callable
   [remap describe-command]  'helpful-command
   [remap describe-variable] 'helpful-variable
   [remap describe-key]      'helpful-key
   [remap describe-symbol]   'helpful-symbol))

;; OSC52 FTW
(use-package clipetty
  :hook (after-init . global-clipetty-mode))

;; display cursor as bar instread of box in terminal
(use-package evil-terminal-cursor-changer
  :hook (after-init . evil-terminal-cursor-changer-activate)
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

(use-package evil
  :hook (after-init . evil-mode)
  :custom
  (evil-want-keybinding nil) ; for evil-collection
  (evil-want-C-g-bindings t)
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
  :general
  ;; mimic emacs key bindings
  (:states 'insert
           "C-a" 'move-beginning-of-line
           "C-e" 'move-end-of-line
           "C-p" 'previous-line
           "C-n" 'next-line
           "C-d" 'delete-forward-char
           "C-h" 'delete-backward-char
           "C-k" 'kill-line
           "C-g" 'evil-normal-state)
  (:keymaps 'minibuffer-mode-map
            "<escape>" 'abort-recursive-edit
            "C-h"      'delete-backward-char
            "C-u"      'evil-delete-back-to-indentation)
  (:keymaps 'evil-ex-completion-map
            "M-p" 'previous-complete-history-element
            "M-n" 'next-complete-history-element)
  :config
  ;; override "gd" to `evil-goto-definition'
  (general-define-key
   :states '(normal motion)
   "gd" #'my/goto-definition
   "gD" #'lsp-ui-peek-find-references)
  ;; unbind tab for neotree
  (general-define-key :states '(normal motion) [?\t] nil)
  )

(use-package evil-collection
  :demand
  :after evil
  :custom
  ;; (evil-collection-mode-list '())
  (evil-collection-magit-want-horizontal-movement t)
  (evil-collection-magit-use-z-for-folds t)
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :general
  ("M-;" 'comment-line
   [remap comment-line] #'evilnc-comment-or-uncomment-lines))

(use-package evil-surround
  :general (:states 'visual "S" 'evil-surround-region))

(use-package undo-fu
  :config
  (setq undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)
  )

(use-package undo-fu-session
  :hook (after-init . undo-fu-session-global-mode)
  :custom (undo-fu-session-directory (emacsd "cache/undo-fu-session"))
  :config (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package saveplace
  :hook (after-init . save-place-mode)
  :custom (save-place-file (emacsd "cache/saveplace"))
  ;; :config (add-hook 'save-place-after-find-file-hook (lambda (&rest _) (if buffer-file-name (ignore-errors (recenter)))))
  )

(use-package savehist
  :hook (after-init . savehist-mode)
  :custom (savehist-file (emacsd "cache/savehist")))

(use-package recentf
  :hook (after-init . recentf-mode)
  :custom
  (recentf-save-file (emacsd "cache/recentf"))
  (recentf-auto-cleanup nil)
  (recentf-max-saved-items 200)
  :config
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

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

(use-package doom-modeline
  :demand
  :custom
  (doom-modeline-lsp t)
  (doom-modeline-icon nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-project-detection 'projectile)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  :config
  ;; (display-time-mode)
  (doom-modeline-mode 1))

(use-package doom-themes
  :demand
  :custom
  (custom-theme-directory (emacsd ""))
  (doom-themes-enable-bold t)
  ;; (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-aura t)
  ;; (doom-themes-visual-bell-config)
  ;; (doom-themes-neotree-config)
  (doom-themes-org-config)
  )

(use-package vertico
  :straight
  (:host github :repo "minad/vertico" :files (:defaults "extensions/*"))
  :hook (after-init . vertico-mode)
  :custom
  (vertico-resize nil)
  (vertico-count 17)
  (vertico-cycle t))

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :general
  (:keymaps 'minibuffer-local-map
            "M-A" #'marginalia-cycle)
  :config
  (setq marginalia-command-categories
        (append '((projectile-find-file . project-file)
                  (projectile-recentf . project-file)
                  (projectile-find-dir . file)
                  (projectile-switch-to-buffer . buffer)
                  (projectile-switch-project . project-file))
                marginalia-command-categories)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles orderless partial-completion))))
  (orderless-component-separator "[ &]"))

(use-package consult
  :demand
  :general
  ([remap apropos]                       #'consult-apropos
   [remap bookmark-jump]                 #'consult-bookmark
   [remap evil-show-marks]               #'consult-mark
   [remap evil-show-jumps]               #'+vertico/jump-list
   [remap evil-show-registers]           #'consult-register
   [remap goto-line]                     #'consult-goto-line
   [remap imenu]                         #'consult-imenu
   [remap locate]                        #'consult-locate
   [remap load-theme]                    #'consult-theme
   [remap man]                           #'consult-man
   [remap recentf-open-files]            #'consult-recent-file
   [remap switch-to-buffer]              #'consult-buffer
   [remap switch-to-buffer-other-window] #'consult-buffer-other-window
   [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
   [remap yank-pop]                      #'consult-yank-pop
   [remap persp-switch-to-buffer]        #'+vertico/switch-workspace-buffer)
  :config
  ;; to use consult for completion in `Eval:'
  (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1))

(use-package iedit :commands iedit-start)

(use-package evil-multiedit
  :commands evil-multiedit-mode
  :custom
  (evil-multiedit-follow-matches t))

(use-package lsp-mode
  :hook ((c-mode c++-mode) . lsp-deferred)
  :custom
  (lsp-idle-delay 0)
  (eldoc-idle-delay 0)
  (lsp-lens-enable nil)
  (lsp-enable-snippet nil)
  (lsp-enable-file-watchers nil)
  (lsp-signature-render-documentation nil)
  (lsp-client-packages '(ccls lsp-rust))
  (lsp-headerline-breadcrumb-enable nil)
  :custom-face
  ;; remove underline in hover
  (lsp-face-highlight-read ((t :underline nil)))
  )

(with-eval-after-load 'lsp-mode
  ;; override `lsp-tramp-connection'
  (advice-add 'lsp-tramp-connection :override #'lsp-tramp-connection@override)
  ;; register remote ccls client
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "ccls")
                    :activation-fn (lsp-activate-on "c" "cpp")
                    :remote? t
                    :multi-root nil
                    :server-id 'ccls-remote))
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil)
  :custom-face
  (lsp-ui-peek-highlight ((t :forground unspecified :background unspecified :inherit highlight)))
  )

(use-package company
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-tooltip-limit 14)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  (company-backends '(company-capf))
  (company-dabbrev-other-buffers nil)
  (company-dabbrev-code-other-buffers nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  :general
  (:keymaps 'company-active-map
            "C-i" #'company-show-doc-buffer
            "C-h" nil)
  )

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-indication-mode 'left-margin)
  (flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-emacs-lisp-load-path 'inherit)
  ;; (flycheck-idle-change-delay 1.0)
  (flycheck-display-errors-delay '0.2)
  :config
  ;; less warnings (for editing config like this init.el)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-emacs-lisp-check-form
        (prin1-to-string
         `(progn
            (setq byte-compile-warnings '(not free-vars noruntime unresolved))
            ,(read (default-toplevel-value 'flycheck-emacs-lisp-check-form)))))
  )

(use-package flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode)
  :config
  (setq flycheck-popup-tip-error-prefix "⚠ ")
  ;; don't display popups while in insert mode
  (add-hook 'evil-insert-state-entry-hook #'flycheck-popup-tip-delete-popup)
  (advice-add #'flycheck-popup-tip-show-popup :before-while (lambda (&rest _) (eq evil-state 'normal))))

(use-package rust-mode
  :custom
  (rust-match-angle-brackets nil)
  :config
  ;; do not cache the shitty result from rust-analyzer
  (advice-add #'lsp-eldoc-function :after (lambda (&rest _) (setq lsp--hover-saved-bounds nil)))
  ;; extract and show short signature for rust-analyzer
  (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
    (let* ((value (if lsp-use-plists (plist-get contents :value) (gethash "value" contents)))
           (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
           (sig_group (if (s-equals? "```rust" (car (-third-item groups)))
                          (-third-item groups)
                        (car groups)))
           (sig (--> sig_group
                     (--drop-while (s-equals? "```rust" it) it)
                     (--take-while (not (s-equals? "```" it)) it)
                     (--map (s-trim it) it)
                     (s-join " " it))))
      (lsp--render-element (concat "```rust\n" sig "\n```"))))
  )

(use-package rustic
  )

(use-package ccls
  :custom
  (ccls-args '("--log-file=/tmp/ccls.log"))
  :config
  (setq ccls-initialization-options
        '(:index (:trackDependency 1 :threads 8)))
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
  (transient-levels-file  (emacsd "cache/transient/levels"))
  (transient-values-file  (emacsd "cache/transient/values"))
  (transient-history-file (emacsd "cache/transient/history"))
  (transient-default-level 5)
  (transient-display-buffer-action '(display-buffer-below-selected))
  (magit-save-repository-buffers nil)
  (magit-revision-insert-related-refs nil)
  (magit-display-buffer-function '+magit-display-buffer-fn)
  :config
  (general-define-key
   :keymaps 'magit-mode-map :states 'normal
   "zt" #'evil-scroll-line-to-top
   "zz" #'evil-scroll-line-to-center
   "zb" #'evil-scroll-line-to-bottom)
  (general-define-key
   :keymaps 'magit-status-mode-map :states 'normal
   "gt" #'tab-bar-switch-to-next-tab)
  (general-define-key
   :keymaps 'magit-diff-mode-map :states 'normal
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
  )

(use-package tramp
  :straight (:type built-in)
  :custom
  (tramp-default-method "ssh")
  :config
  (setq tramp-verbose 1)
  (setq tramp-persistency-file-name (emacsd "cache/tramp"))
  ;; FIXME: for magit to use newer git
  (add-to-list 'tramp-remote-path "~/.local/bin")
  )

(use-package projectile
  :hook (after-init . projectile-mode)
  :custom
  (projectile-cache-file (emacsd "cache/projectile.cache"))
  (projectile-known-projects-file (emacsd "cache/projectile.projects"))
  (projectile-auto-discover nil)
  (projectile-globally-ignored-files '(".DS_Store" "TAGS"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
  (projectile-ignored-projects '("~/"))
  (projectile-enable-caching (not noninteractive))
  (projectile-kill-buffers-filter 'kill-only-files)
  (projectile-indexing-method 'alien)
  :preface
  ;; disable checking and cleaning especially for remote projects
  (advice-add #'projectile--cleanup-known-projects :override (lambda (&rest _)))
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
  :commands (deft)
  :custom
  (deft-default-extensions "org")
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  (deft-auto-save-interval 0)
  (deft-directory "~/code/notes")
  (deft-file-naming-rules '((noslash . "_") (nospace . "_") (case-fn . downcase)))
  :config
  (evil-set-initial-state 'deft-mode 'insert)
  )

(use-package neotree
  )

;; (use-package treesit-auto
;;   :hook (after-init . global-treesit-auto-mode))

(use-package tree-sitter
  :hook (after-init . global-tree-sitter-mode)
  :hook (tree-sitter-mode . tree-sitter-hl-mode)
  )

(use-package tree-sitter-langs)

(use-package org
  :straight (:type built-in)
  )

;; TODO: org

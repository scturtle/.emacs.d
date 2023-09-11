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

  (load-theme 'aura 'no-confirm)

  ;; sensible defaults
  (setq inhibit-splash-screen t)
  (setq initial-scratch-message nil)
  (setq initial-major-mode 'fundamental-mode) ; for *scratch*
  (setq use-short-answers t) ;; y-or-n-p for >= 28
  (delete-selection-mode +1)
  (setq vc-follow-symlinks t)
  (setq require-final-newline t)
  (show-paren-mode t)
  (setq show-paren-delay 0.0)

  ;; save history of minibuffer, recent files, last place
  (setq savehist-file (emacsd "cache/savehist")
        save-place-file (emacsd "cache/saveplace")
        recentf-save-file (emacsd "cache/recentf")
        recentf-auto-cleanup nil
        recentf-max-saved-items 400)
  (savehist-mode)
  (recentf-mode)
  (save-place-mode)
  (add-hook 'kill-emacs-hook #'recentf-cleanup)

  ;; enable mouse in terminal
  (add-hook 'tty-setup-hook #'xterm-mouse-mode)

  ;; override the env SHELL for fish is slow to start
  (setq-default shell-file-name "/bin/bash")

  ;; from doom-start
  (set-language-environment "UTF-8")
  (setq default-input-method nil)
  (setq bidi-inhibit-bpa t)
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; IO-related tunings
  (setq read-process-output-max (* 1024 1024))
  (when IS-MAC (setq process-adaptive-read-buffering nil)) ;; eshell

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

  ;; for prog mode
  (add-hook 'prog-mode-hook 'electric-pair-mode) ;; TODO: smartparens?
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  ;; TODO: treesitter
  ;; (setq treesit-extra-load-path '("~/code/repos/tree-sitter-module/dist"))

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

  (setq eshell-directory-name (emacsd "cache/eshell"))

  ;; do not show line number in modeline
  (setq line-number-mode nil)

  ;; diff-mode
  (setq diff-refine nil)  ;; no hunk refinement
  )

;; packages
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
  )

;; put after evil for perf issue https://github.com/noctuid/general.el/issues/180
(use-package general
  :demand
  :config
  (general-evil-setup)

  (general-define-key
    :prefix "SPC"
    :states '(normal visual emacs)
    :keymaps 'override

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
    "tn" #'tab-bar-new-tab
    "tm" #'tab-bar-move-tab
    "1"  '((lambda() (interactive) (tab-bar-select-tab 1)) :wk "switch tab 1")
    "2"  '((lambda() (interactive) (tab-bar-select-tab 2)) :wk "switch tab 2")
    "3"  '((lambda() (interactive) (tab-bar-select-tab 3)) :wk "switch tab 3")
    "4"  '((lambda() (interactive) (tab-bar-select-tab 4)) :wk "switch tab 4")
    "5"  '((lambda() (interactive) (tab-bar-select-tab 5)) :wk "switch tab 5")

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
  (general-define-key
   :states 'normal
   "gd" #'+goto-definition
   "gD" #'lsp-ui-peek-find-references
   "gb" #'xref-go-back
   "gf" #'xref-go-forward
   )

  ;; unbind tab for neotree
  (general-define-key
   :states '(normal motion) [?\t] nil)

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

  ;; oevrride evil ex mode (in ":")
  (general-define-key
   :keymaps 'evil-ex-completion-map
   "C-a" 'move-beginning-of-line
   "C-f" 'forward-char
   "C-b" 'backward-char
   "C-d" 'delete-forward-char
   "C-h" 'delete-backward-char
   "C-u" 'evil-delete-back-to-indentation
   "C-k" 'kill-line)

  ;; don't leave visual mode after shifting
  (general-define-key
   :states 'visual
   "<" #'+evil/shift-left
   ">" #'+evil/shift-right)
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
  :general
  ("M-;" 'comment-line
   [remap comment-line] #'evilnc-comment-or-uncomment-lines))

(use-package evil-surround
  :general (:states 'visual "S" 'evil-surround-region))

(use-package undo-fu
  :config
  (setq undo-limit         400000   ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)
  )

(use-package undo-fu-session
  :demand
  :config (undo-fu-session-global-mode)
  :custom (undo-fu-session-directory (emacsd "cache/undo-fu-session"))
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

(use-package nerd-icons)

(use-package doom-modeline
  ;; :disabled
  :demand
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-time-icon nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-buffer-file-true-name t)
  :config
  (doom-modeline-def-modeline 'main
    '(modals matches buffer-info remote-host)
    '(checker misc-info lsp buffer-position time)))

;; (load-file (emacsd "mudline.el"))
;; (mudline-mode)

(use-package vertico
  :straight (:host github :repo "minad/vertico" :files (:defaults "extensions/*"))
  :hook (after-init . vertico-mode)
  :custom
  (vertico-resize nil)
  (vertico-count 17)
  (vertico-cycle t))

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :general
  (:keymaps 'minibuffer-local-map
            "M-A" #'marginalia-cycle))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-ignore-case t)
  )

(use-package consult
  :general
  ([remap apropos]                       #'consult-apropos
   [remap bookmark-jump]                 #'consult-bookmark
   [remap evil-show-marks]               #'consult-mark
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
   [remap projectile-ripgrep]            #'consult-ripgrep)
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

(use-package lsp-mode
  :hook ((c-mode c++-mode python-mode) . lsp-deferred)
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
  ;; fix ccls progress report, need `%%%%' here
  (advice-add #'lsp--progress-status :filter-return
              (lambda (s) (unless (null s) (replace-regexp-in-string "%" "%%" s))))
  ;; for corfu, original style is lsp-passthrough
  (defun +lsp-orderless () (setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (orderless)))))
  (add-hook 'lsp-completion-mode-hook #'+lsp-orderless)
  ;; setup the orderless-flex style for its 1st search term
  (defun +orderless-flex-first (_pat idx _tot) (if (eq idx 0) 'orderless-flex 'orderless-literal))
  (add-hook 'orderless-style-dispatchers #'+orderless-flex-first nil 'local)
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
  :config
  (general-define-key
   :keymaps 'lsp-ui-peek-mode-map
   "k" #'lsp-ui-peek--select-prev
   "j" #'lsp-ui-peek--select-next
   ))

(use-package yasnippet
  :hook (lsp-mode . yas-minor-mode) ;; for yas-enable-snippet
  )

(use-package corfu
  :straight (:host github :repo "minad/corfu" :files (:defaults "extensions/*.el"))
  :hook (prog-mode . corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-preview-current nil)
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  ;; use nerd-icons
  (add-to-list 'corfu-margin-formatters #'+corfu/margin-formatter)
  :general
  (:keymaps 'corfu-map
            "M-SPC" 'corfu-insert-separator
            "M-m"   'corfu-move-to-minibuffer)
  )

(use-package corfu-terminal
  :straight (:host codeberg :repo "scturtle/emacs-corfu-terminal" :branch "test")
  :hook (corfu-mode . corfu-terminal-mode))

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
        '(:index (:trackDependency 1 :threads 8) :completion (:caseSensitivity 0 :filterAndSort t :maxNum 300)))
  (when IS-MAC
    (setq ccls-initialization-options
          (append ccls-initialization-options
                  `(:clang ,(list :extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                                              "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                                              "-isystem/usr/local/include"]
                                  :resourceDir (string-trim (shell-command-to-string "clang -print-resource-dir")))))))
  )

(use-package cmake-mode)

(use-package elisp-def)

(use-package magit
  :custom
  (transient-levels-file  (emacsd "cache/transient/levels"))
  (transient-values-file  (emacsd "cache/transient/values"))
  (transient-history-file (emacsd "cache/transient/history"))
  (transient-default-level 7) ;; for --autostash
  (transient-display-buffer-action '(display-buffer-below-selected))
  (magit-save-repository-buffers nil)
  (magit-revision-insert-related-refs nil)
  (magit-display-buffer-function '+magit-display-buffer-fn)
  ;; (magit-auto-revert-mode nil) ;; too slow for tramp
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
  ;; do not move these to `custom'
  (setq tramp-verbose 1)
  (setq tramp-persistency-file-name (emacsd "cache/tramp"))
  (setq tramp-ssh-controlmaster-options "-o ControlPath=~/.ssh/master-%%h:%%p -o ControlMaster=auto -o ControlPersist=yes")
  ;; for magit to use newer git
  (add-to-list 'tramp-remote-path "~/.local/bin")
  )

(use-package projectile
  :config (projectile-mode)
  :custom
  (projectile-cache-file "/dev/null")
  (projectile-known-projects-file (emacsd "cache/projectile.projects"))
  (projectile-auto-discover nil)
  (projectile-globally-ignored-files '(".DS_Store" "TAGS"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
  (projectile-ignored-projects '("~/"))
  (projectile-enable-caching (not noninteractive))
  (projectile-kill-buffers-filter 'kill-only-files)
  (projectile-indexing-method 'alien)
  :preface
  ;; only load projectile when it is needed
  (autoload 'projectile-project-p "projectile")
  (autoload 'projectile-project-root "projectile")
  ;; disable checking and cleaning especially for remote projects
  (advice-add #'projectile--cleanup-known-projects :override (lambda (&rest _)))
  ;; disable serialize to `projectile-cache-file', may be large to load
  (advice-add #'projectile-serialize-cache :override (lambda (&rest _)))
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
  (general-define-key
   :keymaps 'neotree-mode-map :states 'normal
   "w" #'+neotree/set-width
   )
  )

(use-package tree-sitter
  :hook (prog-mode . global-tree-sitter-mode)
  :hook (tree-sitter-mode . tree-sitter-hl-mode)
  )

(use-package tree-sitter-langs)

(use-package ts-fold
  :straight (:host github :repo "emacs-tree-sitter/ts-fold")
  :hook (tree-sitter-mode . ts-fold-mode)
  )

(use-package org
  :straight (:type built-in)
  :hook (org-mode . org-indent-mode)
  :hook (org-mode . evil-org-mode)
  :custom
  (org-list-allow-alphabetical t)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-cycle-separator-lines 1)
  (org-persist-directory (emacsd "cache/org-persist"))
  (org-src-preserve-indentation t)
  (org-html-head-include-default-style nil) ;; org-html-style-default
  (org-return-follows-link t) ;; org-open-at-point (C-c C-o)
  :config
  (general-define-key
   :keymaps 'org-mode-map
   "C-c s" #'org-insert-structure-template
   )
  ;; open file links in current window, rather than split window
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  )

(use-package htmlize)

(use-package evil-org)

(use-package lsp-pyright
  :custom
  ;; (lsp-pyright-use-library-code-for-types nil)
  (lsp-pyright-python-executable-cmd "python3")
  (lsp-pyright-typechecking-mode "off")
  )

;; setup llvm
(let* ((dirs '("~/workspace/llvm-utils" "~/code/llvm-utils"))
       (llvm-dir (cl-first (cl-remove-if-not 'file-directory-p dirs)))
       (lsp-cmds '("tblgen-lsp-server" "--tablegen-compilation-database=tablegen_compile_commands.yml")))
  (when llvm-dir
    (add-to-list 'load-path (concat llvm-dir "/llvm/utils/emacs"))
    (add-to-list 'load-path (concat llvm-dir "/mlir/utils/emacs"))
    (require 'tablegen-mode)
    (require 'mlir-mode)
    (add-hook 'tablegen-mode-hook #'lsp-deferred)
    (with-eval-after-load 'lsp-mode
      (add-to-list 'lsp-language-id-configuration '(tablegen-mode . "tablegen"))
      (lsp-register-client
       (make-lsp-client :new-connection (lsp-stdio-connection lsp-cmds)
                        :major-modes '(tablegen-mode)
                        :server-id 'tblgenls)))))

(use-package rainbow-mode)

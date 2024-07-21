;;; catppuccin-latte-theme.el --- a theme -*- lexical-binding: t; -*-
;;
;; Author: scturtle <sctuetle@gmail.com>
;; Original-Author: nyxkrage, film42
;; Source: https://github.com/scturtle/.emacs.d
;;         https://github.com/catppuccin/emacs
;;
;;; Commentary:
;;
;;; Code:


(deftheme catppuccin-latte)

(require 'cl-lib)

(defun blend (color1 color2 alpha)
  (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
         (cl-loop for it    in (tty-color-standard-values (downcase color1))
                  for other in (tty-color-standard-values (downcase color2))
                  collect (+ (* alpha (/ it 65535.0)) (* (/ other 65535.0) (- 1 alpha))))))

(defun darken (color alpha)
  (blend color "#000000" (- 1 alpha)))

(defun lighten (color alpha)
  (blend color "#FFFFFF" (- 1 alpha)))

(let*
    ((rosewater "#dc8a78")
     (flamingo  "#dd7878")
     (pink      "#ea76cb")
     (mauve     "#8839ef")
     (red       "#d20f39")
     (maroon    "#e64553")
     (peach     "#fe640b")
     (yellow    "#df8e1d")
     (green     "#40a02b")
     (teal      "#179299")
     (sky       "#04a5e5")
     (sapphire  "#209fb5")
     (blue      "#1e66f5")
     (lavender  "#7287fd")
     (text      "#4c4f69")
     (subtext1  "#5c5f77")
     (subtext0  "#6c6f85")
     (overlay2  "#7c7f93")
     (overlay1  "#8c8fa1")
     (overlay0  "#9ca0b0")
     (surface2  "#acb0be")
     (surface1  "#bcc0cc")
     (surface0  "#ccd0da")
     (base      "#eff1f5")
     (mantle    "#e6e9ef")
     (crust     "#dce0e8")

     (bg        base)
     (current   "#e3e4e8")

     (faces
      `(
        (default :background ,base :foreground ,text)
        (cursor :background ,text)
        (region :background ,surface0 :extend t)
        (highlight :foreground ,text :background ,current)
        (hl-line :background ,current :extend t)
        (hl-todo :foreground ,peach)
        (link :foreground ,lavender :underline t)
        (tooltip :foreground ,overlay2 :background ,surface0)
        (match :background ,rosewater :foreground ,mantle)
        (shadow :foreground ,overlay1)
        (error :foreground ,red)
        (success :foreground ,green)
        (warning :foreground ,yellow)
        (line-number :foreground ,surface1)
        (line-number-current-line :foreground ,lavender)
        (minibuffer-prompt :foreground ,subtext0)
        (mode-line :background ,mantle :foreground ,text)
        (mode-line-inactive :background ,crust :foreground ,overlay0)
        (trailing-whitespace :background ,red)
        (vertical-border :foreground ,mantle :background ,mantle)
        (escape-glyph :foreground ,pink)

        ;; font-lock
        (font-lock-comment-face :inherit shadow)
        (font-lock-comment-delimiter-face :inherit font-lock-comment-face)
        (font-lock-string-face :foreground ,green)
        (font-lock-doc-face :foreground ,overlay1)
        (font-lock-keyword-face :foreground ,mauve)
        (font-lock-builtin-face :foreground ,red)
        (font-lock-function-name-face :foreground ,blue)
        (font-lock-function-call-face :foreground ,blue)
        (font-lock-variable-name-face :foreground ,text)
        (font-lock-variable-use-face :foreground ,text)
        (font-lock-type-face :foreground ,flamingo)
        (font-lock-constant-face :foreground ,peach)
        (font-lock-warning-face :inherit warning)
        (font-lock-negation-char-face :foreground ,sky)
        (font-lock-preprocessor-face :foreground ,teal)
        (font-lock-escape-face :foreground ,pink)
        (font-lock-number-face :foreground ,peach)
        (font-lock-operator-face :foreground ,overlay2)
        (font-lock-property-name-face :foreground ,text)
        (font-lock-property-use-face :foreground ,text)
        (font-lock-punctuation-face :foreground ,overlay2)

        ;; tab-bar
        (tab-bar :foreground ,overlay2 :background ,base)
        (tab-bar-tab :foreground ,base :background ,mauve)
        (tab-bar-tab-inactive :foreground ,text :background ,surface0)

        ;; ansi-color
        (ansi-color-black :foreground ,surface1)
        (ansi-color-red :foreground ,red)
        (ansi-color-yellow :foreground ,yellow)
        (ansi-color-green :foreground ,green)
        (ansi-color-blue :foreground ,blue)
        (ansi-color-magenta :foreground ,pink)
        (ansi-color-cyan :foreground ,teal)
        (ansi-color-white :foreground ,subtext1)
        (ansi-color-bright-black :foreground ,surface2)
        (ansi-color-bright-red :foreground ,red)
        (ansi-color-bright-yellow :foreground ,yellow)
        (ansi-color-bright-green :foreground ,green)
        (ansi-color-bright-blue :foreground ,blue)
        (ansi-color-bright-magenta :foreground ,pink)
        (ansi-color-bright-cyan :foreground ,teal)
        (ansi-color-bright-white :foreground ,subtext0)

        ;; custom
        (custom-variable-tag :foreground ,mauve)
        (custom-group-tag    :foreground ,mauve)
        (custom-state        :foreground ,green)
        (widget-field        :background ,surface0 :extend t)

        ;; dired
        (dired-directory :foreground ,blue)
        (dired-symlink :foreground ,pink)

        ;; eshell
        (eshell-ls-archive :foreground ,mauve)
        (eshell-ls-backup :foreground ,yellow)
        (eshell-ls-clutter :foreground ,red :weight bold)
        (eshell-ls-directory :foreground ,blue :weight bold)
        (eshell-ls-executable :foreground ,green :weight bold)
        (eshell-ls-missing :foreground ,red :weight bold)
        (eshell-ls-product :foreground ,peach)
        (eshell-ls-readonly :foreground ,flamingo)
        (eshell-ls-special :foreground ,sapphire :weight bold)
        (eshell-ls-symlink :foreground ,pink :weight bold)
        (eshell-prompt :foreground ,lavender :weight bold)

        ;; search
        (lazy-highlight :foreground ,mantle :background ,lavender :weight bold)
        (isearch :inherit lazy-highlight)
        (isearch-fail :inherit error)
        (iedit-occurrence :inherit lazy-highlight)
        (iedit-read-only-occurrence :inherit region)

        ;; evil search
        (evil-ex-info   :foreground ,red :slant italic)
        (evil-ex-search :foreground ,mantle :background ,rosewater :weight bold)
        (evil-ex-lazy-highlight :inherit lazy-highlight)
        (evil-ex-substitute-matches :foreground ,red :strike-through t)
        (evil-ex-substitute-replacement :foreground ,green)

        ;; flycheck
        (flycheck-error   :underline (:style wave :color ,red))
        (flycheck-warning :underline (:style wave :color ,yellow))
        (flycheck-info    :underline (:style wave :color ,green))

        ;; diff-hl
        (diff-hl-change :foreground ,yellow :background ,yellow)
        (diff-hl-delete :foreground ,red :background ,red)
        (diff-hl-insert :foreground ,green :background ,green)

        ;; which-key
        (which-key-key-face :inherit font-lock-builtin-face)
        (which-key-group-description-face :inherit font-lock-keyword-face)
        (which-key-command-description-face :inherit default)
        (which-key-separator-face :inherit font-lock-comment-face)
        (which-key-local-map-description-face :foreground ,green)

        ;; show-paren
        (show-paren-match :foreground ,text :weight bold :background ,surface0)
        (show-paren-mismatch :foreground ,mantle :background ,red)

        ;; vertico
        (vertico-current         :background ,current :extend t)
        (vertico-group-title     :foreground ,overlay0)
        (vertico-group-separator :foreground ,overlay0 :strike-through t)
        (consult-file            :foreground ,text)

        ;; orderless
        (orderless-match-face-0 :foreground ,blue :weight bold)
        (orderless-match-face-1 :foreground ,mauve :weight bold)
        (orderless-match-face-2 :foreground ,teal :weight bold)
        (orderless-match-face-3 :foreground ,peach :weight bold)

        ;; marginalia
        (marginalia-documentation :foreground ,overlay0)
        ;; (marginalia-size :foreground ,blue)
        ;; (marginalia-date :foreground ,mauve)

        ;; transient
        (transient-active-infix :background ,current :extend t)

        ;; magit section
        (magit-section-heading :foreground ,blue :weight bold)
        (magit-section-highlight :background unspecified)
        (magit-section-heading-selection :foreground ,peach :weight bold)
        ;; magit diff
        (magit-diff-added             :background ,(blend green bg 0.05) :foreground ,green :extend t)
        (magit-diff-added-highlight   :background ,(blend green bg 0.1) :foreground ,green :extend t)
        (magit-diff-removed           :background ,(blend red bg 0.05) :foreground ,red :extend t)
        (magit-diff-removed-highlight :background ,(blend red bg 0.1) :foreground ,red :extend t)
        (magit-diff-base              :foreground ,peach :extend t)
        (magit-diff-base-highlight    :background ,base :foreground ,peach :extend t)
        (magit-diff-context           :foreground ,overlay2 :extend t)
        (magit-diff-context-highlight :background ,base :foreground ,text :extend t)
        (magit-diff-hunk-heading           :background ,surface0)
        (magit-diff-hunk-heading-highlight :background ,surface1)
        (magit-diff-hunk-heading-selection :foreground ,peach)
        (magit-diff-file-heading           :foreground ,text :weight bold)
        (magit-diff-file-heading-highlight :inherit magit-diff-file-heading)
        (magit-diff-file-heading-selection :foreground ,peach)
        (magit-diff-lines-heading :inherit magit-diff-hunk-heading-highlight)
        (magit-diff-hunk-region :inherit unspecified)
        (magit-diffstat-added :foreground ,green)
        (magit-diffstat-removed :foreground ,red)
        (magit-diff-revision-summary :foreground ,blue :weight bold)
        (magit-diff-revision-summary-highlight :inherit magit-diff-revision-summary)
        (git-commit-summary :inherit magit-diff-revision-summary)
        ;; magit bisect/blame/branch
        (magit-bisect-bad     :foreground ,red)
        (magit-bisect-good    :foreground ,green)
        (magit-bisect-skip    :foreground ,peach)
        (magit-blame-hash     :foreground ,yellow)
        (magit-blame-date     :foreground ,yellow)
        (magit-blame-heading  :foreground ,yellow :background ,mantle :extend t)
        (magit-branch-current :foreground ,blue)
        (magit-branch-local   :foreground ,sky)
        (magit-branch-remote  :foreground ,green)
        (magit-branch-remote-head :foreground ,green)
        ;; magit log
        (magit-header-line :background ,crust :foreground ,blue :underline nil :weight bold)
        (magit-tag         :foreground ,peach)
        (magit-hash        :foreground ,blue)
        (magit-filename    :foreground ,text)
        (magit-log-author  :foreground ,mauve)
        (magit-log-date    :foreground ,teal)
        (magit-log-graph   :foreground ,surface1)
        (magit-process-ng  :foreground ,peach :weight bold)
        (magit-process-ok  :foreground ,green :weight bold)

        ;; smerge-mode
        (smerge-upper    :background ,(blend red bg 0.2) :extend t)
        (smerge-lower    :background ,(blend green bg 0.2) :extend t)
        (smerge-base     :background ,(blend peach bg 0.2) :extend t)
        (smerge-markers  :background ,(blend blue bg 0.2) :extend t)
        ;; diff-mode
        (diff-header      :background unspecified)
        (diff-file-header :background unspecified :foreground ,text)
        (diff-hunk-header :background ,(blend blue bg 0.2))
        (diff-removed     :background ,(blend red bg 0.2))
        (diff-added       :background ,(blend green bg 0.2))

        ;; neotree
        (neo-root-dir-face :foreground ,green)
        (neo-file-link-face :inherit link)
        (neo-dir-link-face :foreground ,blue)
        (neo-expand-btn-face :foreground ,text)
        (neo-vc-edited-face :foreground ,rosewater)
        (neo-vc-added-face :foreground ,green)
        (neo-vc-removed-face :foreground ,red)
        (neo-vc-conflict-face :inherit error)
        (neo-vc-ignored-face :inherit font-lock-comment-face)
        (neo-vc-up-to-date-face :foreground ,text)
        (neo-vc-unregistered-face :foreground ,pink)

        ;; lsp-mode
        (lsp-face-highlight-textual :inherit highlight)
        (lsp-face-highlight-read    :inherit lsp-face-highlight-textual)
        (lsp-face-highlight-write   :inherit lsp-face-highlight-textual)
        ;; lsp-ui-peek
        (lsp-ui-peek-filename    :foreground ,teal)
        (lsp-ui-peek-header      :foreground ,blue :background ,crust)
        (lsp-ui-peek-selection   :background ,surface0)
        (lsp-ui-peek-list        :background ,mantle)
        (lsp-ui-peek-peek        :background ,mantle)
        (lsp-ui-peek-highlight   :background ,crust)
        (lsp-ui-peek-line-number :inherit line-number)

        ;; corfu
        (corfu-default :background ,mantle)
        (corfu-current :background ,surface0)
        (corfu-bar     :background ,surface0)

        ;; org mode
        (org-archived :inherit font-lock-comment-face)
        (org-block :extend t :background ,mantle :foreground ,subtext1)
        (org-block-begin-line :inherit org-meta-line :extend t :background ,mantle)
        (org-block-end-line :inherit org-block-begin-line :extend t :background ,mantle)
        (org-done :inherit font-lock-comment-face)
        (org-todo :foreground ,peach)
        (org-headline-done :inherit org-done)
        (org-checkbox                 :inherit org-todo)
        (org-checkbox-statistics-done :inherit org-done)
        (org-checkbox-statistics-todo :inherit org-todo)
        (org-code :foreground ,green)
        (org-meta-line :inherit font-lock-comment-face)
        (org-document-info :foreground ,sapphire)
        (org-document-info-keyword :inherit font-lock-comment-face)
        (org-document-title :weight bold :foreground ,blue)
        (org-drawer :inherit font-lock-comment-face)
        (org-footnote :foreground ,mauve)
        (org-link :inherit link)
        (org-priority :foreground ,yellow)
        (org-priority-value :inherit font-lock-comment-face)
        (org-quote :inherit org-block :slant italic)
        (org-table :foreground ,pink)
        (org-tag :foreground ,mauve :weight bold)
        (org-verbatim :foreground ,green)
        (org-level-1 :inherit bold :foreground ,red)
        (org-level-2 :inherit bold :foreground ,peach)
        (org-level-3 :weight normal :foreground ,yellow)
        (org-level-4 :weight normal :foreground ,green)
        (org-level-5 :weight normal :foreground ,sapphire)
        (org-level-6 :weight normal :foreground ,lavender)
        (org-level-7 :weight normal :foreground ,mauve)
        (org-level-8 :weight normal :foreground ,maroon)
        ;; org-agenda
        (org-agenda-date :weight normal :foreground ,blue)
        (org-agenda-date-today :weight bold :foreground ,blue :underline t)
        (org-scheduled-previously :foreground ,rosewater)
        (org-scheduled-today :foreground ,mauve)
        (org-scheduled :foreground ,flamingo)
        )))

  (apply #'custom-theme-set-faces
         'catppuccin-latte
         (mapcar (lambda (face) `(,(car face) ((t ,(cdr face))))) faces))
  )

(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'catppuccin-latte)
;;; catppuccin-latte-theme.el ends here

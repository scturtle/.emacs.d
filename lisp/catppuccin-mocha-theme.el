;;; catppuccin-mocha-theme.el --- a theme -*- lexical-binding: t; -*-
;;
;; Author: scturtle <sctuetle@gmail.com>
;; Original-Author: nyxkrage, film42
;; Source: https://github.com/scturtle/.emacs.d
;;         https://github.com/catppuccin/emacs
;;
;;; Commentary:
;;
;;; Code:


(deftheme catppuccin-mocha)

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
    ((rosewater "#f5e0dc")
     (flamingo  "#f2cdcd")
     (pink      "#f5c2e7")
     (mauve     "#cba6f7")
     (red       "#f38ba8")
     (maroon    "#eba0ac")
     (peach     "#fab387")
     (yellow    "#f9e2af")
     (green     "#a6e3a1")
     (teal      "#94e2d5")
     (sky       "#89dceb")
     (sapphire  "#74c7ec")
     (blue      "#89b4fa")
     (lavender  "#b4befe")
     (text      "#cdd6f4")
     (subtext1  "#bac2de")
     (subtext0  "#a6adc8")
     (overlay2  "#9399b2")
     (overlay1  "#7f849c")
     (overlay0  "#6c7086")
     (surface2  "#585b70")
     (surface1  "#45475a")
     (surface0  "#313244")
     (base      "#1e1e2e")
     (mantle    "#181825")
     (crust     "#11111b")

     (current   (lighten base 0.05))
     (comments  surface1)

     (faces
      `(
        (cursor :background ,rosewater)
        (default :background ,base :foreground ,text)
        (region :background ,surface0 :extend t)
        (highlight :foreground ,text :background ,current)
        (hl-line :background ,current :extend t)
        (link :foreground ,lavender :underline t)
        (fringe :background ,base :foreground ,surface1)
        (tooltip :foreground ,overlay2 :background ,surface0)
        (match :background ,red :foreground ,mantle)
        (shadow :foreground ,overlay0)
        (error :foreground ,red)
        (success :foreground ,green)
        (warning :foreground ,yellow)
        (line-number :inherit default :foreground ,surface1 :background ,base)
        (line-number-current-line :inherit line-number :foreground ,lavender)
        (minibuffer-prompt :weight normal :foreground ,subtext0)
        (mode-line :background ,mantle nil :foreground ,text)
        (mode-line-inactive :background ,crust :inverse-video nil :foreground ,overlay0)
        (trailing-whitespace :inherit warning)
        (vertical-border :foreground ,mantle)
        (escape-glyph :foreground ,green)

        ;; font-lock
        (font-lock-comment-face :inherit shadow)
        (font-lock-comment-delimiter-face :inherit shadow)
        (font-lock-string-face :foreground ,green)
        (font-lock-doc-face :inherit font-lock-comment-face)
        (font-lock-keyword-face :foreground ,mauve)
        (font-lock-builtin-face :foreground ,red)
        (font-lock-function-name-face :foreground ,blue)
        (font-lock-function-call-face :foreground ,blue)
        (font-lock-variable-name-face :foreground ,text)
        (font-lock-variable-use-face :foreground ,text)
        (font-lock-type-face :foreground ,yellow)
        (font-lock-constant-face :foreground ,peach)
        (font-lock-warning-face :inherit warning)
        (font-lock-negation-char-face :foreground ,sky)
        (font-lock-preprocessor-face :foreground ,yellow)
        (font-lock-escape-face :foreground ,pink)
        (font-lock-number-face :foreground ,peach)
        (font-lock-operator-face :foreground ,sky)
        (font-lock-property-name-face :foreground ,text)
        (font-lock-property-use-face :foreground ,text)
        (font-lock-punctuation-face :foreground ,overlay2)

        ;; tab-bar
        (tab-bar :foreground ,subtext0 :background ,base)
        (tab-bar-tab :foreground ,text :background ,current)
        (tab-bar-tab-inactive :foreground ,subtext0 :background ,base)

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
        (custom-variable-tag :foreground ,pink)
        (custom-group-tag    :foreground ,pink)
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
        (eshell-ls-special :foreground ,pink :weight bold)
        (eshell-ls-symlink :foreground ,sapphire :weight bold)
        (eshell-prompt :foreground ,blue :weight bold)

        ;; search
        (lazy-highlight :foreground ,subtext1 :background ,surface1)
        (isearch :inherit match :weight bold)
        (isearch-fail :inherit error)
        (iedit-occurrence :inherit match :weight bold)
        (iedit-read-only-occurrence :inherit region)

        ;; evil search
        (evil-ex-info   :foreground ,red :slant italic)
        (evil-ex-search :background ,current :foreground ,pink :weight bold)
        (evil-ex-lazy-highlight :inherit lazy-highlight)
        (evil-ex-substitute-matches :foreground ,red :underline t)
        (evil-ex-substitute-replacement :foreground ,green :underline t)

        ;; flycheck
        (flycheck-error   :foreground ,red :underline t)
        (flycheck-warning :foreground ,yellow :underline t)
        (flycheck-info    :foreground ,green :underline t)

        ;; diff-hl
        (diff-hl-change :foreground ,blue :background ,blue)
        (diff-hl-delete :foreground ,red :background ,red)
        (diff-hl-insert :foreground ,green :background ,green)

        ;; which-key
        (which-key-key-face :inherit font-lock-builtin-face)
        (which-key-command-description-face :inherit default)
        (which-key-separator-face :inherit font-lock-comment-delimiter-face)
        (which-key-local-map-description-face :foreground ,green)

        ;; show-paren
        (show-paren-match :foreground ,pink :weight bold :background ,surface0)
        (show-paren-match-expression :inherit match)
        (show-paren-mismatch :inherit warning)

        ;; vertico
        (vertico-current         :background ,surface0 :extend t)
        (vertico-group-title     :foreground ,comments)
        (vertico-group-separator :foreground ,comments :strike-through t)

        ;; orderless
        (orderless-match-face-0 :foreground ,blue :weight bold)
        (orderless-match-face-1 :foreground ,mauve :weight bold)
        (orderless-match-face-2 :foreground ,teal :weight bold)
        (orderless-match-face-3 :foreground ,peach :weight bold)

        ;; marginalia
        (marginalia-documentation   :foreground ,comments)
        (marginalia-size            :foreground ,blue)
        (marginalia-date            :foreground ,pink)
        (marginalia-file-priv-dir   :foreground ,blue)
        (marginalia-file-priv-exec  :foreground ,green)
        (marginalia-file-priv-link  :foreground ,pink)
        (marginalia-file-priv-other :foreground ,pink)
        (marginalia-file-priv-rare  :foreground ,pink)
        (marginalia-file-priv-read  :foreground ,yellow)
        (marginalia-file-priv-write :foreground ,red)

        ;; magit section
        (magit-section-heading :foreground ,blue :weight bold)
        (magit-section-heading-selection :foreground ,peach :weight bold)
        (magit-section-highlight :background ,base :extend t)
        (magit-section-secondary-heading :foreground ,pink :weight bold :extend t)
        ;; magit diff
        (magit-diff-added :foreground ,green :extend t)
        (magit-diff-added-highlight :background ,(blend green base 0.1) :foreground ,green :extend t)
        (magit-diff-removed :foreground ,red :extend t)
        (magit-diff-removed-highlight :background ,(blend red base 0.1) :foreground ,red :extend t)
        (magit-diff-base :foreground ,peach :extend t)
        (magit-diff-base-highlight :background ,base :foreground ,peach :extend t)
        (magit-diff-context :foreground ,yellow :extend t)
        (magit-diff-context-highlight :background ,base :foreground ,text :extend t)
        (magit-diff-hunk-heading :inherit diff-hunk-header)
        (magit-diff-hunk-heading-highlight :inherit diff-hunk-header :weight bold)
        (magit-diff-file-heading :foreground ,text)
        (magit-diff-file-heading-highlight :inherit magit-section-highlight)
        (magit-diff-lines-heading :inherit magit-diff-hunk-heading-highlight)
        (magit-diffstat-added :foreground ,green)
        (magit-diffstat-removed :foreground ,red)
        (magit-diff-revision-summary :foreground ,blue :weight bold)
        (magit-diff-revision-summary-highlight :foreground ,blue :weight bold)
        (diff-header :foreground ,blue)
        (diff-hunk-header :foreground ,text :background ,surface2)
        ;; magit bisect/blame/branch
        (magit-bisect-bad     :foreground ,red)
        (magit-bisect-good    :foreground ,green)
        (magit-bisect-skip    :foreground ,peach)
        (magit-blame-hash     :foreground ,comments)
        (magit-blame-date     :foreground ,yellow)
        (magit-blame-heading  :foreground ,yellow :background ,surface0 :extend t)
        (magit-branch-current :foreground ,blue)
        (magit-branch-local   :foreground ,sky)
        (magit-branch-remote  :foreground ,green)
        ;; magit log
        (magit-header-line :background ,pink :foreground ,blue :underline nil :weight bold)
        (magit-tag :foreground ,peach)
        (magit-hash :foreground ,subtext0)
        (magit-filename :foreground ,pink)
        (magit-log-author :foreground ,subtext0)
        (magit-log-date   :foreground ,pink)
        (magit-log-graph  :foreground ,comments)
        (magit-process-ng :foreground ,peach :weight bold)
        (magit-process-ok :foreground ,green :weight bold)
        ;; TODO: magit cherry dimmed reflog sequence signature

        ;; smerge-mode
        (smerge-upper    :background ,red :extend t)
        (smerge-lower    :background ,green :extend t)
        (smerge-base     :background ,peach :extend t)
        (smerge-markers  :background ,blue :extend t)

        ;; neotree
        (neo-root-dir-face :foreground ,green :weight bold)
        (neo-file-link-face :inherit link)
        (neo-dir-link-face :foreground ,blue :weight bold)
        (neo-expand-btn-face :foreground ,text)
        (neo-vc-edited-face :foreground ,peach)
        (neo-vc-added-face :foreground ,green)
        (neo-vc-removed-face :foreground ,red)
        (neo-vc-conflict-face :inherit error)
        (neo-vc-ignored-face :inherit font-lock-comment-face)
        (neo-vc-up-to-date-face :foreground ,text)

        ;; lsp-mode
        (lsp-face-highlight-textual :background ,surface1 :foreground ,text)
        (lsp-face-highlight-read    :inherit lsp-face-highlight-textual)
        (lsp-face-highlight-write   :inherit lsp-face-highlight-textual)
        ;; lsp-ui-peek
        (lsp-ui-peek-filename    :foreground ,yellow)
        (lsp-ui-peek-header      :foreground ,text :background ,surface0)
        (lsp-ui-peek-selection   :foreground ,text :background ,surface1)
        (lsp-ui-peek-list        :background ,mantle)
        (lsp-ui-peek-peek        :background ,mantle)
        (lsp-ui-peek-highlight   :foreground ,subtext1 :background ,surface1)
        (lsp-ui-peek-line-number :inherit line-number)

        ;; corfu
        (corfu-default :background ,current)
        (corfu-current :background ,surface0)

        ;; org mode
        (org-archived :foreground ,comments)
        (org-block :extend t :background ,mantle :foreground ,green)
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
        (org-drawer :foreground ,comments)
        (org-footnote :foreground ,mauve)
        (org-link :inherit link)
        (org-priority :foreground ,yellow)
        (org-priority-value :foreground ,comments)
        (org-quote :inherit org-block :slant italic)
        (org-table :foreground ,pink)
        (org-tag :foreground ,mauve :weight bold)
        (org-verbatim :foreground ,green)
        ;; org-level-*
        (org-level-1 :inherit bold :foreground ,red)
        (org-level-2 :inherit bold :foreground ,peach)
        (org-level-3 :weight normal :foreground ,yellow)
        (org-level-4 :weight normal :foreground ,green)
        (org-level-5 :weight normal :foreground ,sapphire)
        (org-level-6 :weight normal :foreground ,lavender)
        (org-level-7 :weight normal :foreground ,mauve)
        (org-level-8 :weight normal :foreground ,maroon)

        )))

  (apply #'custom-theme-set-faces
         'catppuccin-mocha
         (mapcar (lambda (face) `(,(car face) ((t ,(cdr face))))) faces))
  )

(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'catppuccin-mocha)
;;; catppuccin-mocha-theme.el ends here

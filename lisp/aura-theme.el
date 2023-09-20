;;; aura-theme.el --- aura theme -*- lexical-binding: t; -*-
;;
;; Author: scturtle <https://github.com/scturtle>
;; Source: https://github.com/scturtle/.emacs.d
;;
;;; Commentary:
;;
;;; Code:


(deftheme aura)

(require 'cl-lib)

;;;###autoload
(defun blend (color1 color2 alpha)
  (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
         (cl-loop for it    in (tty-color-standard-values (downcase color1))
                  for other in (tty-color-standard-values (downcase color2))
                  collect (+ (* alpha (/ it 65535.0)) (* (/ other 65535.0) (- 1 alpha))))))

;;;###autoload
(defun darken (color alpha)
  (blend color "#000000" (- 1 alpha)))

;;;###autoload
(defun lighten (color alpha)
  (blend color "#FFFFFF" (- 1 alpha)))

(let* ((bg      "#21202e")
       (bg-alt  "#1c1b27")
       (fg      "#edecee")
       (fg-alt  "#e1e0e2")

       (grey    "#b6b6b2")
       (red     "#ff6767")
       (yellow  "#ffca85")
       (orange  "#ffca85")
       (green   "#61ffca")
       (blue    "#a277ff")
       (magenta "#a277ff")
       (violet  "#f694ff")
       (cyan    "#61ffca")

       (blue1   "#82e2ff")
       (blue2   "#7e7edd")
       (blue3   "RoyalBlue3")

       (region       "#353441")
       (inactive-fg  "#494854")
       (comments     "#6272a4")
       (highlight    blue)
       (code-bg      (darken bg 0.125))
       (selection-bg (blend blue bg 0.3))

       (builtin      blue)
       (doc-comments (lighten comments 0.25))
       (constants    blue1)
       (functions    orange)
       (keywords     blue)
       (operators    blue)
       (type         blue)
       (strings      green)
       (variables    violet)
       (numbers      green)
       (error        red)
       (warning      yellow)
       (success      green)

       (vc-modified  orange)
       (vc-added     green)
       (vc-deleted   red)
       )

  (custom-theme-set-faces
   'aura

   ;;;; basics
   `(default   ((t (:background ,bg :foreground ,fg))))
   `(cursor    ((t (:background ,fg))))
   `(region    ((t (:background ,region))))
   `(highlight ((t (:foreground ,highlight :background ,bg-alt))))
   `(hl-line   ((t (:background ,bg-alt))))
   `(link      ((t (:foreground ,highlight :underline t :weight bold))))
   `(fringe    ((t (:foreground ,inactive-fg :background ,bg))))
   `(tooltip   ((t (:foreground ,fg :background ,bg-alt))))
   `(match     ((t (:foreground ,fg :background ,selection-bg))))
   `(shadow    ((t (:foreground ,grey))))
   `(error     ((t (:foreground ,error))))
   `(warning   ((t (:foreground ,warning))))
   `(success   ((t (:foreground ,success))))
   `(mode-line           ((t (:background ,bg-alt :foreground ,fg-alt))))
   `(mode-line-inactive  ((t (:background ,bg :foreground ,inactive-fg))))
   `(escape-glyph        ((t (:foreground ,cyan))))
   `(vertical-border     ((t (:background ,bg-alt :foreground ,bg-alt))))
   `(minibuffer-prompt   ((t (:foreground ,highlight))))
   `(trailing-whitespace ((t (:foreground ,fg :background ,red))))

   ;;;; font-lock
   `(font-lock-builtin-face  ((t (:foreground ,builtin))))
   `(font-lock-comment-face  ((t (:foreground ,comments))))
   `(font-lock-constant-face ((t (:foreground ,constants))))
   `(font-lock-doc-face      ((t (:foreground ,doc-comments))))
   `(font-lock-keyword-face  ((t (:foreground ,keywords))))
   `(font-lock-string-face   ((t (:foreground ,strings))))
   `(font-lock-type-face     ((t (:foreground ,type))))
   `(font-lock-warning-face  ((t (:inherit 'warning))))
   `(font-lock-function-name-face ((t (:foreground ,functions))))
   `(font-lock-variable-name-face ((t (:foreground ,variables))))
   `(font-lock-comment-delimiter-face ((t (:inherit 'font-lock-comment-face))))
   `(font-lock-negation-char-face        ((t (:weight bold :foreground ,operators))))
   `(font-lock-preprocessor-face         ((t (:weight bold :foreground ,operators))))
   `(font-lock-preprocessor-char-face    ((t (:weight bold :foreground ,operators))))
   `(font-lock-regexp-grouping-backslash ((t (:weight bold :foreground ,operators))))
   `(font-lock-regexp-grouping-construct ((t (:weight bold :foreground ,operators))))

   ;;;; line-number
   `(line-number ((t (:foreground ,comments :background ,bg :italic t))))
   `(line-number-current-line ((t (:inherit 'line-number :foreground ,fg-alt))))

   ;;;; tab-bar
   `(tab-bar              ((t (:background ,bg-alt :foreground ,bg-alt))))
   `(tab-bar-tab          ((t (:background ,bg :foreground ,fg))))
   `(tab-bar-tab-inactive ((t (:background ,bg-alt :foreground ,inactive-fg))))

   ;;;; ansi-color
   `(ansi-color-black          ((t (:foreground ,bg      :background ,bg))))
   `(ansi-color-red            ((t (:foreground ,red     :background ,red))))
   `(ansi-color-green          ((t (:foreground ,green   :background ,green))))
   `(ansi-color-yellow         ((t (:foreground ,yellow  :background ,yellow))))
   `(ansi-color-blue           ((t (:foreground ,blue    :background ,blue))))
   `(ansi-color-magenta        ((t (:foreground ,magenta :background ,magenta))))
   `(ansi-color-cyan           ((t (:foreground ,cyan    :background ,cyan))))
   `(ansi-color-white          ((t (:foreground ,fg      :background ,fg))))
   `(ansi-color-bright-black   ((t (:foreground ,bg      :background ,bg))))
   `(ansi-color-bright-red     ((t (:foreground ,red     :background ,red))))
   `(ansi-color-bright-green   ((t (:foreground ,green   :background ,green))))
   `(ansi-color-bright-yellow  ((t (:foreground ,yellow  :background ,yellow))))
   `(ansi-color-bright-blue    ((t (:foreground ,blue    :background ,blue))))
   `(ansi-color-bright-magenta ((t (:foreground ,magenta :background ,magenta))))
   `(ansi-color-bright-cyan    ((t (:foreground ,cyan    :background ,cyan))))
   `(ansi-color-bright-white   ((t (:foreground ,fg      :background ,fg))))

   ;;;; custom
   `(custom-variable-tag ((t (:foreground ,violet))))
   `(custom-group-tag    ((t (:foreground ,violet))))
   `(custom-state        ((t (:foreground ,green))))
   `(widget-field ((t (:background ,region :extend t))))

   ;;;; dired
   `(dired-directory ((t (:foreground ,blue))))
   `(dired-symlink   ((t (:foreground ,constants))))

   ;;;; eshell
   `(eshell-prompt        ((t (:foreground ,highlight))))
   `(eshell-ls-archive    ((t (:foreground ,magenta))))
   `(eshell-ls-backup     ((t (:foreground ,yellow))))
   `(eshell-ls-clutter    ((t (:foreground ,red))))
   `(eshell-ls-directory  ((t (:foreground ,blue))))
   `(eshell-ls-executable ((t (:foreground ,green))))
   `(eshell-ls-missing    ((t (:foreground ,red))))
   `(eshell-ls-product    ((t (:foreground ,orange))))
   `(eshell-ls-readonly   ((t (:foreground ,orange))))
   `(eshell-ls-special    ((t (:foreground ,violet))))
   `(eshell-ls-symlink    ((t (:foreground ,cyan))))
   `(eshell-ls-unreadable ((t (:foreground ,comments))))

   ;;;; search
   `(lazy-highlight   ((t (:background ,(darken highlight 0.4) :foreground ,fg :weight bold))))
   `(isearch          ((t (:inherit 'lazy-highlight))))
   `(isearch-fail     ((t (:background ,red :foreground ,fg :weight bold))))
   `(iedit-occurrence ((t (:foreground ,magenta :weight bold :inverse-video t))))

   ;;;; evil search
   `(evil-ex-info                   ((t (:foreground ,error :slant italic))))
   `(evil-ex-search                 ((t (:background ,highlight :foreground ,fg :weight bold))))
   `(evil-ex-lazy-highlight         ((t (:inherit 'lazy-highlight))))
   `(evil-ex-substitute-matches     ((t (:background ,bg-alt :foreground ,red   :weight bold :strike-through t))))
   `(evil-ex-substitute-replacement ((t (:background ,bg-alt :foreground ,green :weight bold))))

   ;;;; flycheck
   `(flycheck-error   ((t (:foreground ,red :underline t))))
   `(flycheck-warning ((t (:foreground ,yellow :underline t))))
   `(flycheck-info    ((t (:foreground ,green :underline t))))

   ;;;; diff-hl
   `(diff-hl-change ((t (:foreground ,vc-modified :background ,vc-modified))))
   `(diff-hl-delete ((t (:foreground ,vc-deleted  :background ,vc-deleted))))
   `(diff-hl-insert ((t (:foreground ,vc-added    :background ,vc-added))))

   ;;;; which-key
   `(which-key-key-face                   ((t (:foreground ,green))))
   `(which-key-group-description-face     ((t (:foreground ,blue))))
   `(which-key-command-description-face   ((t (:foreground ,violet))))
   `(which-key-local-map-description-face ((t (:foreground ,orange))))
   `(help-key-binding  ((t (:foreground ,blue1 :background unspecified))))

   ;;;; show-paren
   `(show-paren-match    ((t (:background ,blue2 :foreground ,fg))))
   `(show-paren-mismatch ((t (:background ,red   :foreground ,fg))))

   ;;;; vertico
   `(vertico-current         ((t (:background ,region :extend t))))
   `(vertico-group-title     ((t (:foreground ,comments))))
   `(vertico-group-separator ((t (:foreground ,comments :strike-through t))))

   ;;;; orderless
   `(orderless-match-face-0 ((t (:foreground ,(blend blue3  fg 0.6) :background ,(blend blue3  bg 0.1)))))
   `(orderless-match-face-1 ((t (:foreground ,(blend green  fg 0.6) :background ,(blend green  bg 0.1)))))
   `(orderless-match-face-2 ((t (:foreground ,(blend violet fg 0.6) :background ,(blend violet bg 0.1)))))
   `(orderless-match-face-3 ((t (:foreground ,(blend yellow fg 0.6) :background ,(blend yellow bg 0.1)))))

   ;;;; marginalia
   `(marginalia-documentation   ((t (:foreground ,doc-comments))))
   `(marginalia-size            ((t (:foreground ,blue))))
   `(marginalia-date            ((t (:foreground ,violet))))
   `(marginalia-file-priv-dir   ((t (:foreground ,blue))))
   `(marginalia-file-priv-exec  ((t (:foreground ,green))))
   `(marginalia-file-priv-link  ((t (:foreground ,violet))))
   `(marginalia-file-priv-other ((t (:foreground ,magenta))))
   `(marginalia-file-priv-rare  ((t (:foreground ,fg))))
   `(marginalia-file-priv-read  ((t (:foreground ,yellow))))
   `(marginalia-file-priv-write ((t (:foreground ,red))))

   ;;;; magit section
   `(magit-section-heading           ((t (:foreground ,blue :weight bold :extend t))))
   `(magit-section-heading-selection ((t (:foreground ,orange :weight bold :extend t))))
   `(magit-section-highlight         ((t (:inherit 'hl-line))))
   `(magit-section-secondary-heading ((t (:foreground ,violet :weight bold :extend t))))
   ;;;; magit diff
   `(magit-diff-added             ((t (:foreground ,(darken vc-added 0.2) :background ,(blend vc-added bg 0.1) :extend t))))
   `(magit-diff-added-highlight   ((t (:foreground ,vc-added :background ,(blend vc-added bg 0.2) :weight bold :extend t))))
   `(magit-diff-removed           ((t (:foreground ,(darken vc-deleted 0.2) :background ,(blend vc-deleted bg 0.1) :extend t))))
   `(magit-diff-removed-highlight ((t (:foreground ,vc-deleted :background ,(blend vc-deleted bg 0.2) :weight bold :extend t))))
   `(magit-diff-base              ((t (:foreground ,(darken orange 0.2) :background ,(blend orange bg 0.1) :extend t))))
   `(magit-diff-base-highlight    ((t (:foreground ,orange :background ,(blend orange bg 0.2) :weight bold :extend t))))
   `(magit-diff-context           ((t (:foreground ,(darken fg 0.4) :background ,bg :extend t))))
   `(magit-diff-context-highlight ((t (:foreground ,fg :background ,bg-alt :extend t))))
   `(magit-diff-hunk-heading           ((t (:foreground ,fg-alt :background ,(darken blue3 0.3) :extend t))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,fg     :background ,blue3 :weight bold :extend t))))
   ;; `(magit-diff-file-heading           ((t (:foreground ,fg :weight bold :extend t))))
   ;; `(magit-diff-file-heading-selection ((t (:foreground ,magenta :background ,blue :weight bold :extend t))))
   ;; `(magit-diff-lines-heading          ((t (:foreground ,yellow :background ,red :extend t))))
   `(magit-diffstat-added   ((t (:foreground ,vc-added))))
   `(magit-diffstat-removed ((t (:foreground ,vc-deleted))))
   ;;;; magit bisect/blame/branch
   `(magit-bisect-bad     ((t (:foreground ,red))))
   `(magit-bisect-good    ((t (:foreground ,green))))
   `(magit-bisect-skip    ((t (:foreground ,orange))))
   `(magit-blame-hash     ((t (:foreground ,comments))))
   `(magit-blame-date     ((t (:foreground ,yellow))))
   `(magit-blame-heading  ((t (:foreground ,yellow :background ,region :extend t))))
   `(magit-branch-current ((t (:foreground ,blue))))
   `(magit-branch-local   ((t (:foreground ,cyan))))
   `(magit-branch-remote  ((t (:foreground ,green))))
   ;;;; magit log
   `(magit-header-line ((t (:background ,bg :foreground ,blue :underline nil :weight bold))))
   `(magit-tag         ((t (:foreground ,yellow))))
   `(magit-hash        ((t (:foreground ,comments))))
   `(magit-filename    ((t (:foreground ,fg))))
   `(magit-log-author  ((t (:foreground ,blue))))
   `(magit-log-date    ((t (:foreground ,violet))))
   `(magit-log-graph   ((t (:foreground ,comments))))
   `(magit-process-ng  ((t (:inherit 'error))))
   `(magit-process-ok  ((t (:inherit 'success))))
   ;; TODO: magit cherry dimmed reflog sequence signature

   ;;;; neotree
   `(neo-root-dir-face      ((t (:foreground ,green))))
   `(neo-file-link-face     ((t (:foreground ,constants))))
   `(neo-dir-link-face      ((t (:foreground ,blue))))
   `(neo-expand-btn-face    ((t (:foreground ,blue))))
   `(neo-vc-edited-face     ((t (:foreground ,yellow))))
   `(neo-vc-added-face      ((t (:foreground ,green))))
   `(neo-vc-removed-face    ((t (:foreground ,red :strike-through t))))
   `(neo-vc-conflict-face   ((t (:foreground ,magenta :weight bold))))
   `(neo-vc-ignored-face    ((t (:foreground ,comments))))
   `(neo-vc-up-to-date-face ((t (:foreground ,grey))))

   ;;;; lsp-mode
   `(lsp-face-highlight-textual ((t (:background ,selection-bg :foreground ,fg))))
   `(lsp-face-highlight-read    ((t (:inherit 'lsp-face-highlight-textual))))
   `(lsp-face-highlight-write   ((t (:inherit 'lsp-face-highlight-textual))))
   ;;;; lsp-ui-peek
   `(lsp-ui-peek-filename    ((t (:foreground ,yellow))))
   `(lsp-ui-peek-header      ((t (:foreground ,fg :background ,blue3))))
   `(lsp-ui-peek-selection   ((t (:foreground ,fg :background ,selection-bg))))
   `(lsp-ui-peek-list        ((t (:background ,bg-alt))))
   `(lsp-ui-peek-peek        ((t (:background ,bg-alt))))
   `(lsp-ui-peek-highlight   ((t (:foreground ,fg-alt :background ,selection-bg))))
   `(lsp-ui-peek-line-number ((t (:inherit 'line-number))))

   ;;;; corfu
   `(corfu-default ((t (:inherit 'tooltip))))
   `(corfu-current ((t (:background ,selection-bg :foreground ,fg))))

   ;;;; tree-sitter
   `(tree-sitter-hl-face:property           ((t (:inhert nil :foreground ,blue2))))
   `(tree-sitter-hl-face:punctuation        ((t (:foreground ,grey))))
   `(tree-sitter-hl-face:operator           ((t (:foreground ,grey))))
   `(tree-sitter-hl-face:function.call      ((t (:foreground ,blue2))))
   `(tree-sitter-hl-face:variable.parameter ((t (:foreground ,functions))))
   `(tree-sitter-hl-face:variable.special   ((t (:foreground ,constants))))
   `(tree-sitter-hl-face:number             ((t (:foreground ,numbers))))

   ;;;; org mode
   `(org-archived                 ((t (:foreground ,doc-comments))))
   `(org-block                    ((t (:background ,code-bg :foreground ,fg-alt))))
   `(org-block-begin-line         ((t (:inherit 'org-block  :foreground ,comments))))
   `(org-block-end-line           ((t (:inherit 'org-block-begin-line))))
   `(org-done                     ((t (:foreground ,comments))))
   `(org-todo                     ((t (:foreground ,yellow))))
   `(org-headline-done            ((t (:inherit 'org-done))))
   `(org-checkbox                 ((t (:inherit 'org-todo))))
   `(org-checkbox-statistics-done ((t (:inherit 'org-done))))
   `(org-checkbox-statistics-todo ((t (:inherit 'org-todo))))
   `(org-code                     ((t (:foreground ,orange))))
   `(org-meta-line                ((t (:foreground ,doc-comments))))
   `(org-document-info            ((t (:foreground ,orange))))
   `(org-document-info-keyword    ((t (:foreground ,doc-comments))))
   `(org-document-title           ((t (:foreground ,builtin :weight bold))))
   `(org-drawer                   ((t (:foreground ,comments))))
   `(org-footnote                 ((t (:foreground ,orange))))
   `(org-link                     ((t (:inherit 'link :foreground ,highlight))))
   `(org-priority                 ((t (:foreground ,red))))
   `(org-property-value           ((t (:foreground ,doc-comments))))
   `(org-quote                    ((t (:inherit 'org-block :slant italic))))
   `(org-table                    ((t (:foreground ,violet))))
   `(org-tag                      ((t (:foreground ,doc-comments :weight normal))))
   `(org-verbatim                 ((t (:foreground ,green))))
   ;;;; org-level-*
   `(outline-1 ((t (:foreground ,blue :weight bold :extend t))))
   `(outline-2 ((t (:inherit 'outline-1 :foreground ,violet))))
   `(outline-3 ((t (:inherit 'outline-1 :foreground ,(lighten violet 0.35)))))
   `(outline-4 ((t (:inherit 'outline-1 :foreground ,(lighten magenta 0.35)))))
   `(outline-5 ((t (:inherit 'outline-1 :foreground ,(lighten violet 0.6)))))
   `(outline-6 ((t (:inherit 'outline-1 :foreground ,(lighten magenta 0.6)))))
   `(outline-7 ((t (:inherit 'outline-1 :foreground ,(lighten violet 0.85)))))
   `(outline-8 ((t (:inherit 'outline-1 :foreground ,(lighten magenta 0.85)))))

   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'aura)
;;; aura-theme.el ends here

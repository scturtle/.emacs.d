;;; aura-theme.el --- aura theme -*- lexical-binding: t; -*-
;;
;; Author: scturtle <sctuetle@gmail.com>
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

(let*
    ((bg      "#21202e")
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

     (faces
      `(
        ;; basics
        (default   :background ,bg :foreground ,fg)
        (cursor    :background ,fg)
        (region    :background ,region)
        (highlight :foreground ,highlight :background ,bg-alt)
        (hl-line   :background ,bg-alt)
        (link      :foreground ,highlight :underline t :weight bold)
        (fringe    :foreground ,inactive-fg :background ,bg)
        (tooltip   :foreground ,fg :background ,bg-alt)
        (match     :foreground ,fg :background ,selection-bg)
        (shadow    :foreground ,grey)
        (error     :foreground ,error)
        (warning   :foreground ,warning)
        (success   :foreground ,success)
        (mode-line           :background ,bg-alt :foreground ,fg-alt)
        (mode-line-inactive  :background ,bg :foreground ,inactive-fg)
        (escape-glyph        :foreground ,cyan)
        (vertical-border     :background ,bg-alt :foreground ,bg-alt)
        (minibuffer-prompt   :foreground ,highlight)
        (trailing-whitespace :foreground ,fg :background ,red)

        ;; font-lock
        (font-lock-comment-face           :foreground ,comments)
        (font-lock-comment-delimiter-face :inherit font-lock-comment-face)
        (font-lock-string-face            :foreground ,strings)
        (font-lock-doc-face               :foreground ,doc-comments)
        ;; font-lock-doc-markup-face
        (font-lock-keyword-face           :foreground ,keywords)
        (font-lock-builtin-face           :foreground ,builtin)
        (font-lock-function-name-face     :foreground ,functions)
        (font-lock-function-call-face     :foreground ,blue2)
        (font-lock-variable-name-face     :foreground ,variables)
        (font-lock-variable-use-face      :foreground unspecified)
        (font-lock-type-face              :foreground ,type)
        (font-lock-constant-face          :foreground ,constants)
        (font-lock-warning-face           :inherit warning)
        (font-lock-negation-char-face     :weight bold :foreground ,operators)
        (font-lock-preprocessor-face      :weight bold :foreground ,operators)
        ;; font-lock-regexp-face
        ;; font-lock-regexp-grouping-backslash
        ;; font-lock-regexp-grouping-construct
        ;; font-lock-escape-face
        (font-lock-number-face            :foreground ,numbers)
        (font-lock-operator-face          :foreground ,grey)
        (font-lock-property-name-face     :foreground ,blue2)
        (font-lock-property-use-face      :foreground ,blue2)
        (font-lock-punctuation-face       :foreground ,grey)
        ;; font-lock-bracket-face
        ;; font-lock-delimiter-face
        ;; font-lock-misc-punctuation-face

        ;; line-number
        (line-number :foreground ,comments :background ,bg :italic t)
        (line-number-current-line :inherit line-number :foreground ,fg-alt)

        ;; tab-bar
        (tab-bar              :background ,bg-alt :foreground ,bg-alt)
        (tab-bar-tab          :background ,bg :foreground ,fg)
        (tab-bar-tab-inactive :background ,bg-alt :foreground ,inactive-fg)

        ;; ansi-color
        (ansi-color-black          :foreground ,bg      :background ,bg)
        (ansi-color-red            :foreground ,red     :background ,red)
        (ansi-color-green          :foreground ,green   :background ,green)
        (ansi-color-yellow         :foreground ,yellow  :background ,yellow)
        (ansi-color-blue           :foreground ,blue    :background ,blue)
        (ansi-color-magenta        :foreground ,magenta :background ,magenta)
        (ansi-color-cyan           :foreground ,cyan    :background ,cyan)
        (ansi-color-white          :foreground ,fg      :background ,fg)
        (ansi-color-bright-black   :foreground ,bg      :background ,bg)
        (ansi-color-bright-red     :foreground ,red     :background ,red)
        (ansi-color-bright-green   :foreground ,green   :background ,green)
        (ansi-color-bright-yellow  :foreground ,yellow  :background ,yellow)
        (ansi-color-bright-blue    :foreground ,blue    :background ,blue)
        (ansi-color-bright-magenta :foreground ,magenta :background ,magenta)
        (ansi-color-bright-cyan    :foreground ,cyan    :background ,cyan)
        (ansi-color-bright-white   :foreground ,fg      :background ,fg)

        ;; custom
        (custom-variable-tag :foreground ,violet)
        (custom-group-tag    :foreground ,violet)
        (custom-state        :foreground ,green)
        (widget-field :background ,region :extend t)

        ;; dired
        (dired-directory :foreground ,blue)
        (dired-symlink   :foreground ,constants)

        ;; eshell
        (eshell-prompt        :foreground ,highlight)
        (eshell-ls-archive    :foreground ,magenta)
        (eshell-ls-backup     :foreground ,yellow)
        (eshell-ls-clutter    :foreground ,red)
        (eshell-ls-directory  :foreground ,blue)
        (eshell-ls-executable :foreground ,green)
        (eshell-ls-missing    :foreground ,red)
        (eshell-ls-product    :foreground ,orange)
        (eshell-ls-readonly   :foreground ,orange)
        (eshell-ls-special    :foreground ,violet)
        (eshell-ls-symlink    :foreground ,cyan)
        (eshell-ls-unreadable :foreground ,comments)

        ;; search
        (lazy-highlight   :background ,(darken highlight 0.4) :foreground ,fg :weight bold)
        (isearch          :inherit lazy-highlight)
        (isearch-fail     :background ,red :foreground ,fg :weight bold)
        (iedit-occurrence :foreground ,magenta :weight bold :inverse-video t)

        ;; evil search
        (evil-ex-info                   :foreground ,error :slant italic)
        (evil-ex-search                 :background ,highlight :foreground ,fg :weight bold)
        (evil-ex-lazy-highlight         :inherit lazy-highlight)
        (evil-ex-substitute-matches     :background ,bg-alt :foreground ,red   :weight bold :strike-through t)
        (evil-ex-substitute-replacement :background ,bg-alt :foreground ,green :weight bold)

        ;; flycheck
        (flycheck-error   :foreground ,red :underline t)
        (flycheck-warning :foreground ,yellow :underline t)
        (flycheck-info    :foreground ,green :underline t)

        ;; diff-hl
        (diff-hl-change :foreground ,vc-modified :background ,vc-modified)
        (diff-hl-delete :foreground ,vc-deleted  :background ,vc-deleted)
        (diff-hl-insert :foreground ,vc-added    :background ,vc-added)

        ;; which-key
        (which-key-key-face                   :foreground ,green)
        (which-key-group-description-face     :foreground ,blue)
        (which-key-command-description-face   :foreground ,violet)
        (which-key-local-map-description-face :foreground ,orange)
        (help-key-binding  :foreground ,blue1 :background unspecified)

        ;; show-paren
        (show-paren-match    :background ,blue2 :foreground ,fg)
        (show-paren-mismatch :background ,red   :foreground ,fg)

        ;; vertico
        (vertico-current         :background ,region :extend t)
        (vertico-group-title     :foreground ,comments)
        (vertico-group-separator :foreground ,comments :strike-through t)

        ;; orderless
        (orderless-match-face-0 :foreground ,(blend blue3  fg 0.6) :background ,(blend blue3  bg 0.1))
        (orderless-match-face-1 :foreground ,(blend green  fg 0.6) :background ,(blend green  bg 0.1))
        (orderless-match-face-2 :foreground ,(blend violet fg 0.6) :background ,(blend violet bg 0.1))
        (orderless-match-face-3 :foreground ,(blend yellow fg 0.6) :background ,(blend yellow bg 0.1))

        ;; marginalia
        (marginalia-documentation   :foreground ,doc-comments)
        (marginalia-size            :foreground ,blue)
        (marginalia-date            :foreground ,violet)
        (marginalia-file-priv-dir   :foreground ,blue)
        (marginalia-file-priv-exec  :foreground ,green)
        (marginalia-file-priv-link  :foreground ,violet)
        (marginalia-file-priv-other :foreground ,magenta)
        (marginalia-file-priv-rare  :foreground ,fg)
        (marginalia-file-priv-read  :foreground ,yellow)
        (marginalia-file-priv-write :foreground ,red)

        ;; magit section
        (magit-section-heading           :foreground ,blue :weight bold :extend t)
        (magit-section-heading-selection :foreground ,orange :weight bold :extend t)
        (magit-section-highlight         :inherit hl-line)
        (magit-section-secondary-heading :foreground ,violet :weight bold :extend t)
        ;; magit diff
        (magit-diff-added             :foreground ,(darken vc-added 0.2) :background ,(blend vc-added bg 0.1) :extend t)
        (magit-diff-added-highlight   :foreground ,vc-added :background ,(blend vc-added bg 0.2) :weight bold :extend t)
        (magit-diff-removed           :foreground ,(darken vc-deleted 0.2) :background ,(blend vc-deleted bg 0.1) :extend t)
        (magit-diff-removed-highlight :foreground ,vc-deleted :background ,(blend vc-deleted bg 0.2) :weight bold :extend t)
        (magit-diff-base              :foreground ,(darken orange 0.2) :background ,(blend orange bg 0.1) :extend t)
        (magit-diff-base-highlight    :foreground ,orange :background ,(blend orange bg 0.2) :weight bold :extend t)
        (magit-diff-context           :foreground ,(darken fg 0.4) :background ,bg :extend t)
        (magit-diff-context-highlight :foreground ,fg :background ,bg-alt :extend t)
        (magit-diff-hunk-heading           :foreground ,fg-alt :background ,(darken blue3 0.3) :extend t)
        (magit-diff-hunk-heading-highlight :foreground ,fg     :background ,blue3 :weight bold :extend t)
        (magit-diff-file-heading           :foreground ,fg :weight bold :extend t)
        (magit-diff-file-heading-selection :foreground ,orange :weight bold :extend t)
        (magit-diff-lines-heading :inherit magit-diff-hunk-heading-highlight)
        (magit-diffstat-added   :foreground ,vc-added)
        (magit-diffstat-removed :foreground ,vc-deleted)
        ;; magit bisect/blame/branch
        (magit-bisect-bad     :foreground ,red)
        (magit-bisect-good    :foreground ,green)
        (magit-bisect-skip    :foreground ,orange)
        (magit-blame-hash     :foreground ,comments)
        (magit-blame-date     :foreground ,yellow)
        (magit-blame-heading  :foreground ,yellow :background ,region :extend t)
        (magit-branch-current :foreground ,blue)
        (magit-branch-local   :foreground ,cyan)
        (magit-branch-remote  :foreground ,green)
        ;; magit log
        (magit-header-line :background ,bg :foreground ,blue :underline nil :weight bold)
        (magit-tag         :foreground ,yellow)
        (magit-hash        :foreground ,comments)
        (magit-filename    :foreground ,fg)
        (magit-log-author  :foreground ,blue)
        (magit-log-date    :foreground ,violet)
        (magit-log-graph   :foreground ,comments)
        (magit-process-ng  :inherit error)
        (magit-process-ok  :inherit success)
        ;; TODO: magit cherry dimmed reflog sequence signature

        ;; smerge-mode
        (smerge-upper           :background ,(blend vc-deleted bg 0.1) :extend t)
        (smerge-lower           :background ,(blend vc-added bg 0.1) :extend t)
        (smerge-base            :background ,(blend orange bg 0.1) :extend t)
        (smerge-markers         :background ,(darken blue3 0.3) :extend t)

        ;; neotree
        (neo-root-dir-face      :foreground ,green)
        (neo-file-link-face     :foreground ,constants)
        (neo-dir-link-face      :foreground ,blue)
        (neo-expand-btn-face    :foreground ,blue)
        (neo-vc-edited-face     :foreground ,yellow)
        (neo-vc-added-face      :foreground ,green)
        (neo-vc-removed-face    :foreground ,red :strike-through t)
        (neo-vc-conflict-face   :foreground ,magenta :weight bold)
        (neo-vc-ignored-face    :foreground ,comments)
        (neo-vc-up-to-date-face :foreground ,grey)

        ;; lsp-mode
        (lsp-face-highlight-textual :background ,selection-bg :foreground ,fg)
        (lsp-face-highlight-read    :inherit lsp-face-highlight-textual)
        (lsp-face-highlight-write   :inherit lsp-face-highlight-textual)
        ;; lsp-ui-peek
        (lsp-ui-peek-filename    :foreground ,yellow)
        (lsp-ui-peek-header      :foreground ,fg :background ,blue3)
        (lsp-ui-peek-selection   :foreground ,fg :background ,selection-bg)
        (lsp-ui-peek-list        :background ,bg-alt)
        (lsp-ui-peek-peek        :background ,bg-alt)
        (lsp-ui-peek-highlight   :foreground ,fg-alt :background ,selection-bg)
        (lsp-ui-peek-line-number :inherit line-number)

        ;; corfu
        (corfu-default :inherit tooltip)
        (corfu-current :background ,selection-bg :foreground ,fg)

        ;; org mode
        (org-archived                 :foreground ,doc-comments)
        (org-block                    :background ,code-bg :foreground ,fg-alt)
        (org-block-begin-line         :inherit org-block  :foreground ,comments)
        (org-block-end-line           :inherit org-block-begin-line)
        (org-done                     :foreground ,comments)
        (org-todo                     :foreground ,yellow)
        (org-headline-done            :inherit org-done)
        (org-checkbox                 :inherit org-todo)
        (org-checkbox-statistics-done :inherit org-done)
        (org-checkbox-statistics-todo :inherit org-todo)
        (org-code                     :foreground ,orange)
        (org-meta-line                :foreground ,doc-comments)
        (org-document-info            :foreground ,orange)
        (org-document-info-keyword    :foreground ,doc-comments)
        (org-document-title           :foreground ,builtin :weight bold)
        (org-drawer                   :foreground ,comments)
        (org-footnote                 :foreground ,orange)
        (org-link                     :inherit link :foreground ,highlight)
        (org-priority                 :foreground ,red)
        (org-property-value           :foreground ,doc-comments)
        (org-quote                    :inherit org-block :slant italic)
        (org-table                    :foreground ,violet)
        (org-tag                      :foreground ,doc-comments :weight normal)
        (org-verbatim                 :foreground ,green)
        ;; org-level-*
        (outline-1 :foreground ,blue :weight bold :extend t)
        (outline-2 :inherit outline-1 :foreground ,violet)
        (outline-3 :inherit outline-1 :foreground ,(lighten violet 0.35))
        (outline-4 :inherit outline-1 :foreground ,(lighten magenta 0.35))
        (outline-5 :inherit outline-1 :foreground ,(lighten violet 0.6))
        (outline-6 :inherit outline-1 :foreground ,(lighten magenta 0.6))
        (outline-7 :inherit outline-1 :foreground ,(lighten violet 0.85))
        (outline-8 :inherit outline-1 :foreground ,(lighten magenta 0.85))

        )))

  (apply #'custom-theme-set-faces
         'aura
         (mapcar (lambda (face) `(,(car face) ((t ,(cdr face))))) faces))
  )


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'aura)
;;; aura-theme.el ends here

;;; doom-aura-theme.el --- inspired by the aura theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Date: November 11, 2022
;; Author: scturtle <https://github.com/scturtle>
;; Maintainer: scturtle <https://github.com/scturtle>
;; Source: https://github.com/daltonmenezes/aura-theme
;;
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-aura-theme nil
  "Options for the `doom-aura' theme."
  :group 'doom-themes)

;;
;;; Theme definition

(def-doom-theme doom-aura
  "A dark theme based on aura theme."

  ((bg         '("#21202e"))
   (bg-alt     '("#1c1b27")) ;; hl-line
   (base0      '("#1c1b27"))
   (base1      '("#21202e"))
   (base2      '("#373844"))
   (base3      '("#44475a"))
   (base4      '("#565761"))
   (base5      '("#6272a4")) ;; line-number, comments
   (base6      '("#b6b6b2"))
   (base7      '("#ccccc7"))
   (base8      '("#edecee"))
   (fg         '("#edecee"))
   (fg-alt     '("#e1e0e2"))

   (grey       base4)
   (red        '("#ff6767"))
   (yellow     '("#ffca85"))
   (orange     '("#ffca85"))
   (green      '("#61ffca"))
   (teal       '("#61ffca"))
   (blue       '("#a277ff"))
   (dark-blue  '("#a277ff"))
   (magenta    '("#a277ff"))
   (violet     '("#f694ff"))
   (cyan       '("#61ffca"))
   (dark-cyan  '("#61ffca"))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   bg-alt)
   (selection      blue)
   (builtin        blue)
   (comments       base5)
   (doc-comments   (doom-lighten base5 0.25))
   (constants      "#82e2ff") ;; light blue
   (functions      orange)
   (keywords       blue)
   (methods        orange)
   (operators      blue) ;; include,define
   (type           blue)
   (strings        green)
   (variables      violet)
   (numbers        green)
   (region         base2)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; my handy variables
   (code-bg        (doom-darken base1 0.125))
   (selection-bg   (doom-blend dark-blue bg 0.3))
   )

  ;;;; Base theme face overrides
  (
   ;;;; modeline
   (mode-line :background bg-alt :foreground fg-alt)
   (mode-line-inactive :background bg :foreground base4)
   ;;;; tree-sitter
   (tree-sitter-hl-face:property :inhert nil :foreground "#7e7edd")
   (tree-sitter-hl-face:punctuation :foreground "#B4B4B4")
   (tree-sitter-hl-face:operator :foreground "#B4B4B4")
   (tree-sitter-hl-face:function.call :foreground "#7e7edd") ;; ariake
   (tree-sitter-hl-face:variable.parameter :foreground functions)
   (tree-sitter-hl-face:variable.special :foreground constants)
   (tree-sitter-hl-face:number :foreground green)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground magenta)
   (outline-2 :inherit 'outline-1 :foreground violet)
   (outline-3 :inherit 'outline-2 :foreground (doom-lighten violet 0.35))
   (outline-4 :inherit 'outline-3 :foreground (doom-lighten magenta 0.35))
   (outline-5 :inherit 'outline-4 :foreground (doom-lighten violet 0.6))
   (outline-6 :inherit 'outline-5 :foreground (doom-lighten magenta 0.6))
   (outline-7 :inherit 'outline-6 :foreground (doom-lighten violet 0.85))
   (outline-8 :inherit 'outline-7 :foreground (doom-lighten magenta 0.85))
   ;;;; org <built-in>
   ((org-block &override) :background code-bg :foreground base7)
   ((org-block-begin-line &override) :background code-bg :foreground comments)
   ((org-code &override) :foreground yellow)
   (org-todo :foreground orange :bold 'inherit)
   ((org-link &override) :foreground orange)
   ;;; markdown
   ((markdown-code-face &override) :background code-bg)
   ;;; tabbar
   ((tab-bar-tab-inactive &override) :foreground base4)
   ;;; lsp-mode
   (lsp-ui-peek-selection :forground fg :background selection-bg)
   (lsp-ui-peek-highlight :forground fg-alt :background selection-bg)
   ;;; corfu
   ((corfu-current &override) :background selection-bg)
   ;;; neotree
   (neo-vc-up-to-date-face :foreground base6)
   ;;; diff-mode
   ((diff-removed &override) :background code-bg)
   ((diff-added &override) :background code-bg)
   ;;; table.el
   (table-cell :foreground fg :background code-bg)
   )

  ;;;; Base theme variable overrides-
  ()
  )

;;; doom-aura-theme.el ends here

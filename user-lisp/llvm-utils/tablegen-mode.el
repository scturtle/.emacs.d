;;; tablegen-mode.el --- Major mode for TableGen description files (part of LLVM project) -*- lexical-binding: t -*-

;; Maintainer:  The LLVM team, http://llvm.org/
;; Version: 2.0

;;; Commentary:
;; A major mode for TableGen description files in LLVM.
;; Covers the full TableGen language per the LLVM ProgRef:
;;   https://llvm.org/docs/TableGen/ProgRef.html

;;; Code:

;; ---------------------- Font-lock ----------------------------

(defvar tablegen-keywords
  '("assert" "class" "def" "defm" "defset" "deftype" "defvar"
    "dump" "else" "field" "foreach" "if" "in" "include"
    "let" "multiclass" "then")
  "TableGen statement keywords.")

(defvar tablegen-builtin-constants
  '("true" "false")
  "TableGen boolean literals.")

(defvar tablegen-type-keywords
  '("bit" "bits" "code" "dag" "int" "list" "string")
  "TableGen type keywords.")

;; Context-sensitive keywords (only meaningful after `let', but highlighting
;; them everywhere avoids complex parser state and is harmless in practice.)
(defvar tablegen-context-keywords
  '("append" "prepend")
  "TableGen context-sensitive keywords (let modifiers).")

;; Every bang operator from Appendix A of the ProgRef, plus !cond.
(defvar tablegen-bang-operators
  '("!add" "!and" "!cast" "!con" "!cond" "!dag" "!div"
    "!empty" "!eq" "!exists" "!filter" "!find" "!foldl" "!foreach"
    "!ge" "!getdagarg" "!getdagname" "!getdagop" "!getdagopname"
    "!gt" "!head" "!if" "!initialized" "!instances" "!interleave"
    "!isa" "!le" "!listconcat" "!listflatten" "!listremove"
    "!listsplat" "!logtwo" "!lt" "!match" "!mul" "!ne" "!not"
    "!or" "!range" "!repr" "!setdagarg" "!setdagname" "!setdagop"
    "!setdagopname" "!shl" "!size" "!sra" "!srl" "!strconcat"
    "!sub" "!subst" "!substr" "!tail" "!tolower" "!toupper" "!xor")
  "TableGen bang operators.")

(defvar tablegen-font-lock-keywords
  (let ((kw       (concat "\\_<" (regexp-opt tablegen-keywords) "\\_>"))
        (builtins (concat "\\_<" (regexp-opt tablegen-builtin-constants) "\\_>"))
        (type-kw  (concat "\\_<" (regexp-opt tablegen-type-keywords) "\\_>"))
        (ctx-kw   (concat "\\_<" (regexp-opt tablegen-context-keywords) "\\_>"))
        (bang-re  (concat (regexp-opt tablegen-bang-operators) "\\_>")))
    (list
     ;; Preprocessor directives: #define, #ifdef, #ifndef, #else, #endif
     '("^[ \t]*\\(#define\\|#ifdef\\|#ifndef\\|#else\\|#endif\\)\\b" 1 font-lock-preprocessor-face)
     ;; Decorators / attributes (@name at start of line)
     '("^[ \t]*\\(@[A-Za-z_][A-Za-z0-9_]*\\)" 1 font-lock-preprocessor-face)
     ;; DAG variable names ($varname)
     '("\\$[A-Za-z_][A-Za-z0-9_]*" . font-lock-variable-name-face)
     ;; Bang operators  (!add, !cond, etc.)
     (cons bang-re 'font-lock-builtin-face)
     ;; Boolean constants
     (cons builtins 'font-lock-constant-face)
     ;; Context-sensitive keywords (append / prepend)
     (cons ctx-kw 'font-lock-keyword-face)
     ;; Statement keywords
     (cons kw 'font-lock-keyword-face)
     ;; Type keywords
     (cons type-kw 'font-lock-type-face)
     ;; Hex integer literals  0x…
     '("\\_<0x[0-9A-Fa-f]+\\_>" . font-lock-constant-face)
     ;; Binary integer literals  0b…
     '("\\_<0b[01]+\\_>" . font-lock-constant-face)
     ;; Decimal integer literals  [-+]?digits
     '("\\(?:^\\|[^A-Za-z0-9_$]\\)\\([-+]?[0-9]+\\)\\_>" 1 font-lock-constant-face)
     ;; Uninitialized value placeholder  ?
     '("\\?" . font-lock-constant-face)))
  "Font-lock rules for TableGen mode.")

(defconst tablegen-syntax-propertize-function
  (syntax-propertize-rules
   ;; TokCode literals:[{ ... }] (multi-line raw strings)
   ;; We mark '[' and ']' with the generic string fence syntax '|'.
   ("\\(\\[\\){\\(?:.\\|\n\\)*?}\\(\\]\\)"
    (1 "|")
    (2 "|")))
  "Syntax propertize function for TableGen.")

;; ---------------------- Syntax table ---------------------------

(defvar tablegen-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Word constituents
    (modify-syntax-entry ?_  "w"      table)
    ;; Punctuation
    (modify-syntax-entry ??  "."      table)
    (modify-syntax-entry ?#  "."      table)
    (modify-syntax-entry ?!  "."      table)
    (modify-syntax-entry ?$  "_"      table) ; DAG vars begin with $, map it to symbol
    ;; Comments: // line comments and /* ... */ block comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    ;; Paired delimiters
    (modify-syntax-entry ?\( "()"     table)
    (modify-syntax-entry ?\) ")("     table)
    (modify-syntax-entry ?\[ "(]"     table)
    (modify-syntax-entry ?\] ")["     table)
    (modify-syntax-entry ?\{ "(}"     table)
    (modify-syntax-entry ?\} "){"     table)
    (modify-syntax-entry ?\< "(>"     table)
    (modify-syntax-entry ?\> ")<"     table)
    ;; String delimiter
    (modify-syntax-entry ?\" "\""     table)
    table)
  "Syntax table used in `tablegen-mode' buffers.")

;; --------------------- Abbrev table -----------------------------

(define-abbrev-table 'tablegen-mode-abbrev-table ()
  "Abbrev table used while in TableGen mode.")

;; ----------------------- Indentation ----------------------------

(defcustom tablegen-indent-offset 2
  "Number of spaces per indentation level in `tablegen-mode'."
  :type 'integer
  :group 'tablegen)

(defun tablegen-indent-line ()
  "Indent the current line of TableGen source."
  (interactive)
  (let ((indent (tablegen--current-indent)))
    (when indent
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion (indent-line-to indent))))))

(defun tablegen--current-indent ()
  "Return the proper indentation column for the current line."
  (save-excursion
    (beginning-of-line)
    (let* ((ppss (syntax-ppss))
           (depth (nth 0 ppss)))
      ;; Decrease indentation depth if current line starts with a closing delimiter.
      (when (looking-at-p "[ \t]*[\\]})>]")
        (setq depth (max 0 (1- depth))))
      (* tablegen-indent-offset depth))))

;; ----------------------- Mode definition ------------------------

(defgroup tablegen nil
  "Major mode for editing TableGen source files."
  :group 'languages
  :prefix "tablegen-")

;;;###autoload
(define-derived-mode tablegen-mode prog-mode "TableGen"
  "Major mode for editing TableGen description files.

Covers the full TableGen language:
  keywords, type keywords, bang operators (!add, !cond, …),
  boolean literals, numeric/hex/binary constants, DAG $variables,
  TokCode [{ … }] literals, and preprocessor directives.

\\{tablegen-mode-map}
Runs `tablegen-mode-hook' on startup."
  (set-syntax-table tablegen-mode-syntax-table)
  (setq-local syntax-propertize-function tablegen-syntax-propertize-function)
  (setq font-lock-defaults
        '(tablegen-font-lock-keywords
          nil   ; keywords-only: no — also fontify strings/comments
          nil   ; case-fold: TableGen is case-sensitive
          nil   ; syntax-alist
          nil)) ; syntax-begin
  (setq-local comment-start      "// ")
  (setq-local comment-end        "")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local indent-line-function #'tablegen-indent-line)
  (setq-local indent-tabs-mode   nil)
  (setq-local local-abbrev-table tablegen-mode-abbrev-table)
  (setq-local require-final-newline t))

;; Associate .td files with tablegen-mode
;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.td\\'") 'tablegen-mode))

(provide 'tablegen-mode)

;;; tablegen-mode.el ends here

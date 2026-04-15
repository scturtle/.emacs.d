;;; mlir-mode.el --- Major mode for editing MLIR -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; Keywords: languages, mlir, compiler

;;; Commentary:
;;
;; Basic major mode for MLIR (Multi-Level Intermediate Representation).
;; Provides syntax highlighting based on the MLIR grammar definition.

;;; Code:

(defgroup mlir nil
  "Major mode for editing MLIR."
  :prefix "mlir-"
  :group 'languages)

(defconst mlir-keywords
  '("ins" "outs" "else" "do" "attributes" "into" "to" "from"
    "step" "low" "high" "iter_args" "padding_value" "inner_tiles"
    "gather_dims" "scatter_dims" "outer_dims_perm" "inner_dims_pos"
    "shared_outs" "default" "br" "ceildiv" "floordiv" "func" "cond_br" "true" "false" "unit" "mod" "size"
    "module" "unrealized_conversion_cast" "func.func" "return" "llvm.func" "llvm.return"
    "scf.if" "scf.for" "scf.forall" "scf.parallel" "scf.while" "scf.yield"
    "memref.alloc" "memref.cast" "memref.copy" "memref.collapse_shape" "memref.expand_shape"
    "memref.dim" "memref.rank" "memref.load" "memref.store" "memref.view" "memref.subview" "memref.reshape" "memref.transpose"
    )
  "List of MLIR keywords.")

(defconst mlir-font-lock-keywords
  `((,(regexp-opt mlir-keywords 'symbols) . font-lock-builtin-face)

    ("=[ \t]*\\([a-zA-Z][a-zA-Z0-9$._-]*\\)" 1 font-lock-function-name-face)
    ("^[ \t]*\\([a-zA-Z][a-zA-Z0-9$._-]*\\)" 1 font-lock-function-name-face)

    ;; Types (!...)
    ("\\(![a-zA-Z_][a-zA-Z0-9_$.]*\\)" 1 font-lock-type-face)

    ;; Built-in Types (i32, f32, tensor, memref, etc.)
    ("\\_<\\(i[1-9][0-9]*\\|f16\\|bf16\\|f32\\|f64\\|f8E4M3FN\\|f8E5M2\\|index\\|tensor\\|memref\\|vector\\|complex\\|tuple\\|none\\)\\_>" . font-lock-type-face)

    ;; Variables / Registers (%...)
    ("\\(%[a-zA-Z0-9_#$.-]+\\)" 1 font-lock-keyword-face)

    ;; Symbols / Functions (@...)
    ("\\(@[a-zA-Z_][a-zA-Z0-9_$.-]*\\)" 1 font-lock-function-name-face)

    ;; Block Labels (^...)
    ("\\(\\^[a-zA-Z0-9_.-]+\\)" 1 font-lock-constant-face)

    ;; Locations (loc(#loc[0-9]))
    ("loc(#loc[0-9]*)" . font-lock-comment-face)

    ;; Attributes (#...)
    ("\\(#[a-zA-Z_][a-zA-Z0-9_$.]*\\)" 1 font-lock-comment-face)

    ;; Numeric Literals (Integers and Floats)
    ("\\_<[-+]?[0-9]*\\.?[0-9]+\\([eE][-+]?[0-9]+\\)?\\_>" . font-lock-number-face)
    ("\\_<0x[0-9a-fA-F]+\\_>" . font-lock-number-face))

  "Font-lock definitions for `mlir-mode`.")

(defvar mlir-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C++ style comments `// ...`
    (modify-syntax-entry ?/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)

    ;; Strings `"..."`
    (modify-syntax-entry ?\" "\"" table)

    ;; Word/Symbol constituents
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?. "_" table) ; Dot is considered part of a symbol (e.g., func.call)
    (modify-syntax-entry ?- "_" table)

    ;; Punctuation / Brackets
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)

    ;; Special MLIR Sigils (prefix chars)
    (modify-syntax-entry ?% "'" table)
    (modify-syntax-entry ?@ "'" table)
    (modify-syntax-entry ?^ "'" table)
    (modify-syntax-entry ?! "'" table)
    (modify-syntax-entry ?# "'" table)

    table)
  "Syntax table for `mlir-mode`.")

;;;###autoload
(define-derived-mode mlir-mode prog-mode "MLIR"
  "Major mode for editing MLIR code.

\\{mlir-mode-map}"
  :syntax-table mlir-mode-syntax-table
  (setq-local font-lock-defaults '(mlir-font-lock-keywords))
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mlir\\'" . mlir-mode))

(provide 'mlir-mode)
;;; mlir-mode.el ends here

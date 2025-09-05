;;; -*- lexical-binding: t; -*-

;; lots of functions are stolen and modified from doomemacs

(defun +symbol-highlight (beg end)
  "Highlight current symbol. Restrict by evil visual region."
  (interactive
   (if (evil-visual-state-p)
       (prog1 (list evil-visual-beginning evil-visual-end)
         (evil-exit-visual-state))
     (list (point-min) (point-max))))
  (when-let* ((word (current-word t))
              (regexp (format "\\_<%s\\_>" (regexp-quote word))))
    (iedit-start regexp beg end)
    (evil-multiedit-mode +1)))

(defun +goto-definition ()
  (interactive)
  (cond
   ((bound-and-true-p eglot--managed-mode) (call-interactively #'lsp-ui-peek-find-definitions))
   ((eq major-mode 'emacs-lisp-mode) (call-interactively #'elisp-def))
   (t (call-interactively #'evil-goto-definition))))

(defun +search-project-for-symbol-at-point ()
  (interactive)
  (let ((symbol (if (evil-visual-state-p)
                    (prog1 (buffer-substring-no-properties evil-visual-beginning evil-visual-end)
                      (evil-exit-visual-state))
                  (or (thing-at-point 'symbol t) "")))
        (dir (or (projectile-acquire-root) default-directory)))
    (consult-ripgrep dir symbol)))

(defun +evil/shift-right ()
  (interactive)
  (call-interactively #'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))

(defun +evil/shift-left ()
  (interactive)
  (call-interactively #'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

(defun +magit-display-buffer-fn (buffer)
  (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
    (display-buffer
     buffer (cond
             ((and (eq buffer-mode 'magit-status-mode)
                   (get-buffer-window buffer))
              '(display-buffer-reuse-window))
             ;; Any magit buffers opened from a commit window should open below
             ;; it. Also open magit process windows below.
             ((or (bound-and-true-p git-commit-mode)
                  (eq buffer-mode 'magit-process-mode))
              (let ((size (if (eq buffer-mode 'magit-process-mode) 0.35 0.7)))
                `(display-buffer-below-selected
                  . ((window-height . ,(truncate (* (window-height) size)))))))
             ;; Everything else should reuse the current window.
             ((or (not (derived-mode-p 'magit-mode))
                  (not (memq (with-current-buffer buffer major-mode)
                             '(magit-process-mode
                               magit-revision-mode
                               magit-diff-mode
                               magit-stash-mode
                               magit-status-mode))))
              '(display-buffer-same-window))
             ('(+magit--display-buffer-in-direction))))))

(defun +magit--display-buffer-in-direction (buffer alist)
  (let ((direction (or (alist-get 'direction alist) 'right))
        (origin-window (selected-window)))
    (if-let* ((window (window-in-direction direction)))
        (select-window window)
      (if-let* ((window (and (not (one-window-p)) (window-in-direction 'left))))
          (select-window window)
        (let ((window (split-window nil nil direction)))
          (select-window window)
          (display-buffer-record-window 'reuse window buffer)
          (set-window-buffer window buffer)
          (set-window-parameter window 'quit-restore (list 'window 'window origin-window buffer))
          (set-window-prev-buffers window nil))))
    (switch-to-buffer buffer t t)
    (selected-window)))

(defun +neotree/find-this-file ()
  (interactive)
  (let ((path buffer-file-name)
        (project-root (or (projectile-acquire-root) default-directory)))
    (neotree-dir project-root)
    (neotree-find path project-root)
    (neotree-refresh)))

(defun +neotree/set-width ()
  (interactive)
  (let ((width (read-number "width: ")))
    (setq neo-window-width width)
    (neo-global--set-window-width width)))

(defun +colorize (color-list)
  (setq colors-alist (mapcar (lambda (l) (cons (symbol-name (car l)) (cadr l))) color-list))
  (setq font-lock-keywords `((,(regexp-opt (mapcar 'car colors-alist) 'words)
                              (0 (rainbow-colorize-by-assoc colors-alist)))))
  (font-lock-add-keywords nil font-lock-keywords t))

(defun +eglot-rust-hover-info (contents &optional _)
  (let* ((value (plist-get contents :value))
         (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
         (mod-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-third-item groups))
                          ((s-equals? "```rust" (car (-third-item groups))) (-first-item groups))
                          (t nil)))
         (cmt (if (null mod-group) "" (concat " // " (cadr mod-group))))
         (sig-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-fifth-item groups))
                          ((s-equals? "```rust" (car (-third-item groups))) (-third-item groups))
                          (t (-first-item groups))))
         (sig (->> sig-group
                   (--drop-while (s-starts-with? "```" it))
                   (--take-while (not (s-equals? "```" it)))
                   (--map (s-replace-regexp "//.*" "" it))
                   (--map (s-trim it))
                   (s-join " "))))
    (eglot--format-markup (concat "```rust\n" sig cmt "\n```"))))

;; ccls skipped ranges https://github.com/nemethf/eglot-x/pull/7
(cl-defmethod eglot-client-capabilities :around (_)
  (let ((base (cl-call-next-method)))
    (setf (cl-getf (cl-getf base :textDocument)
                   :inactiveRegionsCapabilities)
          '(:inactiveRegions t))
    base))

(cl-defmethod eglot-handle-notification
  (_server (_method (eql $ccls/publishSkippedRanges)) &key uri skippedRanges)
  (eglot-x--hide-inactive-regions uri skippedRanges))

(defun eglot-x--hide-inactive-regions (uri skipped-ranges)
  (when-let* ((buffer (or (find-buffer-visiting (eglot-uri-to-path uri))
                          (gethash uri eglot--temp-location-buffers))))
    (with-current-buffer
        buffer
      (remove-overlays nil nil 'eglot-x--inactive-code t)
      (mapc (lambda (range)
              (pcase-let*
                  ((`(,beg . ,end) (eglot-range-region range)))
                (let ((ov (make-overlay beg end buffer t nil)))
                  (overlay-put ov 'face 'font-lock-comment-face)
                  (overlay-put ov 'eglot--overlay t)
                  (overlay-put ov 'eglot-x--inactive-code t))))
            skipped-ranges))))

;; https://www.jamescherti.com/emacs-symbol-highlighting-built-in-functions
(defun hi-lock-symbol-at-point ()
  (interactive)
  (require 'hi-lock)
  (when-let* ((regexp (find-tag-default-as-symbol-regexp)))
    (if (member regexp (hi-lock--regexps-at-point))
        (hi-lock-unface-buffer regexp)
      (hi-lock-face-symbol-at-point))))

(defun highlight-codetags-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\(?:(.*)\\)?:?\\)\\>"  1 '(:inherit warning :weight bold) prepend)
         ("\\<\\(FIXME\\(?:(.*)\\)?:?\\)\\>" 1 '(:inherit error :weight bold) prepend)
         ("\\<\\(NOTE\\(?:(.*)\\)?:?\\)\\>"  1 '(:inherit success :weight bold) prepend))))

;; from elsa--worker-debugger
(defun +debugger (&rest args)
  (setq num-nonmacro-input-events (1+ num-nonmacro-input-events))
  (display-warning
   :debug
   (let ((frames nil)
         (in-program-stack nil))
     (dolist (frame (backtrace-frames))
       (when in-program-stack (push frame frames))
       (when (eq (elt frame 1) '+debugger) (setq in-program-stack t)))
     (concat
      (format "%s\n" args)
      (mapconcat
       (lambda (frame)
         (if (car frame)
             (format "  %S%s"
                     (cadr frame)
                     (if (nth 2 frame) (cl-prin1-to-string (nth 2 frame)) "()"))
           (format "  (%S %s)"
                   (cadr frame)
                   (mapconcat (lambda (x) (format "%S" x)) (nth 2 frame) " "))))
       (nreverse frames) "\n")))))

(provide 'funcs)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

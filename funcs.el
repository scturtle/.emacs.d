;;; -*- lexical-binding: t; -*-
;;; lots of functions are stolen and modified from doomemacs

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
   ((bound-and-true-p lsp-ui-mode) (call-interactively #'lsp-ui-peek-find-definitions))
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
    (if-let (window (window-in-direction direction))
        (select-window window)
      (if-let (window (and (not (one-window-p)) (window-in-direction 'left)))
          (select-window window)
        (let ((window (split-window nil nil direction)))
          (select-window window)
          (display-buffer-record-window 'reuse window buffer)
          (set-window-buffer window buffer)
          (set-window-parameter window 'quit-restore (list 'window 'window origin-window buffer))
          (set-window-prev-buffers window nil))))
    (switch-to-buffer buffer t t)
    (selected-window)))


;; https://github.com/emacs-lsp/lsp-mode/pull/2531
;; https://github.com/emacs-lsp/lsp-mode/issues/2375
(defun lsp-tramp-connection@override (local-command)
  (defvar tramp-connection-properties)
  (list :connect (lambda (filter sentinel name environment-fn _workspace)
                   (add-to-list 'tramp-connection-properties
                                (list (regexp-quote (file-remote-p default-directory))
                                      "direct-async-process" t))
                   (let* ((final-command (lsp-resolve-final-function local-command))
                          (process-name (generate-new-buffer-name name))
                          (stderr-buf (format "*%s::stderr*" process-name))
                          (process-environment (lsp--compute-process-environment environment-fn))
                          (proc (make-process
                                 :name process-name
                                 :buffer (format "*%s*" process-name)
                                 :command final-command
                                 :connection-type 'pipe
                                 :coding 'no-conversion
                                 :noquery t
                                 :filter filter
                                 :sentinel sentinel
                                 :stderr (generate-new-buffer stderr-buf)
                                 :file-handler t)))
                     (cons proc proc)))
        :test? (lambda () (-> local-command lsp-resolve-final-function lsp-server-present?))))

(defun +neotree/find-this-file ()
  (interactive)
  (let ((path buffer-file-name)
        (project-root (or (projectile-acquire-root) default-directory)))
    (neotree-dir project-root)
    (neotree-find path project-root)
    (neotree-refresh)))

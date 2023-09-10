;;; mudline.el --- A minimal mode-line inspired by doom-modeline and mood-line -*- lexical-binding: t; -*-

;; Author: scturtle <sctuetle@gmail.com>
;; Keywords: mode-line faces
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;;; Code:

(defvar flycheck-current-errors)

(declare-function flycheck-count-errors "flycheck" (errors))

(defgroup mudline nil
  ""
  :group 'mode-line)

(defface mudline-buffer-name
  '((t (:inherit (mode-line-buffer-id))))
  "Face used for major mode indicator in the mode-line."
  :group 'mudline)

(defface mudline-unimportant
  '((t (:inherit (shadow))))
  "Face used for less important mode-line elements."
  :group 'mudline)

(defface mudline-modified
  '((t (:inherit (error))))
  "Face used for the `modified' indicator symbol in the mode-line."
  :group 'mudline)

;;
;; Helper functions
;;

(defun mudline--format (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT,
aligned respectively."
  (let ((reserve (length right)))
    (concat left
            " "
            (propertize " "
                        'display `((space :align-to (- right ,reserve))))
            right)))

(defvar mudline--current-window (frame-selected-window)
  "Current window.")

(defun mudline--active ()
  (eq (frame-selected-window) mudline--current-window))

(defun mudline--set-selected-window (&rest _)
  (let ((win (frame-selected-window)))
    (setq mudline--current-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))

;;
;; Update functions
;;

(defvar-local mudline--flycheck-text nil)
(defun mudline--update-flycheck-segment (&optional status)
  "Update `mudline--flycheck-text' against the reported flycheck STATUS."
  (setq mudline--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (propertize (concat "⚑ Issues: " (number-to-string sum) "  ")
                                         'face (if .error 'error 'warning))))
                       (propertize "✔ Good  " 'face 'success)))
          ('running (propertize "Δ Checking  " 'face 'font-lock-keyword-face))
          ('errored (propertize "✖ Error  " 'face 'error))
          ('interrupted (propertize "⏸ Paused  " 'face 'shadow))
          ('no-checker ""))))

(defun mudline--icon (icon-set icon-name face)
  (let ((func (nerd-icons--function-name icon-set)))
    (concat (propertize (funcall func icon-name) 'face face) " ")))

;;
;; Segments
;;

(defun mudline-segment-evil-state ()
  "Segment to show Evil-mode state."
  (mudline--icon
   'mdicon
   (cond
    ((evil-normal-state-p)   "nf-md-alpha_n_box")
    ((evil-emacs-state-p)    "nf-md-alpha_e_box")
    ((evil-insert-state-p)   "nf-md-alpha_i_box")
    ((evil-motion-state-p)   "nf-md-alpha_m_box")
    ((evil-visual-state-p)   "nf-md-alpha_v_box")
    ((evil-operator-state-p) "nf-md-alpha_o_box")
    ((evil-replace-state-p)  "nf-md-alpha_r_box")
    (t "nf-md-alpha_x_circle"))
   (cond
    ((evil-normal-state-p) 'font-lock-string-face)
    ((evil-insert-state-p) 'font-lock-keyword-face)
    ((evil-visual-state-p) 'font-lock-warning-face)
    (t                     'error))
   ))

(defun mudline-segment-recording ()
  (when (or defining-kbd-macro executing-kbd-macro)
    (mudline--icon 'mdicon "nf-md-record_circle" 'font-lock-keyword-face)))

(defun mudline-segment-iedit-count ()
  (when (and (bound-and-true-p iedit-mode)
             (bound-and-true-p iedit-occurrences-overlays))
    (let* ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (save-excursion (iedit-prev-occurrence)
                                        (iedit-find-current-occurrence-overlay))))
           (sorted (sort (append iedit-occurrences-overlays nil)
                         (lambda (a b) (< (overlay-start a) (overlay-start b)))))
           (index (length (memq this-oc sorted)))
           (total (length iedit-occurrences-overlays))
           (text (format "%s/%d " (if this-oc (- total index -1) "-") total)))
      (propertize text 'face 'font-lock-keyword-face))))

;; (defun mudline-segment-modified ()
;;   "Displays a color-coded buffer modification/read-only indicator in the mode-line."
;;   (if (not (string-match-p "\\*.*\\*" (buffer-name)))
;;       (if (buffer-modified-p)
;;           (propertize "● " 'face 'mudline-modified)
;;         (if (and buffer-read-only (buffer-file-name))
;;             (propertize "■ " 'face 'mudline-unimportant)
;;           "  "))
;;     "  "))

;; (defun mudline-segment-buffer-name ()
;;   "Displays the name of the current buffer in the mode-line."
;;   (propertize "%b  " 'face 'mudline-buffer-name))

(defvar-local mudline--buffer-file-icon nil)

(defun mudline-update-buffer-file-icon (&rest _)
  (setq mudline--buffer-file-icon
        (let ((icon (nerd-icons-icon-for-buffer)))
          (propertize icon 'help-echo (concat "Major-mode: " (format-mode-line mode-name))))))

(add-hook 'find-file-hook               #'mudline-update-buffer-file-icon)
(add-hook 'after-change-major-mode-hook #'mudline-update-buffer-file-icon)

(defun mudline-segment-major-mode-icon ()
  (when-let ((icon (or mudline--buffer-file-icon
                       (mudline-update-buffer-file-icon))))
    (unless (string-empty-p icon) (concat icon " "))))

(defun mudline-segment-buffer-state-icon ()
  (ignore-errors
    (concat
     (cond (buffer-read-only
            (mudline--icon 'mdicon "nf-md-lock" 'warning))
           ((and buffer-file-name (buffer-modified-p))
            (mudline--icon 'mdicon "nf-md-content_save_edit" 'warning))
           (t ""))
     (when (buffer-narrowed-p)
       (mudline--icon 'mdicon "nf-md-unfold_less_horizontal" 'warning))
     )))

(defun mudline-segment-position ()
  "Displays the current cursor position in the mode-line."
  (propertize "%l:%c %p%% " 'face 'shadow))

(defun mudline-segment-flycheck ()
  "Displays color-coded flycheck information in the mode-line (if available)."
  mudline--flycheck-text)

;; Store the default mode-line format
(defvar mudline--default-mode-line mode-line-format)

;;;###autoload
(define-minor-mode mudline-mode
  "Toggle mudline on or off."
  :group 'mudline
  :global t
  :lighter nil
  (if mudline-mode
      (progn

        (add-hook 'pre-redisplay-functions #'mudline--set-selected-window)

        ;; Setup flycheck hooks
        (add-hook 'flycheck-status-changed-functions #'mudline--update-flycheck-segment)
        (add-hook 'flycheck-mode-hook #'mudline--update-flycheck-segment)

        ;; Set the new mode-line-format
        (setq-default mode-line-format
                      '((:eval
                         (mudline--format
                          ;; Left
                          (format-mode-line
                           '(
                             (:eval (mudline-segment-evil-state))
                             (:eval (mudline-segment-recording))
                             (:eval (mudline-segment-iedit-count))
                             (:eval (mudline-segment-major-mode-icon))
                             (:eval (mudline-segment-buffer-state-icon))
                             ))

                          ;; Right
                          (format-mode-line
                           '(
                             (:eval (mudline-segment-flycheck))
                             (:eval (mudline-segment-position))
                             ))
                          )))))
    (progn

      ;; Remove flycheck hooks
      (remove-hook 'flycheck-status-changed-functions #'mudline--update-flycheck-segment)
      (remove-hook 'flycheck-mode-hook #'mudline--update-flycheck-segment)

      ;; Restore the original mode-line format
      (setq-default mode-line-format mudline--default-mode-line))))

(provide 'mudline)
;;; mudline.el ends here

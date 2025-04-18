;;; mudline.el --- A minimal mode-line inspired by doom-modeline and mood-line -*- lexical-binding: t; -*-

;; Author: scturtle <sctuetle@gmail.com>
;; Source: https://github.com/scturtle/.emacs.d
;; Keywords: mode-line
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;;; Code:

(defvar flycheck-current-errors)

(defgroup mudline nil
  ""
  :group 'mode-line)

(defun mudline--icon (icon-set icon-name face)
  (let ((func (nerd-icons--function-name icon-set)))
    (concat (propertize (funcall func icon-name) 'face face) " ")))

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
  "Segment to show macro recording status."
  (when (or defining-kbd-macro executing-kbd-macro)
    (concat
     (mudline--icon 'mdicon "nf-md-record_circle" 'font-lock-keyword-face)
     (propertize (format "@%s " (char-to-string evil-this-macro))
                 'face 'font-lock-keyword-face 'help-echo "macro recording")
     )))

(defun mudline-segment-iedit-count ()
  "Segment to show curent and total count of iedit selection."
  (when (and (bound-and-true-p iedit-mode)
             (bound-and-true-p iedit-occurrences-overlays))
    (let* ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (save-excursion (ignore-errors (iedit-prev-occurrence))
                                        (iedit-find-current-occurrence-overlay))))
           (sorted (sort (append iedit-occurrences-overlays nil)
                         (lambda (a b) (< (overlay-start a) (overlay-start b)))))
           (index (length (memq this-oc sorted)))
           (total (length iedit-occurrences-overlays))
           (text (format "%s/%d " (if this-oc (- total index -1) "-") total)))
      (propertize text 'face 'font-lock-keyword-face))))

(defvar-local mudline--buffer-file-icon nil)
(defun mudline-update-buffer-file-icon (&rest _)
  (setq mudline--buffer-file-icon
        (let ((icon (nerd-icons-icon-for-buffer)))
          (propertize icon 'help-echo (concat "Major-mode: " (format-mode-line mode-name))))))
(add-hook 'find-file-hook               #'mudline-update-buffer-file-icon)
(add-hook 'after-change-major-mode-hook #'mudline-update-buffer-file-icon)

(defun mudline-segment-major-mode-icon ()
  "Segment to show major mode as icon."
  (when-let* ((icon (or mudline--buffer-file-icon
                        (mudline-update-buffer-file-icon))))
    (unless (string-empty-p icon) (concat icon " "))))

(defun mudline-segment-buffer-state-icon ()
  "Segment to show buffer status (read-only or modified) as icon."
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

(defvar-local mudline--buffer-file-name nil)
(defun mudline-update-buffer-file-name (&rest _)
  (setq mudline--buffer-file-name
        (if buffer-file-name
            (let* ((buffer-file-name (file-local-name buffer-file-name))
                   (file-truename (file-local-name (file-truename buffer-file-name)))
                   (project-root (file-local-name (or (projectile-project-root) "")))
                   (relative-path (file-relative-name file-truename project-root)))
              (concat
               ;; project directory
               (propertize
                (concat (file-name-nondirectory (directory-file-name project-root))
                        (if (string-empty-p project-root) "" "/"))
                'face 'font-lock-string-face)
               ;; relative path
               (propertize relative-path 'face 'mode-line-buffer-id)))
          (propertize "%b" 'face 'mode-line-buffer-id))))
(add-hook 'find-file-hook                  #'mudline-update-buffer-file-name)
(add-hook 'after-save-hook                 #'mudline-update-buffer-file-name)
(advice-add #'rename-buffer         :after #'mudline-update-buffer-file-name)
(advice-add #'set-visited-file-name :after #'mudline-update-buffer-file-name)
(advice-add #'pop-to-buffer         :after #'mudline-update-buffer-file-name)

(defun mudline-segment-buffer-name ()
  "Segment to show current project and buffer name."
  (unless mudline--buffer-file-name
    (mudline-update-buffer-file-name))
  (if (buffer-modified-p)
      (propertize mudline--buffer-file-name 'face 'font-lock-warning-face)
    mudline--buffer-file-name))

(defun mudline-segment-position ()
  "Displays the current cursor position in the mode-line."
  (propertize "%03c %p%" 'face 'shadow))

(declare-function flycheck-count-errors "flycheck" (errors))

(defvar-local mudline--flycheck-text "")
(defun mudline--update-flycheck (&optional status)
  (setq mudline--flycheck-text
        (pcase status
          ('finished  (if flycheck-current-errors
                          (let-alist (flycheck-count-errors flycheck-current-errors)
                            (let* ((error (or .error 0))
                                   (warning (or .warning 0))
                                   (info (or .info 0))
                                   (num (cond ((> error 0) error)
                                              ((> warning 0) warning)
                                              (t info)))
                                   (face (cond ((> error 0) 'error)
                                               ((> warning 0) 'warning)
                                               (t 'success))))
                              (concat
                               (mudline--icon 'mdicon "nf-md-alert_circle_outline" face)
                               (propertize (number-to-string num) 'face face))))
                        (mudline--icon 'mdicon "nf-md-check_circle_outline" 'success)))
          ('running     (mudline--icon 'mdicon "nf-md-timer_sand" 'success))
          ('errored     (mudline--icon 'mdicon "nf-md-alert_circle_outline" 'error))
          ('interrupted (mudline--icon 'mdicon "nf-md-pause_circle_outline" 'error))
          (_ ""))))

(defun mudline-segment-flycheck ()
  "Segment to show flycheck status."
  (propertize mudline--flycheck-text 'help-echo "flycheck-mode"))

(defvar-local mudline--flymake-text "")

(defun mudline--flymake-count (type)
  (cl-loop for diag in (flymake-diagnostics)
           as diag-type = (flymake-diagnostic-type diag)
           count (eq (flymake--lookup-type-property diag-type 'severity)
                     (flymake--lookup-type-property type 'severity))))

(defun mudline--update-flymake (&rest _args)
  (setq mudline--flymake-text
        (when-let* ((flymake-active (and (fboundp 'flymake-is-running)
                                         (flymake-is-running)))
                    (status (if (seq-difference (flymake-running-backends)
                                                (flymake-reporting-backends))
                                'running 'finished))
                    (error (mudline--flymake-count :error))
                    (warning (mudline--flymake-count :warning))
                    (note (mudline--flymake-count :note)))
          (setq mudline--flycheck-text
                (pcase status
                  ('finished
                   (let* ((num (cond ((> error 0) error)
                                     ((> warning 0) warning)
                                     (t note)))
                          (face (cond ((> error 0) 'error)
                                      ((> warning 0) 'warning)
                                      (t 'success))))
                     (concat
                      (mudline--icon 'mdicon "nf-md-alert_circle_outline" face)
                      (unless (eq face 'finished)
                        (propertize (number-to-string num) 'face face)))))
                  ('running (mudline--icon 'mdicon "nf-md-timer_sand" 'success))
                  )))))

(defun mudline-segment-flymake ()
  "Segment to show flymake status."
  (propertize mudline--flymake-text 'help-echo "flymake-mode"))

(defun mudline-segment-misc-info ()
  "Segment to display `mode-line-misc-info'."
  (let ((str (format-mode-line mode-line-misc-info 'shadow)))
    (if (string-empty-p str) "" (concat str " "))))

;;;###autoload
(define-minor-mode mudline-mode
  "Toggle mudline on or off."
  :group 'mudline
  :global t
  :lighter nil
  (if mudline-mode
      (progn
        ;; Setup hooks
        (add-hook 'flycheck-status-changed-functions #'mudline--update-flycheck)
        (add-hook 'flycheck-mode-hook #'mudline--update-flycheck)
        ;; (advice-add #'flymake--handle-report :after #'mudline--update-flymake)

        ;; Set the new mode-line-format
        (setq-default mode-line-format
                      '(
                        (:eval (mudline-segment-evil-state))
                        (:eval (mudline-segment-recording))
                        (:eval (mudline-segment-iedit-count))
                        (:eval (mudline-segment-major-mode-icon))
                        (:eval (mudline-segment-buffer-state-icon))
                        (:eval (mudline-segment-buffer-name))
                        mode-line-format-right-align
                        (:eval (mudline-segment-misc-info))
                        (:eval (mudline-segment-flycheck))
                        ;; (:eval (mudline-segment-flymake))
                        (:eval (mudline-segment-position))
                        " ")))
    ;; Remove hooks
    (remove-hook 'flycheck-status-changed-functions #'mudline--update-flycheck)
    (remove-hook 'flycheck-mode-hook #'mudline--update-flycheck)
    ;; (advice-remove #'flymake--handle-report #'mudline--update-flymake)
    ))

(provide 'mudline)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; mudline.el ends here

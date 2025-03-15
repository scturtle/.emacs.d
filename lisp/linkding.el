;;; linkding.el --- Linkding Integration -*- lexical-binding: t -*-

;;; Commentary:
;;; Editing and update linkding bookmark notes in org-mode.

;;; Code:
(require 'json)

(defgroup linkding nil
  ""
  :group 'net)

(defcustom linkding-base-url "https://ld.scturtle.me"
  ""
  :type 'string
  :group 'linkding)

(defcustom linkding-token ""
  ""
  :type 'string
  :group 'linkding)

(defun linkding--build-headers ()
  (if (string-empty-p linkding-token)
      (setq linkding-token (read-string "linkding token: ")))
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(concat "Token " linkding-token))))

(defun linkding-get-note (id)
  "Get a Linkding bookmark by ID and display in org-mode."
  (interactive "sBookmark ID: ")
  (let ((api-url (concat linkding-base-url "/api/bookmarks/" id "/")))
    (require 'plz)
    (plz 'get api-url
         :headers (linkding--build-headers)
         :as 'json-read
         :then (lambda (bookmark) (linkding--display-note bookmark))
         :else (lambda (error) (error "%S" error)))))

(defun linkding--display-note (bookmark)
  "Display bookmark note in org-mode."
  (let* ((buffer-name (format "*Linkding Bookmark %d*" (alist-get 'id bookmark)))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (require 'org)
      (org-mode)
      (erase-buffer)
      (insert (alist-get 'notes bookmark))
      (switch-to-buffer buffer))))

(defun linkding-update-note ()
  "Submit the current buffer content as notes for a bookmark."
  (interactive)
  (let* ((id-match (string-match "*Linkding Bookmark \\([0-9]+\\)*" (buffer-name)))
         (id (if id-match (match-string 1 (buffer-name)) nil))
         (api-url (concat linkding-base-url "/api/bookmarks/" id "/"))
         (payload (json-encode `(("notes" . ,(buffer-string))))))
    (if (not id)
        (error "Linkding bookmark id is not found.")
      (require 'plz)
      (plz 'patch api-url
           :headers (linkding--build-headers)
           :body payload
           :as 'json-read
           :then (lambda (_)
                   (message "Bookmark %s notes updated successfully." id))
           :else (lambda (error) (error "%S" error))))))

(provide 'linkding)
;;; linkding.el ends here

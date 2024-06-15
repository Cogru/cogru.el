;;; cogru-handler.el --- Request handler  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Request handler
;;

;;; Code:

(require 'ht)

(require 'cogru-util)
(require 'cogru-tip)
(require 'cogru-client)

;;
;;; Externals

(defvar cogru--path)

(declare-function cogru-send "cogru.el")
(declare-function cogru-print "cogru.el")

(declare-function cogru--ensure-connected "cogru.el")
(declare-function cogru--ensure-entered "cogru.el")
(declare-function cogru--ensure-under-path "cogru.el")

;;
;;; Request

(defun cogru-test ()
  "Send test request to the server."
  (interactive)
  (cogru--ensure-connected
    (cogru-send `((method . "test")))))

(defun cogru-ping ()
  "Ping the server."
  (interactive)
  (cogru--ensure-connected
    (cogru-send `((method . "ping")))))

(defun cogru-enter ()
  "Enter the room."
  (interactive)
  (cogru--ensure-connected
    (let ((username (read-string "Enter your username: " user-full-name))
          (password (and (y-or-n-p "Does the server requires a password to enter? ")
                         (read-string "Enter password: "))))
      ;; Frist make the client.
      (setq cogru--client (cogru-client-create :username username))
      (cogru-send `((method   . "room::enter")
                    (username . ,username)
                    (password . ,password))))))

(defun cogru-exit ()
  "Exit the room."
  (interactive)
  (cogru--ensure-connected
    (when (yes-or-no-p "Are you sure you want to leave the room? ")
      (cogru-send `((method   . "room::exit")
                    (username . ,(cogru-client-username cogru--client)))))))

(defun cogru-kick ()
  "Kick someone out of the room."
  (interactive)
  (cogru--ensure-connected
    (when (yes-or-no-p (concat "Kick other user may make them lose thier work; "
                               "are you sure you want to proceed? "))
      (let* ((usernames (cogru-client-usernames))
             (username (completing-read "Kick the user: " usernames)))
        (cogru-send `((method   . "room::kick")
                      (admin    . ,(cogru-client-username cogru--client))
                      (username . ,username)))))))

(defun cogru-broadcast ()
  "Broadcast across the room."
  (interactive)
  (cogru--ensure-connected
    (let ((msg (read-string "Broadcast: ")))
      (cogru-send `((method  . "room::broadcast")
                    (message . ,msg))))))

(defun cogru-say ()
  "Say something."
  (interactive)
  (cogru--ensure-under-path
    (let ((msg (read-string "Say: ")))
      (cogru-send `((method  . "file::say")
                    (message . ,msg)
                    (file    . ,(buffer-file-name)))))))

(defun cogru-sync-room ()
  "Sync room files."
  (interactive)
  (cogru--ensure-connected
    (cogru-send `((method . "room::sync")
                  (path   . ,cogru--path)))))

(defun cogru-sync-file ()
  "Sync single file."
  (interactive)
  (cogru--ensure-under-path
    (cogru-send `((method . "file::sync")
                  (file   . ,(buffer-file-name))))))

;;
;;; Response (Room)

(defun cogru--handle-test (data)
  "Handle the `test' event from DATA."
  (message "%s" data))

(defun cogru--handle-pong (data)
  "Handle the `poing' event from DATA."
  (message "%s" data))

(defun cogru--handle-room-enter (data)
  "Handle the `room::enter' event from DATA."
  (let* ((username (ht-get data "username"))
         (msg      (ht-get data "message"))
         (success  (cogru--success-p data)))
    (cond (success
           ;; Validate client and sync!
           (when (string= username (cogru-client-username cogru--client))
             (cogru-sync-room)))
          (t
           ;; Invalidate client!
           (setq cogru--client nil)))
    (message msg)))

(defun cogru--handle-room-exit (data)
  "Handle the `room::exit' event from DATA."
  (let ((msg     (ht-get data "message"))
        (success (cogru--success-p data)))
    (when success (setq cogru--client nil))
    (message msg)))

(defun cogru--handle-room-kick (data)
  "Handle the `room::kick' event from DATA."
  (let* ((username (ht-get data "username"))
         (admin    (ht-get data "admin"))
         (msg      (ht-get data "message"))
         (success  (cogru--success-p data)))
    (if success
        (message "🦶 %s has been kicked out by %s" username admin)
      (message msg))))

(defun cogru--handle-room-broadcast (data)
  "Handle the `room::broadcast' event from DATA."
  (let ((username (ht-get data "username"))
        (msg      (ht-get data "message"))
        (success (cogru--success-p data)))
    (if success
        (message "📢 %s: %s" username msg)
      (message msg))))

(defun cogru--handle-room-users (data)
  "Handle the `room::users' event from DATA."
  (when-let* (((cogru--success-p data))
              (clients (ht-get data "clients"))
              (clients (cogru--json-read-from-string clients)))
    (message "%s" clients)
    ))

(defun cogru--handle-room-sync (data)
  "Handle the `room::sync' event from DATA."
  (let* ((path     (ht-get data "path"))
         (contents (ht-get data "contents"))
         (msg      (ht-get data "message"))
         (success  (cogru--success-p data)))
    (cond (success
           (cogru-write-file path contents))
          (t (message msg)))))

;;
;;; Response (File)

(defun cogru--handle-file-update (data)
  "Handle the `file::update' event from DATA."
  (let* ((success       (cogru--success-p data))
         (file          (cogru--get-file data))
         (add-or-delete (ht-get data "add_or_delete"))
         (beg           (ht-get data "beg"))
         (beg           (cogru-re-point beg))
         (end           (ht-get data "end"))
         (end           (cogru-re-point end))
         (contents      (ht-get data "contents")))
    (cond (success
           (cogru--ensure-under-path
             (cogru--safe-edit
               (pcase add-or-delete
                 ("add"
                  (save-excursion
                    (goto-char beg)
                    (insert contents)))
                 ("delete"
                  (delete-region beg end))))))
          (t (message "Error occurs in `file::update' handler")))))

(defun cogru--handle-file-save (data)
  "Handle the `file::save' event from DATA."
  (let* ((success (cogru--success-p data))
         (file     (cogru--get-file data))
         (contents (ht-get data "contents"))
         (msg      (ht-get data "message")))
    (cond (success
           (cogru-write-file file contents))
          (t (message msg)))))

(defun cogru--handle-file-sync (data)
  "Handle the `file::sync' event from DATA."
  (let* ((file     (cogru--get-file data))
         (contents (ht-get data "contents"))
         (msg      (ht-get data "message"))
         (success  (cogru--success-p data)))
    (cond (success
           (cogru-write-file file contents))
          (t (message msg)))))

(defun cogru--handle-file-info (data)
  "Handle the `file::info' event from DATA."
  (let* (((cogru--success-p data))
         (file     (cogru--get-file data))
         (contents (ht-get data "contents"))
         (clients  (ht-get data "clients"))
         (clients  (cogru--json-read-from-string clients))
         (current-file ))
    (when-let* ((current-file (buffer-file-name))
                ((equal current-file file)))
      (cogru--safe-edit
        (erase-buffer)
        (insert contents)))
    ;;(message "%s" clients)
    (mapc (lambda (client)
            ;; TODO: Collect all client's data!
            )
          clients)))

(defun cogru--handle-file-say (data)
  "Handle the `file::say' event from DATA."
  (let* ((username (ht-get data "username"))
         (msg      (ht-get data "message"))
         (success  (cogru--success-p data)))
    (cond (success
           (if-let ((client (cogru-client-by-username username)))
               (cogru-tip-client-say client msg)
             (message "Try to display `file::say' message but client not found")))
          (t (message msg)))))

(provide 'cogru-handler)
;;; cogru-handler.el ends here

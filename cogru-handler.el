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
(require 'cogru-mode)

;;
;;; Externals

(defvar cogru--client)
(defvar cogru--clients)
(defvar cogru--path)

(declare-function cogru-send "cogru-handler.el")

(declare-function cogru-client-create "cogru.el")
(declare-function cogru-client-username "cogru.el")

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
    ;; TODO: Fill in completing candidates.
    (let ((username (completing-read "Kick the user: " nil)))
      (cogru-send `((method   . "room::kick")
                    (admin    . ,(cogru-client-username cogru--client))
                    (username . ,username))))))

(defun cogru-broadcast ()
  "Broadcast across the room."
  (interactive)
  (cogru--ensure-connected
    (let ((msg (read-string "Message you want to broadcast: ")))
      (cogru-send `((method  . "room::broadcast")
                    (message . ,msg))))))

(defun cogru-sync-room ()
  "Sync room files."
  (interactive)
  (cogru--ensure-connected
    (cogru-send `((method . "room::sync")
                  (path   . ,cogru--path)))))

(defun cogru-sync-file ()
  "Sync single file."
  (interactive)
  (cogru--ensure-connected
    ;; TODO: ..
    ))

;;
;;; Response

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
    (when success
      (setq cogru--client (cogru-client-create :username username))
      (cogru-sync-room))
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

(defun cogru--handle-room-update (data)
  "Handle the `room::update' event from DATA."
  )

(defun cogru--handle-room-users (data)
  "Handle the `room::users' event from DATA."
  (when-let* (((cogru--success-p data))
              (clients (ht-get data "clients"))
              (clients (cogru--json-read-from-string clients)))
    (ic clients)
    ))

(defun cogru--handle-room-sync (data)
  "Handle the `room::sync' event from DATA."
  (let* ((path    (ht-get data "path"))
         (content (ht-get data "content"))
         (exists  (file-exists-p path)))
    (ignore-errors (make-directory (file-name-directory path) t))
    (msgu-silent (write-region content nil path))
    (if exists (message "Overwrote file %s" path)
      (message "Wrote file %s" path))))

(defun cogru--handle-file-say (_data)
  "Handle the `file::say' event from DATA."
  )

(provide 'cogru-handler)
;;; cogru-handler.el ends here

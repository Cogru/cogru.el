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

;;
;;; Externals

(defvar cogru--username)
(defvar cogru--process)

(declare-function cogru-print "cogru.el")
(declare-function cogru--ensure "cogru.el")
(declare-function cogru-send "cogru.el")

;;
;;; Util

(defun cogru--connected-p ()
  "Return non-nil when is connected."
  (process-live-p cogru--process))

(defun cogru--success-p (data)
  "Return status"
  (string= (ht-get data "status") "success"))

;;
;;; Request

(defun cogru-test ()
  "Send test request to the server."
  (interactive)
  (cogru--ensure
    (cogru-send `((method . "test")))))

(defun cogru-ping ()
  "Ping the server."
  (interactive)
  (cogru--ensure
    (cogru-send `((method . "ping")))))

(defun cogru-enter ()
  "Enter the room."
  (interactive)
  (cogru--ensure
    (let ((username (read-string "Enter your username: " user-full-name))
          (password (and (y-or-n-p "Does the server requires a password to enter? ")
                         (read-string "Enter password: "))))
      (cogru-send `((method   . "enter")
                    (username . ,username)
                    (password . ,password))))))

(defun cogru-exit ()
  "Exit the room."
  (interactive)
  (cogru--ensure
    (when (yes-or-no-p "Are you sure you want to leave the room? ")
      (cogru-send `((method   . "exit")
                    (username . ,cogru--username)))
      (setq cogru--username nil))))

(defun cogru-broadcast ()
  "Broadcast across the room."
  (interactive)
  (cogru--ensure
    (let ((msg (read-string "Message you want to broadcast: ")))
      (cogru-send `((method  . "broadcast")
                    (message . ,msg))))))

;;
;;; Response

(defun cogru--handle-test (data)
  "Handler the `test' event from DATA."
  (message "%s" data))

(defun cogru--handle-pong (data)
  "Handler the `poing' event from DATA."
  (message "%s" data))

(defun cogru--handle-enter (data)
  "Handler the `enter' event from DATA."
  (let* ((username (ht-get data "username"))
         (msg      (ht-get data "message"))
         (success  (cogru--success-p data)))
    (when success (setq cogru--username username))
    (cogru-print msg)))

(defun cogru--handle-exit (data)
  "Handler the `exit' event from DATA."
  (let ((msg     (ht-get data "message"))
        (success (cogru--success-p data)))
    (when success (setq cogru--username nil))
    (cogru-print msg)))

(defun cogru--handle-broadcast (data)
  "Handler the `broadcast' event from DATA."
  (let ((username (ht-get data "username"))
        (msg      (ht-get data "message"))
        ;;(success (cogru--success-p data))
        )
    (message "📢 %s: %s" username msg)))

(provide 'cogru-handler)
;;; cogru-handler.el ends here

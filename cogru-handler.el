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
;; Request handler.
;;

;;; Code:

(require 'ht)

(require 'cogru-util)
(require 'cogru-tip)
(require 'cogru-client)

;;
;;; Externals

(defvar lsp-inhibit-lsp-hooks)

(defvar cogru--path)

(declare-function cogru-send "cogru.el")
(declare-function cogru-print "cogru.el")

(defvar cogru-mode)
(declare-function cogru-mode "cogru-mode.el")

(defvar cogru-inhibit-change-hooks)

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
             (username (completing-read "Kick: " usernames)))
        (cogru-send `((method   . "room::kick")
                      (admin    . ,(cogru-client-username cogru--client))
                      (username . ,username)))))))

(defun cogru-broadcast (&optional msg)
  "Broadcast MSG across the room."
  (interactive)
  (cogru--ensure-connected
    (let ((msg (or msg (read-string "Broadcast: "))))
      (cogru-send `((method  . "room::broadcast")
                    (message . ,msg))))))

(defun cogru-say (&optional msg)
  "Say MSG across the file."
  (interactive)
  (cogru--ensure-under-path
    (let ((msg (or msg (read-string "Say: "))))
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
  "Sync the file."
  (interactive)
  (cogru--ensure-under-path
    (cogru-send `((method . "file::sync")
                  (file   . ,(buffer-file-name))))))

(defun cogru-sync-buffer ()
  "Sync the buffer."
  (interactive)
  (cogru--ensure-under-path
    (cogru-send `((method . "file::sync_buffer")
                  (file   . ,(buffer-file-name))))))

(defun cogru-find-user ()
  "Move to user's location."
  (interactive)
  (cogru--ensure-connected
    (let* ((usernames (cogru-client-usernames))
           (username (completing-read "Find: " usernames)))
      (cogru-send `((method . "room::find_user")
                    (username . ,username))))))

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
  (let ((username (ht-get data "username"))
        (admin    (ht-get data "admin")))
    (cogru--handle-request data nil
      (message "🦶 %s has been kicked out by %s" username admin))))

(defun cogru--handle-room-broadcast (data)
  "Handle the `room::broadcast' event from DATA."
  (let ((username (ht-get data "username")))
    (cogru--handle-request data nil
      (message "📢 %s: %s" username msg))))

(defun cogru--handle-room-info (data)
  "Handle the `room::info' event from DATA."
  (cogru--handle-file-info data))

(defun cogru--handle-room-sync (data)
  "Handle the `room::sync' event from DATA."
  (cogru--handle-file-sync data))

(defun cogru--handle-room-find-user (data)
  "Handle the `room::find_user' event from DATA."
  (let ((username (ht-get data "username"))
        (file     (cogru--data-file data))
        (point    (cogru--data-point data "point")))
    (cogru--handle-request data nil
      (when (y-or-n-p
             (format "User `%s` is located at %s in %s; move to it? "
                     username point file))
        (with-current-buffer (find-file file)
          (goto-char point))))))

;;
;;; Response (File)

(defun cogru--handle-file-update (data)
  "Handle the `file::update' event from DATA."
  (let* ((success       (cogru--success-p data))
         (file          (cogru--data-file data))
         (add-or-delete (ht-get data "add_or_delete"))
         (beg           (cogru--data-point data "beg"))
         (end           (cogru--data-point data "end"))
         (contents      (cogru--data-contents data)))
    (cond (success
           (cogru--ensure-under-file file
             (cogru--safe-edit
               (pcase add-or-delete
                 ("add"    (save-excursion (goto-char beg) (insert contents)))
                 ("delete" (delete-region beg end))))))
          (t (message "Error occurs in `file::update' handler")))))

(defun cogru--handle-file-save (data)
  "Handle the `file::save' event from DATA."
  (let ((file     (cogru--data-file data))
        (contents (ht-get data "contents")))
    (cogru--handle-request data nil
      (cogru--sync-file file contents))))

(defun cogru--handle-file-sync (data)
  "Handle the `file::sync' event from DATA."
  (let ((file     (cogru--data-file data))
        (contents (ht-get data "contents")))
    (cogru--handle-request data nil
      (cogru--sync-file file contents))))

(defun cogru--handle-file-sync-buffer (data)
  "Handle the `file::sync_buffer' event from DATA."
  (let ((file     (cogru--data-file data))
        (contents (ht-get data "contents")))
    (cogru--handle-request data nil
      (cogru--sync-buffer file contents))))

(defun cogru--handle-file-info (data)
  "Handle the `file::info' event from DATA."
  (let* ((clients (ht-get data "clients"))
         (clients (cogru--json-read-from-string clients)))
    (cogru-client-deactivate-all)  ; Deactivate all before getting the activate one!
    (mapc (lambda (client)
            (let* ((username     (ht-get client "username"))
                   (path         (ht-get client "path"))
                   (path         (cogru-expand-path path))
                   (color-cursor (ht-get client "color_cursor"))
                   (color-region (ht-get client "color_region")))
              ;; We need to enter the file to decode the correct position!
              (cogru--with-file-buffer path
                ;; These information are decoded and ready to use!
                (let* ((point      (cogru--data-point client "point"))
                       (region-beg (cogru--data-point client "region_beg"))
                       (region-end (cogru--data-point client "region_end")))
                  ;;(ic username path point region-beg region-end)
                  (cogru-client-get-or-create username path
                                              point region-beg region-end
                                              color-cursor color-region
                                              t)))))  ; Set activate!
          clients)
    (cogru-client--render-all)))

(defun cogru--handle-file-say (data)
  "Handle the `file::say' event from DATA."
  (let ((username (ht-get data "username")))
    (cogru--handle-request data nil
      (if-let ((client (cogru-client-by-username username)))
          (progn
            (cogru-tip-client-say client msg)
            (message "🗣️ %s: %s" username msg))
        (message "Try to display `file::say' message but client not found")))))

(provide 'cogru-handler)
;;; cogru-handler.el ends here

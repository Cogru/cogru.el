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

(defvar cogru-chat-history nil
  "List of chat history in string.")

;;
;;; Externals

(defvar lsp-inhibit-lsp-hooks)

(defvar cogru--inputted-username)
(defvar cogru--path)

(declare-function cogru-send "cogru.el")
(declare-function cogru-print "cogru.el")

(defvar cogru-mode)
(declare-function cogru-mode "cogru-mode.el")

(defvar cogru-inhibit-process-send)
(defvar cogru-inhibit-change-hooks)

;;
;;; Util

(defun cogru--this-user-p (username)
  "Return non-nil if the USERNAME is referring to current client."
  (string= username cogru--inputted-username))

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
      (setq cogru--inputted-username username)
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
    (let ((msg (or msg
                   (completing-read
                    "Broadcast: "
                    (lambda (string pred action)
                      (cogru-presorted-completions string pred action
                                                   cogru-chat-history))))))
      (cogru-add-completion-history msg 'cogru-chat-history)
      (cogru-send `((method  . "room::broadcast")
                    (message . ,msg))))))

(defun cogru-say (&optional msg)
  "Say MSG across the file."
  (interactive)
  (cogru--ensure-under-path
    (let ((msg (or msg
                   (completing-read
                    "Say: "
                    (lambda (string pred action)
                      (cogru-presorted-completions string pred action
                                                   cogru-chat-history))))))
      (cogru-add-completion-history msg 'cogru-chat-history)
      (cogru-send `((method  . "file::say")
                    (message . ,msg)
                    (file    . ,(buffer-file-name)))))))

(defun cogru-clear-chat-history ()
  "Clear char history."
  (interactive)
  (let ((count (length cogru-chat-history)))
    (setq cogru-chat-history nil)
    (cogru-print "[Cogru] Chat history cleared: %s" count)))

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
  (cogru--ensure-under-file nil
    (cogru-send `((method . "buffer::sync")
                  (file   . ,(buffer-file-name))))))

(defun cogru-save-buffer ()
  "Save the buffer."
  (cogru--ensure-under-file nil
    (cogru-send `((method   . "buffer::save")
                  (file     . ,(buffer-file-name))
                  (contents . ,(cogru-buffer-string))))))

(defun cogru-find-user ()
  "Move to user's location."
  (interactive)
  (cogru--ensure-connected
    (let* ((usernames (cogru-client-usernames))
           (username  (completing-read "Find: " usernames)))
      (cogru-send `((method   . "room::find_user")
                    (username . ,username))))))

(defun cogru-new-file (&optional filename)
  "Add FILENAME to the server.."
  (cogru--ensure-under-path
    (when-let* ((filename (or filename (buffer-file-name)))
                ((file-exists-p filename)))  ; Only when file exists!
      (cogru-send `((method   . "room::add_file")
                    (file     . ,filename)
                    (contents . ,(elenv-file-contents filename)))))))

(defun cogru-delete-file (&optional filename)
  "Delete the FILENAME from the server.."
  (cogru--ensure-under-path
    (cogru-send `((method . "room::delete_file")
                  (file   . ,(or filename (buffer-file-name)))))))

(defun cogru-rename-file (file newname)
  "Rename the FILE to NEWNAME from the server.."
  (cogru--ensure-under-path
    (cogru-send `((method  . "room::rename_file")
                  (file    . ,file)
                  (newname . ,newname)))))

;;
;;; Response (General)

(defun cogru--handle-test (data)
  "Handle the `test' event from DATA."
  (message "%s" data))

(defun cogru--handle-pong (data)
  "Handle the `poing' event from DATA."
  (cogru--handle-request data nil
    (message "Received pong at %s" (ht-get data "timestamp"))))

;;
;;; Response (Room)

(defun cogru--handle-room-enter (data)
  "Handle the `room::enter' event from DATA."
  (let* ((username (ht-get data "username")))
    (cogru--handle-request data
        (progn (setq cogru--client nil)  ; Invalidate client!
               (message msg))            ; Print error
      ;; Validate client and sync!
      (when (cogru--this-user-p username)
        ;; Create the client.
        (setq cogru--client (cogru-client-create :username username))
        (cogru-sync-room))
      (message "🚪 %s has entered the room" username))))

(defun cogru--handle-room-exit (data)
  "Handle the `room::exit' event from DATA."
  (let ((username (ht-get data "username")))
    (cogru--handle-request data
        (message msg)  ; Print error
      (when (cogru--this-user-p username)
        (setq cogru--client nil))
      (message "👋 %s has left the room" username))))

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

(defun cogru--handle-room-add-file (data)
  "Handle the `room::add_file' event from DATA."
  (cogru--handle-buffer-save data))

(defun cogru--handle-room-delete-file (data)
  "Handle the `room::delete_file' event from DATA."
  (let ((cogru-inhibit-process-send t)
        (file (cogru--data-file data)))
    (cogru--handle-request data nil
      (delete-file file))))

(defun cogru--handle-room-rename-file (data)
  "Handle the `room::rename_file' event from DATA."
  (let ((cogru-inhibit-process-send t)
        (file    (cogru--data-file data))
        (newname (cogru--data-file data "newname")))
    (cogru--handle-request data nil
      (rename-file file newname))))

;;
;;; Response (File)

(defun cogru--handle-file-sync (data)
  "Handle the `file::sync' event from DATA."
  (let ((file     (cogru--data-file data))
        (contents (ht-get data "contents")))
    (cogru--handle-request data nil
      (cogru--sync-file file contents))))

(defun cogru--handle-file-info (data)
  "Handle the `file::info' event from DATA."
  (let* ((clients (ht-get data "clients"))
         (clients (cogru--json-read-from-string clients)))
    (cogru-client-deactivate-all)  ; Deactivate all before set `activate' flag!
    (mapc (lambda (d-client)
            (let* ((username     (ht-get d-client "username"))
                   (path         (ht-get d-client "path"))
                   (path         (cogru-expand-path path))
                   (color-cursor (ht-get d-client "color_cursor"))
                   (color-region (ht-get d-client "color_region"))
                   (client       (cogru-client-get-or-create username)))
              ;; Skip the current user.
              (unless (cogru-client-this-user-p username)
                ;; We need to enter the file to decode the correct position!
                (cogru--with-file-buffer path
                  ;; These information are decoded and ready to use!
                  (let* ((point      (cogru--data-point d-client "point"))
                         (region-beg (cogru--data-point d-client "region_beg"))
                         (region-end (cogru--data-point d-client "region_end")))
                    ;;(ic username (cogru-client-predicting client))
                    (if (cogru-client-predicting client)
                        (setf (cogru-client-predicting client) nil)
                      (cogru-client-get-or-create username path
                                                  point region-beg region-end
                                                  color-cursor color-region
                                                  t)))))))  ; Set `activate' flag!
          clients)
    (cogru-client--render-all)))

(defun cogru--handle-file-say (data)
  "Handle the `file::say' event from DATA."
  (let ((username (ht-get data "username")))
    (cogru--handle-request data nil
      (if-let ((client (cogru-client-by-username username)))
          (progn
            (cogru-tip-client-say client (format "%s: %s" username msg))
            (message "🗣 %s: %s" username msg))
        (message "Try to display `file::say' message but client not found")))))

;;
;;; Response (Buffer)

(defun cogru--handle-buffer-update (data)
  "Handle the `buffer::update' event from DATA."
  (let* ((success       (cogru--success-p data))
         (username      (ht-get data "username"))
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
                 ("delete" (delete-region beg end))))
             (let* ((delete-p (string= add-or-delete "delete"))
                    (delta (cogru--predict-delta delete-p beg end)))
               (cogru-client--predict-render-all username delete-p beg end delta))))
          (t (message "Error occurs in `buffer::update' handler")))))

(defun cogru--handle-buffer-save (data)
  "Handle the `buffer::save' event from DATA."
  (let ((file     (cogru--data-file data))
        (contents (ht-get data "contents")))
    (cogru--handle-request data nil
      (cogru--sync-file file contents))))

(defun cogru--handle-buffer-sync (data)
  "Handle the `buffer::sync' event from DATA."
  (let* ((file     (cogru--data-file data))
         (contents (ht-get data "contents"))
         (contents (cogru-decode-str contents)))
    (cogru--handle-request data nil
      (cogru--sync-buffer file contents))))

(provide 'cogru-handler)
;;; cogru-handler.el ends here

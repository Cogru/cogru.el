;;; cogru-mode.el --- Cogru mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shen, Jen-Chieh

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
;; Cogru mode.
;;

;;; Code:

(require 'named-timer)

(require 'cogru-client)
(require 'cogru)

(defcustom cogru-interval 0.2
  "Interval in seconds between updating each frames."
  :type 'number
  :group 'cogru)

(defcustom cogru-post-command-delay 0.1
  "Number of seconds before executing post command."
  :type 'number
  :group 'cogru)

(defcustom cogru-update-hook nil
  "Hooks run after each update."
  :type 'hook
  :group 'cogru)

(defcustom cogru-cursor-color (face-background 'cursor)
  "Custom cursor color."
  :type 'string
  :group 'cogru)

(defcustom cogru-region-color (face-background 'region)
  "Custom region color."
  :type 'string
  :group 'cogru)

(defconst cogru--update-timer-name (intern "*cogru-timer:update*")
  "Name of the update timer.")

(defconst cogru--post-command-timer-name (intern "*cogru-timer:post*")
  "Name of the post command timer.")

(defvar cogru--cleared-client-p nil
  "Set to non-nil when info is clear on the server.")

(defvar cogru--old-cursor-color (face-background 'cursor)
  "Old cursor color.")

(defvar cogru--old-region-color (face-background 'region)
  "Old region color.")

(defvar cogru--current-buffer nil
  "Record the current buffer preventing over syncing.")

;;
;;; Externals

(defvar cogru--path)

(declare-function cogru-start "cogru.el")
(declare-function cogru-stop "cogru.el")
(declare-function cogru-send "cogru.el")

;;
;;; Entry

(defvar cogru--starting-p nil
  "Record state of starting the client.")

(defun cogru-mode--post-command ()
  "Disable `cogru-mode' when the starting operation is cancelled."
  (unless cogru--starting-p
    (unless (cogru--connected-p) (cogru-mode -1))
    (remove-hook 'post-command-hook #'cogru-mode--post-command)))

;;;###autoload
(define-minor-mode cogru-mode
  "Minor mode `cogru-mode'."
  :global t
  :require 'cogru-mode
  :group 'cogru
  (if cogru-mode (cogru-mode--enable) (cogru-mode--disable)))

(defun cogru-mode--enable ()
  "Enable `cogru-mode'."
  (add-hook 'post-command-hook #'cogru-mode--post-command)
  (let ((cogru--starting-p t))
    (unless (cogru--connected-p) (cogru-start)))
  (remove-hook 'post-command-hook #'cogru-mode--post-command)
  (cogru--ensure-connected
    (named-timer-run cogru--update-timer-name nil cogru-interval
                     #'cogru--update)
    (add-function :after after-focus-change-function #'cogru--after-focus)
    (add-hook 'window-selection-change-functions #'cogru--window-x-change 95)
    (add-hook 'window-buffer-change-functions #'cogru--window-x-change 95)
    (add-hook 'before-change-functions #'cogru--before-change 95)
    (add-hook 'after-change-functions #'cogru--after-change 95)
    (add-hook 'pre-command-hook #'cogru--pre-command 95)
    (add-hook 'post-command-hook #'cogru--post-command 95)
    (add-hook 'after-save-hook #'cogru--after-save 95)))

(defun cogru-mode--disable ()
  "Disable `cogru-mode'."
  (named-timer-cancel cogru--update-timer-name)
  (remove-function after-focus-change-function #'cogru--after-focus)
  (remove-hook 'window-selection-change-functions #'cogru--window-x-change)
  (remove-hook 'window-buffer-change-functions #'cogru--window-x-change)
  (remove-hook 'before-change-functions #'cogru--before-change)
  (remove-hook 'after-change-functions #'cogru--after-change)
  (remove-hook 'pre-command-hook #'cogru--pre-command)
  (remove-hook 'post-command-hook #'cogru--post-command)
  (remove-hook 'after-save-hook #'cogru--after-save)
  (cogru-client-clean-all)
  (cogru--cursor-revert)
  (cogru-stop))

;;
;;; Core

(defun cogru--after-focus (&rest _)
  "Function runs after focusing the frame."
  (cond ((frame-focus-state)
         (let ((cogru--current-buffer))
           (cogru--window-x-change)))
        (t )))  ; Do nothing

(defun cogru--window-x-change (&rest _)
  "On switch buffer."
  (cogru--ensure-under-path
    (unless (equal cogru--current-buffer (current-buffer))
      (cogru-sync-buffer))))

(defun cogru--update ()
  "Update between interval."
  (cogru--ensure-under-path
    (cogru--schedule-send-client-info)
    (cogru-send `((method   . "room::info")
                  (username . ,(cogru-client-username cogru--client))
                  (file     . ,(buffer-file-name)))))
  (run-hooks 'cogru-update-hook))

(defun cogru--after-save ()
  "After save hook."
  (cogru-save-buffer))

;;
;;; Addition / Deletion

(defvar cogru-inhibit-change-hooks nil
  "Set to non-nil to disable all change requests.")

(defvar cogru--befor-end nil
  "Record the delete end position.")

(defun cogru--before-change (beg end)
  "Do stuff before buffer is changed with BEG and END."
  (cogru--ensure-under-path
    (unless (= beg end)
      ;; Record end position for deletion!
      (setq cogru--befor-end (cogru-encode-point end)))))

(defun cogru--change-data (beg end len)
  "Correct change data calculated by BEG, END and LEN."
  (let* ((new-beg (+ beg len))
         (swap-p (<= end new-beg))  ; If swap mean deleting!
         (beg (if swap-p end new-beg))
         (end (if swap-p new-beg end)))
    (unless (= beg end)
      (list (if swap-p "delete" "add") beg end
            (if swap-p ""
              (buffer-substring-no-properties beg end))))))

(defun cogru--after-change (beg end len)
  "Do stuff after buffer is changed with BEG, END and LEN."
  (cogru--ensure-under-path
    (when-let* (((not cogru-inhibit-change-hooks))
                (data          (cogru--change-data beg end len))
                (add-or-delete (nth 0 data))
                (beg           (cogru-encode-point (nth 1 data)))
                (end           (if (string= add-or-delete "delete")
                                   cogru--befor-end  ; Use before for deletion
                                 (cogru-encode-point (nth 2 data))))
                (contents      (cogru-encode-str (nth 3 data)))
                (path          (cogru-client-path cogru--client)))
      (cogru--send-client-info)
      (cogru-send `((method        . "buffer::update")
                    (path          . ,path)            ; What file to update?
                    (add_or_delete . ,add-or-delete)   ; `add' or `delete'
                    (beg           . ,beg)             ; Beginning position
                    (end           . ,end)             ; End position
                    (contents      . ,contents))))))   ; Only used for addition!

;;
;;; Post

(defun cogru--client-update-info ()
  "Keep the server's information up to date regarding this client."
  (when cogru--client
    (let* ((use-region (use-region-p))
           (path (and (cogru--under-path-p)
                      (buffer-file-name)))
           (point (point))
           (region-beg (and use-region (region-beginning)))
           (region-end (and use-region (region-end))))
      (setf (cogru-client-path cogru--client) path)
      (setf (cogru-client-point cogru--client) point)
      (setf (cogru-client-region-beg cogru--client) region-beg)
      (setf (cogru-client-region-end cogru--client) region-end)
      (setf (cogru-client-color-cursor cogru--client) cogru-cursor-color)
      (setf (cogru-client-color-region cogru--client) cogru-region-color))))

(defun cogru--send-client-info ()
  "Send the client information."
  (cogru--client-update-info)  ; Update status before send.
  (cogru--ensure-entered
    (let* ((path         (cogru-client-path cogru--client))
           (point        (cogru-client-point cogru--client))
           (point        (cogru-encode-point point))
           (region-beg   (cogru-client-region-beg cogru--client))
           (region-beg   (cogru-encode-point region-beg))
           (region-end   (cogru-client-region-end cogru--client))
           (region-end   (cogru-encode-point region-end))
           (color-cursor (cogru-client-color-cursor cogru--client))
           (color-region (cogru-client-color-region cogru--client)))
      (when (or path (not cogru--cleared-client-p))
        (cogru-send `((method       . "room::update_client")
                      (path         . ,path)
                      (point        . ,point)
                      (region_beg   . ,region-beg)
                      (region_end   . ,region-end)
                      (color_cursor . ,color-cursor)
                      (color_region . ,color-region)))
        (setq cogru--cleared-client-p nil))
      ;; Flag to clean up the client info once before stop
      ;; sending further more data.
      (unless path
        (setq cogru--cleared-client-p t)))))

(defun cogru--schedule-send-client-info ()
  "Schedule to send client information."
  (named-timer-run cogru--post-command-timer-name
    cogru-post-command-delay nil
    #'cogru--send-client-info))

(defun cogru--pre-command ()
  "Pre command hook."
  (setq cogru--current-buffer (current-buffer)))

(defun cogru--post-command ()
  "Post command hook."
  ;; XXX: Update status for tip? Or just move to tip post command?
  (progn
    (cogru--client-update-info))
  (cogru--schedule-send-client-info)
  (cogru--cursor-post-command)
  (cogru-tip--post-command))

;;
;;; Cursor & Region

(defun cogru--cursor-set ()
  "Set cursor status."
  (set-cursor-color cogru-cursor-color)
  (set-face-background 'region cogru-region-color))

(defun cogru--cursor-revert ()
  "Revert cursor status."
  (set-cursor-color cogru--old-cursor-color)
  (set-face-background 'region cogru--old-region-color))

(defun cogru--cursor-post-command ()
  "Post command for cursor."
  (cogru--ensure-connected
    (if (and cogru--client (cogru--under-path-p))
        (cogru--cursor-set)
      (cogru--cursor-revert))))

(provide 'cogru-mode)
;;; cogru-mode.el ends here

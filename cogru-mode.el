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

(defcustom cogru-update-hook nil
  "Hooks run after each update."
  :type 'hook
  :group 'cogru)

(defconst cogru--update-timer-name (intern "*cogru-timer*")
  "Name of the update timer.")

(defvar cogru--cleared-client-p nil
  "Set to non-nil when info is clear on the server.")

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
    (add-hook 'before-change-functions #'cogru--before-change 95)
    (add-hook 'after-change-functions #'cogru--after-change 95)
    (add-hook 'post-command-hook #'cogru--post-command 95)
    (add-hook 'after-save-hook #'cogru--after-save 95)))

(defun cogru-mode--disable ()
  "Disable `cogru-mode'."
  (named-timer-cancel cogru--update-timer-name)
  (remove-hook 'before-change-functions #'cogru--before-change)
  (remove-hook 'after-change-functions #'cogru--after-change)
  (remove-hook 'post-command-hook #'cogru--post-command)
  (remove-hook 'after-save-hook #'cogru--after-save)
  (cogru-stop))

;;
;;; Core

(defun cogru--update ()
  "Update between interval."
  (cogru--ensure-under-path
    (cogru-send `((method   . "room::info")
                  (username . ,(cogru-client-username cogru--client))
                  (file     . ,(buffer-file-name)))))
  (run-hooks 'cogru-update-hook))

(defun cogru--after-save ()
  "After save hook."
  (cogru--ensure-under-path
    (when-let ((path (cogru-client-path cogru--client)))
      (cogru-send `((method . "file::save")
                    (path   . ,path))))))

;;
;;; Addition / Deletion

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
              (cogru-str-le (buffer-substring-no-properties beg end)))))))

(defun cogru--after-change (beg end len)
  "Do stuff after buffer is changed with BEG, END and LEN."
  (cogru--ensure-under-path
    (when-let* ((data          (cogru--change-data beg end len))
                (add-or-delete (nth 0 data))
                (beg           (cogru-encode-point (nth 1 data)))
                (end           (if (string= add-or-delete "delete")
                                   cogru--befor-end  ; Use before for deletion
                                 (cogru-encode-point (nth 2 data))))
                (contents      (nth 3 data))
                (path          (cogru-client-path cogru--client)))
      (cogru-send `((method        . "file::update")
                    (path          . ,path)            ; What file to update?
                    (add_or_delete . ,add-or-delete)   ; `add' or `delete'
                    (beg           . ,beg)             ; Beginning position
                    (end           . ,end)             ; End position
                    (contents      . ,contents))))))   ; Only used for addition!

;;
;;; Post

(defun cogru--post-command ()
  "Post command hook."
  (cogru-client-update-info)  ; Update status before send.
  (cogru--ensure-entered
    (let* ((path       (cogru-client-path cogru--client))
           (point      (cogru-client-point cogru--client))
           (point      (cogru-encode-point point))
           (region-beg (cogru-client-region-beg cogru--client))
           (region-beg (cogru-encode-point      region-beg))
           (region-end (cogru-client-region-end cogru--client))
           (region-end (cogru-encode-point      region-end)))
      (when (or path (not cogru--cleared-client-p))
        (cogru-send `((method     . "room::update_client")
                      (path       . ,path)
                      (point      . ,point)
                      (region_beg . ,region-beg)
                      (region_end . ,region-end)))
        (setq cogru--cleared-client-p nil))
      ;; Flag to clean up the client info once before stop
      ;; sending further more data.
      (unless path
        (setq cogru--cleared-client-p t))))
  (cogru--ensure-under-path
    (cogru-tip--post-command)))

(provide 'cogru-mode)
;;; cogru-mode.el ends here

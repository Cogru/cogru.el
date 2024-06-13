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

(require 'cogru-util)

(defcustom cogru-interval 0.4
  "Interval in seconds between updating each frames."
  :type 'number
  :group 'cogru)

(defconst cogru--update-timer-name (intern "*cogru-timer*")
  "Name of the update timer.")

(defvar cogru--cleared-client-p nil
  "Set to non-nil when info is clear on the server.")

;;
;;; Externals

(defvar cogru--client)
(defvar cogru--clients)
(defvar cogru--path)

(declare-function cogru-start "cogru.el")
(declare-function cogru-stop "cogru.el")
(declare-function cogru-send "cogru.el")

(declare-function cogru-client-point "cogru.el")
(declare-function cogru-client-path "cogru.el")
(declare-function cogru-client-region-start "cogru.el")
(declare-function cogru-client-region-end "cogru.el")
(declare-function cogru-client-update "cogru.el")

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
  (cogru--ensure-connected
    (named-timer-run cogru--update-timer-name nil cogru-interval
                     #'cogru--update)
    (add-hook 'after-change-functions #'cogru--after-change 95)
    (add-hook 'post-command-hook #'cogru--post-command 95)
    (add-hook 'after-save-hook #'cogru--after-save 95)))

(defun cogru-mode--disable ()
  "Disable `cogru-mode'."
  (named-timer-cancel cogru--update-timer-name)
  (remove-hook 'after-change-functions #'cogru--after-change)
  (remove-hook 'post-command-hook #'cogru--post-command)
  (remove-hook 'after-save-hook #'cogru--after-save)
  (cogru-stop))

;;
;;; Core

(defun cogru--update ()
  "Update between interval."
  (cogru--ensure-under-path
    (cogru-send `((method   . "file::users")
                  (username . ,(cogru-client-username cogru--client))
                  (file     . ,(buffer-file-name))))))

(defun cogru--after-save ()
  "After save hook."
  (cogru--ensure-under-path
    (when-let ((path (cogru-client-path cogru--client)))
      (cogru-send `((method . "file::save")
                    (path   . ,path))))))

;;
;;; Addition / Deletion

(defun cogru--change-data (beg end len)
  "Correct change data calculated by BEG, END and LEN."
  (let* ((new-beg (+ beg len))
         (swap-p (<= end new-beg))
         (beg (if swap-p end new-beg))
         (end (if swap-p new-beg end)))
    (unless (= beg end)
      (list (if swap-p "delete" "add") beg end
            (buffer-substring-no-properties beg end)))))

(defun cogru--after-change (beg end len)
  "Do stuff after buffer is changed with BEG, END and LEN."
  (cogru--ensure-under-path
    (when-let* ((data          (cogru--change-data beg end len))
                (add-or-delete (nth 0 data))
                (beg           (1- (position-bytes (nth 1 data))))
                (end           (1- (position-bytes (nth 2 data))))
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
  (cogru-client-update)
  (cogru--ensure-entered
    (let ((path         (cogru-client-path cogru--client))
          (point        (cogru-client-point cogru--client))
          (region-start (cogru-client-region-start cogru--client))
          (region-end   (cogru-client-region-end cogru--client)))
      (when (or path (not cogru--cleared-client-p))
        (cogru-send `((method       . "room::update_client")
                      (path         . ,path)
                      (point        . ,point)
                      (region_start . ,region-start)
                      (region_end   . ,region-end)))
        (setq cogru--cleared-client-p nil))
      ;; Flag to clean up the client info once before stop
      ;; sending further more data.
      (unless path
        (setq cogru--cleared-client-p t)))))

(provide 'cogru-mode)
;;; cogru-mode.el ends here

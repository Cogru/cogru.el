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

(defconst cogru--update-timer-name (intern "*cogru-imter*")
  "Name of the update timer.")

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

;;;###autoload
(define-minor-mode cogru-mode
  "Minor mode `cogru-mode'."
  :global t
  :require 'cogru-mode
  :group 'cogru
  (if cogru-mode (cogru-mode--enable) (cogru-mode--disable)))

(defun cogru-mode--enable ()
  "Enable `cogru-mode'."
  (unless (cogru--connected-p) (cogru-start))
  (cogru--ensure-connected
    (named-timer-run cogru--update-timer-name nil cogru-interval
                     #'cogru--update)
    (add-hook 'post-command-hook #'cogru--post-command 95)
    (add-hook 'after-save-hook #'cogru--after-save 95)))

(defun cogru-mode--disable ()
  "Disable `cogru-mode'."
  (named-timer-cancel cogru--update-timer-name)
  (remove-hook 'post-command-hook #'cogru--post-command)
  (remove-hook 'after-save-hook #'cogru--after-save)
  (cogru-stop))

;;
;;; Core

(defun cogru--update ()
  "Update between interval."
  (cogru--ensure-entered
    (cogru-send `((method   . "room::users")
                  (username . ,(cogru-client-username cogru--client))))))

(defun cogru--post-command ()
  "Post command hook."
  (cogru-client-update)
  (cogru--ensure-entered
    (when-let ((cogru--client)
               (path         (cogru-client-path cogru--client))
               (point        (cogru-client-point cogru--client))
               (region-start (cogru-client-region-start cogru--client))
               (region-end   (cogru-client-region-end cogru--client)))
      (cogru-send `((method       . "room::update")
                    (path         . ,path)
                    (point        . ,point)
                    (region_start . ,region-start)
                    (region_end   . ,region-end))))))

(defun cogru--after-save ()
  "After save hook."
  (cogru--ensure-entered
    (when-let* (((cogru--under-path-p))
                (cogru--client)
                (path         (cogru-client-path cogru--client)))
      (cogru-send `((method . "file::save")
                    (path   . ,path))))))

(provide 'cogru-mode)
;;; cogru-mode.el ends here

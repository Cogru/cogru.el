;;; cogru-client.el --- Client implementation  -*- lexical-binding: t; -*-

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
;; Client implementation.
;;

;;; Code:

(require 'cogru-util)

(cl-defstruct (cogru-client
               (:constructor cogru-client-create))
  "The client implementation."
  username
  entered
  admin
  path
  point
  region-beg region-end
  frame-name-dialogue)

(defvar cogru--client nil
  "Local client represent self.")

(defvar cogru--clients nil
  "List of simulated clients.")

;;
;;; Externals

(declare-function cogru--under-path-p "cogru.el")

;;
;;; Properties

(defun cogru-client--get-properties (name &optional this)
  "Get a list of property NAME including THIS."
  (let ((props)
        (func (intern (format "cogru-client-%s" name))))
    (unless (fboundp func)
      (error "[Cogru] Unknown property name: %s" name))
    (dolist (client cogru--clients)
      (push (funcall func client) props))
    (when this
      (push (funcall func cogru--client) props))
    props))

(defun cogru-client-usernames (&optional this)
  "Get a list of username including THIS."
  (cogru-client--get-properties "username" this))

;;
;;; Core

(defun cogru-client-update-info ()
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
      (setf (cogru-client-region-end cogru--client) region-end))))

(defun cogru-client--render-clients ()
  "Render clients."
  (cogru--ensure-connected
    (when cogru--clients

      )))

(defun cogru-client-by-username (username)
  "Return the client by USERNAME."
  (cond ((equal username (cogru-client-username cogru--client))
         cogru--client)
        (t
         (cl-some (lambda (client)
                    (when (equal username (cogru-client-username client))
                      client))
                  cogru--clients))))

(provide 'cogru-client)
;;; cogru-client.el ends here

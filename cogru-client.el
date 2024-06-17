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

(require 'ht)
(require 'posframe)

(require 'cogru-util)

(cl-defstruct (cogru-client
               (:constructor cogru-client-create))
  "The client implementation."
  username
  admin
  path
  point
  region-beg region-end
  ;; Control
  active
  ;; Rendering
  frame-name-dialogue  ; The posframe ID string.
  ov-cursor
  ov-region)

(defcustom cogru-cursor-overlay-proprity 10
  "Overlay priority for cursor."
  :type 'integer
  :group 'cogru)

(defcustom cogru-cursor-overlay-region 0
  "Overlay priority for region."
  :type 'integer
  :group 'cogru)

(defvar cogru--client nil
  "Local client represent self.")

(defvar cogru--clients (make-hash-table :test 'equal)
  "List of simulated clients.")

;;
;;; Externals

(defvar cogru-mode)
(declare-function cogru-mode "cogru-mode.el")

(declare-function cogru-tip-move "cogru-tip.el")

;;
;;; Properties

(defun cogru-client--get-properties (name &optional this)
  "Get a list of property NAME including THIS."
  (let ((props)
        (func (intern (format "cogru-client-%s" name))))
    (unless (fboundp func)
      (error "[Cogru] Unknown property name: %s" name))
    (ht-map (lambda (_username client)
              (push (funcall func client) props))
            cogru--clients)
    (when this
      (push (funcall func cogru--client) props))
    props))

(defun cogru-client-usernames (&optional this)
  "Get a list of username including THIS."
  (cogru-client--get-properties "username" this))

;;
;;; Overlay

(defun cogru-client--update-cursor-ov (client)
  "Update cursor overlay for CLIENT.

If not found, create one instead."
  (let* ((username (cogru-client-username client))
         (pt (cogru-client-point client))
         (ov (or (cogru-client-ov-cursor client)
                 (make-overlay pt (1+ pt)))))
    (move-overlay ov pt (1+ pt))
    (overlay-put ov 'face 'cursor)
    (overlay-put ov 'priority cogru-cursor-overlay-proprity)
    (overlay-put ov 'help-echo (format "%s (cursor)" username))
    (setf (cogru-client-ov-cursor client) ov)  ; set overlay
    ov))

(defun cogru-client--update-region-ov (client)
  "Update region overlay for CLIENT.

If not found, create one instead."
  (let* ((username (cogru-client-username client))
         (region-beg (cogru-client-region-beg client))
         (region-end (cogru-client-region-end client))
         (ov (cogru-client-ov-region client)))
    (cond (region-beg
           (unless ov
             (setq ov (make-overlay region-beg region-end)))
           (move-overlay ov region-beg region-end)
           (overlay-put ov 'face 'region)
           (overlay-put ov 'priority cogru-cursor-overlay-region)
           (overlay-put ov 'help-echo (format "%s (region)" username)))
          (t (cogru-delete-overlay ov)))
    (setf (cogru-client-ov-region client) ov)  ; set overlay
    ov))

;;
;;; Frame

(defun cogru-client--update-dialogue-frame (client)
  "Update dialogue."
  (when-let* ((frame-data (cogru-client-frame-name-dialogue client))
              (frame (cdr frame-data))
              ((frame-visible-p frame))  ; Only effect visible frame!
              (buffer-name (car frame-data))
              (point (cogru-client-point client)))
    (cogru-tip-move buffer-name point)))

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

(defun cogru-client--render (client)
  "Render single client."
  (let* ((path (cogru-client-path client))
         (path (cogru-expand-path path)))
    (cond ((and (cogru-client-active client)
                path
                (equal (buffer-file-name) path))
           (cogru-client--update-dialogue-frame client)
           (cogru-client--update-region-ov client)
           (cogru-client--update-cursor-ov client))
          (t
           (posframe-hide (cogru-client-frame-name-dialogue client))
           (cogru-delete-overlay (cogru-client-ov-cursor client))
           (cogru-delete-overlay (cogru-client-ov-region client))))))

(defun cogru-client--render-all ()
  "Render clients."
  (cogru--ensure-connected
    (ht-map (lambda (_username client)
              (cogru-client--render client))
            cogru--clients)))

(defun cogru-client-by-username (username)
  "Return the client by USERNAME."
  (cond ((equal username (cogru-client-username cogru--client))
         cogru--client)
        (t
         (ht-get cogru--clients username))))

(defun cogru-client-deactivate-all ()
  "Deactivate all clients.

This is used before getting the new clients' information."
  (ht-map (lambda (_username client)
            (setf (cogru-client-active client) nil))
          cogru--clients))

(defun cogru-client-get-or-create ( username path
                                    point region-beg region-end
                                    &optional active)
  "Get the client or create one."
  (when-let (((not (equal username
                          (cogru-client-username cogru--client))))  ; skip self
             (client (or (ht-get cogru--clients username)
                         (cogru-client-create :username username
                                              :path path
                                              :point point
                                              :region-beg region-beg
                                              :region-end region-end))))
    (setf (cogru-client-username   client) username)
    (setf (cogru-client-path       client) path)
    (setf (cogru-client-point      client) point)
    (setf (cogru-client-region-beg client) region-beg)
    (setf (cogru-client-region-end client) region-end)
    (setf (cogru-client-active     client) active)
    (ht-set cogru--clients username client)
    client))

(provide 'cogru-client)
;;; cogru-client.el ends here

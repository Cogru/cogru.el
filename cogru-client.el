;;; cogru-client.el --- Client implementation  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Shen, Jen-Chieh

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
  predicting
  ;; Rendering
  frame-name-dialogue  ; The posframe ID string.
  ov-cursor color-cursor
  ov-region color-region)

(defcustom cogru-cursor-overlay-proprity 10
  "Overlay priority for cursor."
  :type 'integer
  :group 'cogru)

(defcustom cogru-cursor-overlay-region 0
  "Overlay priority for region."
  :type 'integer
  :group 'cogru)

(defvar cogru--inputted-username nil
  "Inputted username; used to later verify enter the room.")

(defvar cogru--client nil
  "Local client represent self.")

(defvar cogru--clients (make-hash-table :test 'equal)
  "List of simulated clients.")

;;
;;; Externals

(defvar cogru-mode)
(declare-function cogru-mode "cogru-mode.el")
(declare-function cogru-cursor-color "cogru-mode.el")

(declare-function cogru-tip-move "cogru-tip.el")

;;
;;; Util

(defun cogru-client-this-user-p (username)
  "Return non-nil if USERNAME is the current user."
  (equal username (cogru-client-username cogru--client)))

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
;;; Tip

(defun cogru-client--update-dialogue-frame (client)
  "Update dialogue for CLIENT."
  (when-let* ((frame-data (cogru-client-frame-name-dialogue client))
              (buffer-name (car frame-data))
              (frame (cdr frame-data))
              (point (cogru-client-point client)))
    (cond ((and (frame-parameter frame 'cogru-active)  ; only active frame
                (cogru--point-visible-p point))        ; only when point is visible
           (make-frame-visible frame)
           (cogru-tip-move buffer-name point (if (equal client cogru--client)
                                                 (cogru-cursor-color)
                                               (cogru-client-color-cursor client))))
          (t
           (make-frame-invisible frame)))))

;;
;;; Faces

(defun cogru-cursor-face (name)
  "Return cursor face by NAME."
  (let* ((name (intern (format "cogru-%s-cursor" name)))
         (copied (or (cogru-get-face name)
                     (copy-face 'cursor name))))
    copied))

(defun cogru-region-face (name)
  "Return region face by NAME."
  (let* ((name (intern (format "cogru-%s-region" name)))
         (copied (or (cogru-get-face name)
                     (copy-face 'region name))))
    copied))

(defun cogru-client--cursor-face (username color)
  "Make custom cursor face by USERNAME and COLOR."
  (let ((face (cogru-cursor-face username)))
    (set-face-background face color)
    face))

(defun cogru-client--region-face (username color)
  "Make custom region face by USERNAME and COLOR."
  (let ((face (cogru-region-face username)))
    (set-face-background face color)
    face))

;;
;;; Overlay

(defun cogru-client--update-cursor-ov (client)
  "Update cursor overlay for CLIENT.

If not found, create one instead."
  (let* ((username (cogru-client-username client))
         (pt (or (cogru-client-point client) (point-max)))
         (ov (or (cogru-client-ov-cursor client)
                 (make-overlay pt (1+ pt))))
         (color (cogru-client-color-cursor client))
         (face (cogru-client--cursor-face username color)))
    (move-overlay ov pt (1+ pt))
    (overlay-put ov 'face face)
    (overlay-put ov 'priority cogru-cursor-overlay-proprity)
    (overlay-put ov 'help-echo (format "%s (cursor)" username))
    (setf (cogru-client-ov-cursor client) ov)  ; set overlay
    ov))

(defun cogru-client--update-region-ov (client)
  "Update region overlay for CLIENT.

If not found, create one instead."
  (let* ((username (cogru-client-username client))
         (region-beg (or (cogru-client-region-beg client) (point-max)))
         (region-end (or (cogru-client-region-end client) (point-max)))
         (color (cogru-client-color-region client))
         (ov (cogru-client-ov-region client))
         (face (cogru-client--region-face username color)))
    (cond (region-beg
           (unless ov
             (setq ov (make-overlay region-beg region-end)))
           (move-overlay ov region-beg region-end)
           (overlay-put ov 'face face)
           (overlay-put ov 'priority cogru-cursor-overlay-region)
           (overlay-put ov 'help-echo (format "%s (region)" username)))
          (t (cogru-delete-overlay ov)))
    (setf (cogru-client-ov-region client) ov)  ; set overlay
    ov))

;;
;;; Rendering (other clients)

(defun cogru--predict-delta (delete-p beg end)
  "Return the predicted delta movement calculated by DELETE-P, BEG and END."
  (if delete-p
      (- beg end)
    (- end beg)))

(defun cogru-client--predict-render (client sender-p delete-p beg end delta)
  "Predict rendering for CLIENT.

Arguments SENDER-P, DELETE-P, BEG, END and DELTA are used to do the prediction."
  ;; First, update the client's data.
  (when-let* (((not (zerop delta)))
              (pt (cogru-client-point client))
              ((<= beg pt))  ; only move cursor below
              (point (if delete-p beg end)))
    (setf (cogru-client-predicting client) t)  ; set `predicting' flag
    (setf (cogru-client-point client) (if sender-p
                                          point
                                        (+ pt delta)))
    (when-let ((region-beg (cogru-client-region-beg client))
               (region-end (cogru-client-region-end client)))
      ;; Only when region is active.
      (setf (cogru-client-region-beg client) (if sender-p
                                                 point
                                               (+ region-beg delta)))
      (setf (cogru-client-region-end client) (if sender-p
                                                 point
                                               (+ region-end delta))))
    ;; Then re-render it.
    (cogru-client--render client)))

(defun cogru-client--predict-render-all (s-username delete-p beg end delta)
  "Predict all clients.

Argument S-USERNAME is the sender of this request.  The rest of the arguments
DELETE-P, BEG, END and DELTA are used to do the prediction."
  (cogru--ensure-connected
    (unless (zerop delta)
      (ht-map (lambda (username client)
                (let ((sender-p (equal s-username username)))
                  (cogru-client--predict-render client sender-p delete-p beg end delta)))
              cogru--clients))))

(defun cogru-client--render (client)
  "Render single CLIENT."
  (when (cogru-client-p client)
    (let* ((path (cogru-client-path client))
           (path (cogru-expand-path path)))
      (cond ((and (cogru-client-active client)
                  path
                  (equal (buffer-file-name) path))
             (cogru-client--update-dialogue-frame client)
             (cogru-client--update-region-ov client)
             (cogru-client--update-cursor-ov client))
            (t
             (when-let* ((frame-data (cogru-client-frame-name-dialogue client))
                         (frame (cdr frame-data)))
               (make-frame-invisible frame))
             (cogru-delete-overlay (cogru-client-ov-cursor client))
             (cogru-delete-overlay (cogru-client-ov-region client)))))))

(defun cogru-client--render-all ()
  "Render clients."
  (cogru--ensure-connected
    (ht-map (lambda (_username client)
              (cogru-client--render client))
            cogru--clients)))

;;
;;; Core

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

(defun cogru-client-get-or-create ( username
                                    &optional path
                                    point region-beg region-end
                                    color-cursor color-region
                                    active)
  "Get the client or create one.

Argument USERNAME is the key in `hash-table'.  You can pass in the rest of the
arguments to override their values; PATH, POINT, REGION-BEG, REGION-END,
COLOR-CURSOR, COLOR-REGION and ACTIVE."
  (when-let (((not (cogru-client-this-user-p username)))  ; skip self
             (client (or (ht-get cogru--clients username)
                         (cogru-client-create :username username
                                              :path path
                                              :point point
                                              :region-beg region-beg
                                              :region-end region-end))))
    (setf (cogru-client-username     client) username)
    (setf (cogru-client-path         client) path)
    (setf (cogru-client-point        client) point)
    (setf (cogru-client-region-beg   client) region-beg)
    (setf (cogru-client-region-end   client) region-end)
    (setf (cogru-client-color-cursor client) color-cursor)
    (setf (cogru-client-color-region client) color-region)
    (setf (cogru-client-active       client) active)
    (ht-set cogru--clients username client)
    client))

(defun cogru-client--clean (client)
  "Clean up single CLIENT."
  (setf (cogru-client-active client) nil)
  (cogru-client--render client))

(defun cogru-client-clean-all ()
  "Clean up all clients."
  (cogru-client--render cogru--client)
  (ht-map (lambda (_username client)
            (cogru-client--clean client))
          cogru--clients))

(provide 'cogru-client)
;;; cogru-client.el ends here

;;; cogru-tip.el --- Tip implementation  -*- lexical-binding: t; -*-

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
;; Tip implementation.
;;

;;; Code:

(require 'ht)
(require 'posframe)
(require 'named-timer)

(require 'cogru-util)
(require 'cogru-client)

(defcustom cogru-tip-delay 3.0
  "Background color string."
  :type 'number
  :group 'cogru)

(defcustom cogru-tip-background-color "#2A2D38"
  "Background color string."
  :type 'string
  :group 'cogru)

(defcustom cogru-tip-foreground-color "#F1F1F1"
  "Foreground color string."
  :type 'string
  :group 'cogru)

(defvar cogru-tip-frame-parameters
  `((cogru                    . t)
    (left                     . -1)
    (no-focus-on-map          . t)
    (min-width                . 0)
    (width                    . 0)
    (min-height               . 0)
    (height                   . 0)
    (internal-border-width    . 1)
    (menu-bar-lines           . 0)
    (tool-bar-lines           . 0)
    (tab-bar-lines            . 0)
    (tab-bar-lines-keep-state . 0)
    (line-spacing             . 0)
    (unsplittable             . t)
    (undecorated              . t)
    (top                      . -1)
    (visibility               . nil)
    (mouse-wheel-frame        . nil)
    (no-other-frame           . t)
    (inhibit-double-buffering . t)
    (drag-internal-border     . t)
    (no-special-glyphs        . t)
    (desktop-dont-save        . t)
    (vertical-scroll-bars     . t))
  "Frame parameters used to create the frame.")

;;
;;; Externals

(declare-function cogru-send "cogru.el")
(declare-function cogru-print "cogru.el")

(defvar cogru-mode)
(declare-function cogru-mode "cogru-mode.el")
(declare-function cogru-cursor-color "cogru-mode.el")

;;
;;; Util

(defun cogru-tip--buffer-name (name)
  "Return the buffer name for NAME."
  (format "*cogru::%s*" name))

(defun cogru-tip-contents (buffer-name)
  "Return frame's contents by BUFFER-NAME."
  (with-current-buffer (cogru-tip--buffer-name buffer-name) (buffer-string)))

;;
;;; Core

(cl-defun cogru-tip-show ( buffer-name string point
                           border-color
                           &key
                           (timeout 300)
                           (hide t))
  "Pop up an tooltip (BUFFER-NAME) depends on the graphic used.

STRING is the content of the toolip.  The location POINT.  TIMEOUT for not
forever delay.  The argument HIDE is for internal use."
  (let* ((buffer-name (cogru-tip--buffer-name buffer-name))
         (fringe-width 10)
         (timer-name (intern buffer-name))
         (frame (posframe-show
                 buffer-name
                 :string string :position point
                 :timeout timeout
                 :background-color cogru-tip-background-color
                 :foreground-color cogru-tip-foreground-color
                 :internal-border-width 1
                 :internal-border-color border-color
                 :left-fringe fringe-width :right-fringe fringe-width
                 :override-parameters
                 (append cogru-tip-frame-parameters
                         `((default-minibuffer-frame . ,(selected-frame))
                           (minibuffer               . ,(minibuffer-window)))))))
    (set-frame-parameter frame 'cogru-active t)
    ;; Start hide timer.
    (when hide
      (named-timer-run timer-name cogru-tip-delay nil
                       (lambda ()
                         (set-frame-parameter frame 'cogru-active nil)
                         (make-frame-invisible frame))))
    frame))

(defun cogru-tip-move ( buffer-name point border-color)
  "Move the posframe by BUFFER-NAME to POINT.

The argument BORDER-COLOR is the border color around the dialogue."
  (let ((contents (cogru-tip-contents buffer-name)))  ; Retrieved original contents!
    (cogru-tip-show buffer-name contents point border-color
                    :hide nil)))

;;
;;; Core

(defun cogru-tip--post-command ()
  "Post command hook for tip."
  (cogru--ensure-under-path
    (cogru-client--update-dialogue-frame cogru--client)))

;;
;;; Actions

(defun cogru-tip-client-say (client msg)
  "Show the tip MSG said by CLIENT."
  (let* ((username (cogru-client-username client))
         (buffer-name (format "say::%s" username))
         (point (cogru-client-point client))
         (frame (cogru-tip-show buffer-name msg point (cogru-cursor-color))))
    (setf (cogru-client-frame-name-dialogue client) (cons buffer-name frame))))

(provide 'cogru-tip)
;;; cogru-tip.el ends here

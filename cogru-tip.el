;;; cogru-tip.el --- Tip implementation  -*- lexical-binding: t; -*-

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
;; Tip implementation.
;;

;;; Code:

(require 'ht)
(require 'posframe)
(require 'named-timer)

(require 'cogru-util)
(require 'cogru-mode)

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
  `((left                     . -1)
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

;;
;;; Core

(cl-defun cogru-tip-create (buffer-name string &key point (timeout 300))
  "Pop up an tooltip (BUFFER-NAME) depends on the graphic used.

STRING is the content of the toolip.  The location POINT.  TIMEOUT for not
forever delay."
  (let* ((bg cogru-tip-background-color)
         (fg cogru-tip-foreground-color)
         (fringe-width 10)
         (timer-name (intern buffer-name)))
    (posframe-show
     buffer-name
     :string string :position point
     :timeout timeout
     :background-color bg :foreground-color fg
     :internal-border-width 1
     :internal-border-color (face-foreground 'font-lock-comment-face nil t)
     :left-fringe fringe-width :right-fringe fringe-width
     :override-parameters
     (append cogru-tip-frame-parameters
             `((default-minibuffer-frame . ,(selected-frame))
               (minibuffer               . ,(minibuffer-window))))
     :accept-focus t)
    (named-timer-run timer-name cogru-tip-delay nil
                     (lambda () (posframe-hide buffer-name)))
    t))

;;
;;; Request

(defun cogru-say ()
  "Say something."
  (interactive)
  (cogru--ensure-connected
    (let ((msg (read-string "Say: ")))
      (cogru-send `((method  . "file::say")
                    (message . ,msg))))))

;;
;;; Response

(defun cogru--handle-file-say (data)
  "Handle the `say' event from DATA."
  (let* ((username (ht-get data "username"))
         (msg      (ht-get data "message"))
         (success  (cogru--success-p data)))
    ;; TODO: ..
    (cogru-print username msg success)
    ))

(provide 'cogru-tip)
;;; cogru-tip.el ends here

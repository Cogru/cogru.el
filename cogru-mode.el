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

(require 'cogru-util)

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
  (cogru--ensure
    (add-hook 'post-command-hook #'cogru--post-command 95)
    (add-hook 'after-save-hook #'cogru--after-save 95)
    ))

(defun cogru-mode--disable ()
  "Disable `cogru-mode'."
  (remove-hook 'post-command-hook #'cogru--post-command)
  (remove-hook 'after-save-hook #'cogru--after-save)
  (cogru-stop))

;;
;;; Core

(defun cogru--post-command ()
  "Post command hook."
  (cogru-client-update)
  (cogru--ensure
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
  (cogru--ensure
    (when (cogru--project-file-p)
      ;; TODO: ..
      )))

(provide 'cogru-mode)
;;; cogru-mode.el ends here

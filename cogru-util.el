;;; cogru-util.el --- Cogru util module  -*- lexical-binding: t; -*-

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
;; Cogru util module.
;;

;;; Code:

(require 'ht)
(require 's)
(require 'show-eol)

;;
;;; Externals

(defvar lsp-inhibit-lsp-hooks)

;;
;;; Network

(defun cogru--success-p (data)
  "Return success status from DATA."
  (string= (ht-get data "status") "success"))

;;
;;; I/O

(defun cogru-write-file (path contents)
  "Write CONTENTS to PATH."
  (let ((exists (ignore-errors (file-exists-p path))))
    (ignore-errors (make-directory (file-name-directory path) t))
    (let ((buffer-file-coding-system 'undecided-unix)
          (coding-system-for-write)
          (last-coding-system-used 'utf-8))
      (msgu-silent (write-region contents nil path)))
    ;; Print status
    (if exists (message "Overwrote file %s" path)
      (message "Wrote file %s" path))))

;;
;;; String

(defun cogru-2str (obj)
  "Convert OBJ to string."
  (format "%s" obj))

(defun cogru-str-le (str)
  "Handle STR line endings."
  (cond ((eq 'dos (show-eol-get-current-system))
         (s-replace "\n" "\r\n" str))
        (t str)))

(defun cogru-insert (&rest args)
  "Insert STR to buffer."
  (let ((lsp-inhibit-lsp-hooks t)
        (after-change-functions)
        (before-change-functions))
    (apply #'insert args)))

;;
;;; IO

(defmacro cogru--json-serialize (params)
  "Serialize json PARAMS."
  (if (progn (require 'json)
             (fboundp 'json-serialize))
      `(json-serialize ,params
                       :null-object nil
                       :false-object :json-false)
    `(let ((json-false :json-false))
       (json-encode ,params))))

(defun cogru--make-message (params)
  "Create a CSP message from PARAMS, after encoding it to a JSON string."
  (let ((body (cogru--json-serialize params)))
    (concat "Content-Length: "
            (number-to-string (1+ (string-bytes body)))
            "\r\n\r\n"
            body
            "\n")))

(defmacro cogru--json-read-buffer ()
  "Read json from the current buffer."
  (if (progn
        (require 'json)
        (fboundp 'json-parse-buffer))
      `(json-parse-buffer :object-type 'hash-table
                          :null-object nil
                          :false-object nil)
    `(let ((json-array-type 'vector)
           (json-object-type 'hash-table)
           (json-false nil))
       (json-read))))

(defun cogru--json-read-from-string (json-string)
  "Read JSON-STRING to JSON object."
  (with-temp-buffer
    (insert json-string)
    (goto-char (point-min))
    (cogru--json-read-buffer)))

;;
;;; Point

(defun cogru-position-bytes (position)
  "Like function `position-bytes' but handle window line endings.

The argument POSITION is the point."
  (+ (position-bytes position)
     (if (eq 'dos (show-eol-get-current-system))
         (how-many "\n" 1 position)
       0)))

(defun cogru-re-point (position)
  "Reverse formula of the function `cogru-position-bytes'.

Convert byte position to text point."
  (byte-to-position
   (- position
      (if (eq 'dos (show-eol-get-current-system))
          (how-many "\n" 1 (point))
        0))))

(defun cogru-point ()
  "Return point in bytes space."
  (cogru-position-bytes (point)))

(defun cogru-region-start ()
  "Return region start point in bytes space."
  (cogru-position-bytes (region-beginning)))

(defun cogru-region-end ()
  "Return region end point in bytes space."
  (cogru-position-bytes (region-end)))

(provide 'cogru-util)
;;; cogru-util.el ends here

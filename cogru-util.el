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

;;
;;; Externals

(defvar cogru-host)
(defvar cogru-port)
(defvar cogru--process)
(defvar cogru--path)

;;
;;; Network

(defun cogru-address ()
  "Return the address name."
  (format "http://%s:%s" cogru-host cogru-port))

(defun cogru--connected-p ()
  "Return non-nil when is connected."
  (process-live-p cogru--process))

(defun cogru--success-p (data)
  "Return status"
  (string= (ht-get data "status") "success"))

(defmacro cogru--ensure-connected (&rest body)
  "Run BODY only if connection is established."
  (declare (indent 0))
  `(cond
    ((cogru--connected-p) ,@body)
    ((not cogru-mode))  ; Do nothing
    (t
     (cogru-mode -1)  ; This will clean up the variable `cogru--client' too!
     (message (concat "[Cogru] No connection being established; "
                      "try `M-x cogru-start` to connect to the server")))))

(defmacro cogru--ensure-entered (&rest body)
  "Run BODY only if client is established."
  (declare (indent 0))
  `(cogru--ensure-connected
     (cond (cogru--client ,@body)
           (t
            (message
             (concat "[Cogru] You haven't enter the room yet; "
                     "try `M-x cogru-enter` to enter the room"))))))

;;
;;; Project

(defun cogru--project-file-p (&optional path)
  "Return non-nil if the PATH is under the workspace."
  (string-prefix-p cogru--path (or path (buffer-file-name)) t))

;;
;;; String

(defun cogru-2str (obj)
  "Convert OBJ to string."
  (format "%s" obj))

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

(provide 'cogru-util)
;;; cogru-util.el ends here

;;; cogru.el --- Cogru plugin for real-time collaborative editing  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/Cogru/cogru.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (msgu "0.1.0") (s "1.12.0") (ht "2.0") (log4e "0.4.1"))
;; Keywords: convenience cogru

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
;; Cogru plugin for real-time collaborative editing.
;;

;;; Code:

(require 'cl-lib)

(require 'msgu)
(require 's)
(require 'ht)
(require 'log4e)

(require 'cogru-handler)

(defgroup cogru nil
  "Cogru plugin for real-time collaborative editing."
  :prefix "cogru-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/Cogru/cogru.el"))

(defcustom cogru-host "localhost"
  "Default host to connect to the server."
  :type 'string
  :group 'cogru)

(defcustom cogru-port 8786
  "Default port to connect to the server."
  :type 'integer
  :group 'cogru)

(defcustom cogru-filter-data-hook nil
  "Hook run when the data comes in."
  :type 'hook
  :group 'cogru)

(defvar cogru--username nil
  "The client's username.")

(defvar cogru--process nil
  "Process to one workspace.")

(defvar cogru--data nil
  "Hold the received raw data wait for a complete requests.")

(defvar cogru-default-directory nil
  "The default directory for syncing the entire file tree.")

;;
;;; Logger

(log4e:deflogger "cogru" "%t [%l] %m" "%Y-%m-%dT%H:%M:%S")

(cogru--log-set-level 'trace)

(define-minor-mode cogru-debug-mode
  "Turn on/off debug mode for `cogru'."
  :global t
  :group 'cogru
  :init-value nil
  (if cogru-debug-mode (cogru--log-enable-logging)
    (cogru--log-disable-logging)))

(defun cogru-print (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (msgu-inhibit-log
    (apply #'message fmt args)))

;;
;;; Util

(defun cogru-2str (obj)
  "Convert OBJ to string."
  (format "%s" obj))

(defun cogru-address ()
  "Return the address name."
  (format "http://%s:%s" cogru-host cogru-port))

(defmacro cogru--ensure (&rest body)
  "Run BODY only if connection is established."
  (declare (indent 0))
  `(if (cogru--connected-p)
       (progn ,@body)
     (user-error
      (concat
       "[WARNING] Can't send data without the connection being established; "
       "try `M-x cogru-start` to connect to the server"))))

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
;;; Core

(defconst cogru--content-length-len (string-bytes "Content-Length: ")
  "Hold the text content-length's length.")

(defconst cogru--separator-len (string-bytes "\r\n")
  "Hold the content separator length.")

(defun cogru-send (obj)
  "Send message to the server."
  (process-send-string cogru--process (cogru--make-message obj)))

(defun cogru--handle (data)
  "Handle the incoming request DATA."
  (cogru--log-trace data)
  (let* ((data   (cogru--json-read-from-string data))
         (method (ht-get data "method"))
         (func (pcase method
                 ("test"      #'cogru--handle-test)
                 ("pong"      #'cogru--handle-pong)
                 ("enter"     #'cogru--handle-enter)
                 ("exit"      #'cogru--handle-exit)
                 ("broadcast" #'cogru--handle-broadcast)
                 (_ (user-error "[ERROR] Unknown action: %s" method)))))
    (funcall func data)))

(defun cogru--content-length (data)
  "Return the content length in number from DATA."
  (when-let* ((splitted (split-string data "\r\n"))
              (len (length splitted))  ; array size
              ((<= 3 len))  ; not enough data to process, need at least 3 lines
              (contnet-length (cl-first splitted))
              (content-body   (cl-third splitted))
              (contnet-length (s-replace "Content-Length: " "" contnet-length)))
    (cons (string-to-number contnet-length) content-body)))

(defun cogru--substring (data &optional from to)
  "Like function `substring' but in bytes.

The arguments DATA is the string data; and FROM and TO are argumenets are
string position but in bytes."
  (with-temp-buffer
    (insert data)
    (set-buffer-multibyte nil)
    (let ((from (or from (1- (point-min))))
          (to   (or to   (1- (point-max)))))
      (decode-coding-region (1+ from) (1+ to) 'utf-8 t))))

(defun cogru--process (data)
  "Decode raw DATA."
  (setq cogru--data (concat cogru--data data))  ; pile up the data
  (unless (string-match-p "\r\n" cogru--data)
    (setq cogru--data (s-replace "\n" "\r\n" cogru--data)))
  (when-let* ((content-info    (cogru--content-length cogru--data))
              (content-length  (car content-info))
              (content-body    (cdr content-info))
              (received-length (string-bytes content-body))
              ;; If `content-length' and `received-length' are the same, it
              ;; mean we have received the complete data and ready to be process!
              ((<= content-length received-length))
              (from (+ cogru--content-length-len (string-bytes (cogru-2str content-length))
                       cogru--separator-len cogru--separator-len))
              (to (+ from content-length)))
    (cogru--handle (cogru--substring cogru--data from (+ from content-length)))
    ;; Remove the processed data.
    (setq cogru--data (cogru--substring cogru--data to))
    ;; When there are data left, continue process the data until we have
    ;; processed all the requests.
    (unless (zerop (string-bytes cogru--data))
      (cogru--process ""))))

(defun cogru--filter (proc data &rest _)
  "Process DATA from PROC."
  (run-hook-with-args cogru-filter-data-hook proc data)
  (cogru--process data))

(defun cogru--select-workspace ()
  "Pick a workspace to start collaboration."
  (setq cogru-default-directory nil)  ; reset
  (let* ((dir (read-directory-name "Select a directory to start the workspace: "))
         (files (directory-files default-directory nil directory-files-no-dot-files-regexp nil 1)))
    (when (or (null files)
              (and files
                   (yes-or-no-p "The folder is not empty, you may lose your file and/or corrupt the workspace.
Ar you sure? ")))
      (setq cogru-default-directory (expand-file-name dir)))))

;;
;;; Entry

;;;###autoload
(defun cogru-start ()
  "Start from connecting to the server."
  (interactive)
  (msgu-inhibit-log
    ;; Silently kill the session if the process is already dead.
    (unless (cogru--connected-p)
      (ignore-errors (cogru-stop)))
    (cond
     (cogru--process
      (user-error "[WARNING] The connection is already established; only one client-server connection is allowed"))
     (t
      (cogru--select-workspace)
      (cond (cogru-default-directory
             (let* ((default-addr (cogru-address))
                    (addr (read-string "Host url: " default-addr))
                    (url-info (url-generic-parse-url addr))
                    (host (url-host url-info))
                    (port (url-port url-info)))
               (cogru-print "[INFO] Connecting to %s..." addr)
               (setq cogru--process
                     (make-network-process :name "*tcp-server-cogru*"
                                           :buffer "*tcp-server-cogru*"
                                           :filter #'cogru--filter
                                           :host host
                                           :service port))
               (cogru-print "[INFO] Connected to [cogru-server:%s %s]" port cogru-default-directory)
               (when (y-or-n-p "Do you want to enter the room? ")
                 (cogru-enter))))
            (t (cogru-print "[INFO] Failed to establish workspace")))))))

(defun cogru-stop ()
  "Stop the connection."
  (interactive)
  (cond (cogru--process
         (delete-process cogru--process)
         (setq cogru--process nil
               cogru--data nil
               cogru-default-directory nil)
         (cogru-print "[INFO] Safely disconnected from the server"))
        (t (user-error "[WARNING] No connection is established; this does nothing"))))

(provide 'cogru)
;;; cogru.el ends here

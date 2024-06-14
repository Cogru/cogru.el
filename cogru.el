;;; cogru.el --- Cogru plugin for real-time collaborative editing  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/Cogru/cogru.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (elenv "0.1.0") (msgu "0.1.0") (s "1.12.0") (ht "2.0") (log4e "0.4.1") (posframe "1.4.3") (named-timer "0.1") (show-eol "0.0.5"))
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

(require 'elenv)
(require 'msgu)
(require 's)
(require 'ht)
(require 'log4e)

(require 'cogru-util)
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

(defvar cogru--process nil
  "Process to one workspace.")

(defvar cogru--data nil
  "Hold the received raw data wait for a complete requests.")

(defvar cogru--path nil
  "The default directory for syncing the entire file tree.")

;;
;;; Extenrals

(defvar cogru-mode)
(defvar cogru--client)
(defvar cogru--clients)

(declare-function cogru-mode "cogru-mode.el")

;;
;;; Logger

(log4e:deflogger "cogru" "%t [%l] %m" "%Y-%m-%dT%H:%M:%S")

(cogru--log-set-level 'trace)

(define-minor-mode cogru-debug-mode
  "Turn on/off debug mode for `cogru'."
  :global t
  :group 'cogru
  :init-value nil
  :require 'cogru
  (cond (cogru-debug-mode
         (cogru--log-enable-logging)
         (cogru--log-enable-messaging nil))
        (t
         (cogru--log-disable-logging)
         (cogru--log-disable-messaging)))
  (cogru--log-clear-log))

(defun cogru-print (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (msgu-inhibit-log
    (apply #'message fmt args)))

;;
;;; Network

(defun cogru-address ()
  "Return the address name."
  (format "http://%s:%s" cogru-host cogru-port))

(defun cogru--connected-p ()
  "Return non-nil when is connected."
  (process-live-p cogru--process))

(defun cogru--under-path-p (&optional path)
  "Return non-nil if the PATH is under the workspace."
  (when-let ((path (or path (buffer-file-name))))
    (string-prefix-p cogru--path path t)))

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
     (when cogru--client ,@body)))

(defmacro cogru--ensure-under-path (&rest body)
  "Run BODY only if client is under session path."
  (declare (indent 0))
  `(cogru--ensure-connected
     (cogru--ensure-entered
       (when (cogru--under-path-p) ,@body))))

;;
;;; Core

(defconst cogru--content-length-len (string-bytes "Content-Length: ")
  "Hold the text content-length's length.")

(defconst cogru--separator-len (string-bytes "\r\n")
  "Hold the content separator length.")

(defun cogru-send (obj)
  "Send message OBJ to the server."
  (process-send-string cogru--process (cogru--make-message obj)))

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
  (setq cogru--path nil)  ; reset
  (let* ((dir (read-directory-name "Select a directory to start the workspace: "))
         (files (directory-files default-directory nil directory-files-no-dot-files-regexp nil 1)))
    (when (or (null files)
              (and files
                   (yes-or-no-p "The folder is not empty, you may lose your file and/or corrupt the workspace.
Ar you sure? ")))
      (setq cogru--path (expand-file-name dir)))))

;;
;;; Entry

(defun cogru--initialize ()
  "Initialize the client.

First message we send to the server."
  (cogru--ensure-connected
    (cogru-send `((method . "init")
                  (path   . ,cogru--path)))))

(defun cogru--handle-init (data)
  "Handle the `init' event from DATA."
  (let ((msg (ht-get data "message"))
        (success (cogru--success-p data)))
    (cond
     (success
      (cogru--log-info msg)
      (sleep-for 0.5)
      (when (y-or-n-p "Do you want to enter the room? ")
        (cogru-enter)))
     (t
      (cogru-print "Unable to initialize the workspace")
      (sleep-for 0.5)
      (cogru-stop)))))

(defun cogru--handle (data)
  "Handle the incoming request DATA."
  (cogru--log-trace data)
  (let* ((data   (cogru--json-read-from-string data))
         (method (ht-get data "method"))
         (func (pcase method
                 ("test"            #'cogru--handle-test)
                 ("pong"            #'cogru--handle-pong)
                 ("init"            #'cogru--handle-init)
                 ("room::enter"     #'cogru--handle-room-enter)
                 ("room::exit"      #'cogru--handle-room-exit)
                 ("room::kick"      #'cogru--handle-room-kick)
                 ("room::broadcast" #'cogru--handle-room-broadcast)
                 ("room::users"     #'cogru--handle-room-users)
                 ("room::sync"      #'cogru--handle-room-sync)
                 ("file::save"      #'cogru--handle-file-save)
                 ("file::sync"      #'cogru--handle-file-sync)
                 ("file::users"     #'cogru--handle-file-users)
                 ("file::say"       #'cogru--handle-file-say)
                 (_ (cogru-print "[Cogru] Unknown action to handle: %s" method)))))
    (funcall func data)))

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
      (cogru-print "[Cogru] The connection is already established; only one client-server connection is allowed"))
     (t
      (cogru--select-workspace)
      (cond (cogru--path
             (let* ((default-addr (cogru-address))
                    (addr (read-string "Host url: " default-addr))
                    (url-info (url-generic-parse-url addr))
                    (host (url-host url-info))
                    (port (url-port url-info)))
               (cogru-print "[Cogru] Connecting to %s..." addr)
               (setq cogru--process
                     (make-network-process :name "*tcp-server-cogru*"
                                           :buffer "*tcp-server-cogru*"
                                           :filter #'cogru--filter
                                           :host host
                                           :service port))
               (cogru-print "[Cogru] Connected to [cogru-server:%s %s]" port cogru--path)
               (cogru--initialize)))
            (t (cogru-print "[Cogru] Failed to establish workspace")))))))

(defun cogru-stop ()
  "Stop the connection."
  (interactive)
  (cond (cogru--process
         (delete-process cogru--process)
         (setq cogru--process nil
               cogru--data nil
               cogru--path nil
               cogru--client nil
               cogru--clients nil)
         (cogru-print "[Cogru] Safely disconnected from the server"))
        (t
         (cogru-print "[Cogru] No connection is established; this does nothing"))))

(provide 'cogru)
;;; cogru.el ends here

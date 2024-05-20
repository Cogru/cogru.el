;;; cogru.el --- Cogru plugin for real-time collaborative editing  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/Cogru/cogru.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
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

(defvar cogru-process nil
  "Process to one workspace.")

(defvar cogru-default-directory nil
  "The default directory for syncing the entire file tree.")

;;
;;; Util

(defun cogru-address ()
  "Return the address name."
  (format "http://%s:%s" cogru-host cogru-port))

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

;;
;;; Core

(defun cogru-send (obj)
  "Send message to the server."
  (process-send-string cogru-process (cogru--make-message obj)))

(defun cogru--handle (data)
  "Handle the incoming request DATA."
  (cogru-send
   '((jsonrpc . "2.0")
     (method   . "enter")
     (password . ""))))

(defun cogru--filter (proc data &rest _)
  "Process DATA from PROC."
  (run-hook-with-args cogru-filter-data-hook proc data)
  (cogru--handle data))

(defun cogru--select-workspace ()
  "Pick a workspace to start collaboration."
  (setq cogru-default-directory nil)  ; reset
  (let* ((dir (read-directory-name "Select a directory to start the workspace: "))
         (files (directory-files default-directory nil directory-files-no-dot-files-regexp nil 1)))
    (when (or (null files)
              (and files
                   (yes-or-no-p "The folder is not empty, you may lose your file and/or corrupt the workspace.
Ar you sure? ")))
      (setq cogru-default-directory dir))))

;;
;;; Entry

;;;###autoload
(defun cogru-start ()
  "Start from connecting to the server."
  (interactive)
  (cond
   (cogru-process
    (user-error "[WARNING] The connection is already established; only one client-server connection is allowed"))
   (t
    (cogru--select-workspace)
    (cond (cogru-default-directory
           (let* ((default-addr (cogru-address))
                  (addr (read-string "Host url: " default-addr))
                  (url-info (url-generic-parse-url addr))
                  (host (url-host url-info))
                  (port (url-port url-info)))
             (message "[INFO] Connecting to %s..." addr)
             (setq cogru-process
                   (make-network-process :name "*tcp-server-cogru*"
                                         :buffer "*tcp-server-cogru*"
                                         :filter #'cogru--filter
                                         :host host
                                         :service port))
             (message "[INFO] Connected to %s" cogru-process)))
          (t (message "[INFO] Failed to establish workspace"))))))

(defun cogru-stop ()
  "Stop the connection."
  (interactive)
  (cond (cogru-process
         (delete-process cogru-process)
         (setq cogru-process nil
               cogru-default-directory nil)
         (message "[INFO] Safely disconnected from the server"))
        (t (user-error "[WARNING] No connection is established; this does nothing"))))

(provide 'cogru)
;;; cogru.el ends here

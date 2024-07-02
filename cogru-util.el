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

(defcustom cogru-after-edit-hook nil
  "Hooks run after safe edit."
  :type 'hook
  :group 'cogru)

;;
;;; Externals

(defvar lsp-inhibit-lsp-hooks)

(defvar cogru-host)
(defvar cogru-port)
(defvar cogru--process)
(defvar cogru--path)

(defvar cogru--client)
(defvar cogru--clients)

(defvar cogru-mode)
(declare-function cogru-mode "cogru-mode.el")
(declare-function cogru-print "cogru.el")

(defvar cogru-inhibit-change-hooks)

;;
;;; Progress

(defmacro coru-with-progress (msg-beg body msg-end)
  "Progress BODY wrapper with prefix (MSG-START) and suffix (MSG-END) messages."
  (declare (indent 0) (debug t))
  `(progn
     (cogru-print ,msg-beg)
     ,body
     (cogru-print ,msg-end)))

;;
;;; Network

(defun cogru--success-p (data)
  "Return success status from DATA."
  (string= (ht-get data "status") "success"))

(defun cogru--data-file (data &optional key)
  "Return KEY file from DATA."
  (when-let* ((file (ht-get data (or key "file")))
              (file (cogru-expand-path file)))
    file))

(defun cogru--data-point (data key)
  "Return point from DATA from KEY."
  (when-let* ((point (ht-get data key))
              (point (cogru-decode-point point)))
    point))

(defun cogru--data-contents (data)
  "Return contents from DATA."
  (when-let* ((contents (ht-get data "contents"))
              (contents (cogru-decode-str contents)))
    contents))

(defun cogru-address ()
  "Return the address name."
  (format "http://%s:%s" cogru-host cogru-port))

(defun cogru--connected-p ()
  "Return non-nil when is connected."
  (process-live-p cogru--process))

(defun cogru--under-path-p (&optional path)
  "Return non-nil if the PATH is under the workspace."
  (when-let ((path (or path (buffer-file-name) "")))
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

(defmacro cogru--ensure-under-file (file &rest body)
  "Run BODY only if client is under session FILE."
  (declare (indent 1))
  `(cogru--ensure-under-path
     (let* ((filename (buffer-file-name))
            (file (or ,file filename)))
       (when (and (equal file filename)
                  (file-exists-p filename))
         ,@body))))

;;
;;; History

(defun cogru-presorted-completions (string pred action completions)
  "Sort COMPLETIONS in it's order.

Argument STRING, PRED and ACTION are required parameters."
  (if (eq action 'metadata)
      `(metadata (display-sort-function . ,#'identity))
    (complete-with-action action completions string pred)))

(defun cogru-add-completion-history (elt history)
  "Add ELT to HISTORY."
  ;; Remove duplicate first.
  (set history (cl-delete elt (symbol-value history) :test 'equal))
  ;; Push it on top.
  (push elt (symbol-value history)))

;;
;;; Buffer

;; XXX: This is a hack to make sure the data will have the
;; same line endings with the server.
(defmacro cogru--ensure-coding-system (&rest body)
  "Run BODY with correct coding system."
  (declare (indent 0))
  ;; These variables are the default value on Windows, and these are
  ;; the only way I found to write files with correct CRLF line endings.
  `(let ((buffer-file-coding-system 'undecided-unix)
         (coding-system-for-write)
         (last-coding-system-used 'utf-8))
     ,@body))

(defmacro cogru--with-buffer (buffer-or-name &rest body)
  "Run BODY only when BUFFER-OR-NAME is safe to visit."
  (declare (indent 1))
  `(when-let* ((buffer (ignore-errors (get-buffer ,buffer-or-name)))
               ((buffer-live-p buffer)))
     (with-current-buffer buffer ,@body)))

(defmacro cogru--with-file-buffer (filename &rest body)
  "Run BODY only when FILENAME is safe to visit."
  (declare (indent 1))
  `(when-let* ((buffer (ignore-errors (get-file-buffer ,filename)))
               ((buffer-live-p buffer)))
     (with-current-buffer buffer ,@body)))

(defun cogru--revert-file (filename)
  "Revert FILENAME if the buffer is valid."
  (cogru--with-file-buffer filename
    (ignore-errors
      (revert-buffer :ignore-auto :noconfirm :preserve-modes))))

(defun cogru-buffer-string ()
  "Return the entire buffer string."
  (let ((buf (buffer-substring-no-properties (point-min) (point-max))))
    (if (eq 'dos (show-eol-get-current-system))
        (s-replace "\n" "\r\n" buf)
      buf)))

;;
;;; String

(defun cogru-2str (obj)
  "Convert OBJ to string."
  (format "%s" obj))

(defun cogru-encode-str (str)
  "Encode STR."
  (cond ((eq 'dos (show-eol-get-current-system))
         (s-replace "\n" "\r\n" str))
        (t str)))

(defun cogru-decode-str (str)
  "Decode STR."
  (cond ((eq 'dos (show-eol-get-current-system))
         (s-replace "\r\n" "\n" str))
        (t str)))

(defmacro cogru--safe-edit (&rest body)
  "Run BODY with no modification's side effect."
  (declare (indent 0))
  `(progn
     (let ((lsp-inhibit-lsp-hooks t)
           (cogru-inhibit-change-hooks)
           ;;(buffer-undo-list)
           (jit-lock-functions))
       (elenv-with-no-redisplay ,@body))
     (run-hooks 'cogru-after-edit-hook)))

(defun cogru--replace-buffer-contents (str)
  "Wrap function `replace-buffer-contents'

Replace current buffer contents with STR."
  (let ((tmp (get-buffer-create " *temp*")))
    (with-current-buffer tmp
      (cogru--ensure-coding-system
        (insert str)))
    (cogru--safe-edit
      (replace-buffer-contents tmp))
    (kill-buffer tmp)))

;;
;;; File

(defun cogru--write-file (path contents)
  "Write CONTENTS to PATH."
  (let ((exists (ignore-errors (file-exists-p path))))
    (ignore-errors (make-directory (file-name-directory path) t))
    (cogru--ensure-coding-system
      (msgu-silent (write-region contents nil path)))
    ;; Print status
    (if exists (message "Overwrote file %s" path)
      (message "Wrote file %s" path))))

(defun cogru--sync-file (filename contents)
  "Sync FILENAME with CONTENTS."
  (coru-with-progress
    (format "[Cogru] Syncing file %s..." filename)
    (msgu-silent (cogru--write-file filename contents)
                 (cogru--revert-file filename))
    (format "[Cogru] Syncing file %s... done!" filename)))

(defun cogru--buffer-diff-p (contents &optional buffer)
  "Return non-nil if CONTENTS is different then BUFFER's string."
  (with-current-buffer (or buffer (current-buffer))
    (not (equal (md5 contents) (md5 (buffer-string))))))

(defun cogru--sync-buffer (filename contents)
  "Sync FILENAME's buffer with CONTENTS."
  (cogru--with-file-buffer filename
    (when (cogru--buffer-diff-p contents)
      (coru-with-progress
        (format "[Cogru] Syncing buffer %s..." filename)
        (elenv-save-window-excursion
          (cogru--safe-edit
            (erase-buffer)
            (insert contents)))
        (format "[Cogru] Syncing buffer %s... done!" filename)))))

;;
;;; Project

(defun cogru-expand-path (path)
  "Convert PATH to project path."
  (ignore-errors (expand-file-name path cogru--path)))

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

(defmacro cogru--handle-request (data failure &rest success)
  "Handle DATA request; run BODY only when return success status."
  (declare (indent 2))
  `(let ((success (cogru--success-p ,data))
         (msg     (ht-get ,data "message")))
     (cond (success ,@success)
           (t (or ,failure
                  (message msg))))))

(defun cogru-encode-point (pos)
  "Encode POS to server's buffer space."
  (ignore-errors (1- (cogru-position-bytes pos))))

(defun cogru-decode-point (pos)
  "Encode POS to client's buffer space."
  (ignore-errors (cogru-re-point (1+ pos))))

;;
;;; Point

(defun cogru-position-bytes (pos)
  "Like function `position-bytes' but handle window line endings.

The argument POS is the point."
  (when pos
    (+ (position-bytes pos)
       (if (eq 'dos (show-eol-get-current-system))
           (how-many "\n" 0 pos)
         0))))

(defun cogru-re-point (pos)
  "Reverse formula of the function `cogru-position-bytes'.

Convert byte POS to text point."
  (when pos
    (byte-to-position
     (- pos
        (if (eq 'dos (show-eol-get-current-system))
            (let* ((buf (cogru-buffer-string))
                   (buf (s-replace "\n" "\r\n" buf)))
              (with-temp-buffer
                (insert buf)
                (how-many "\n" 0 (byte-to-position pos))))
          0)))))

(defun cogru-recenter ()
  "Recenter position to focus."
  (let ((recenter-positions '(middle)))
    (recenter-top-bottom)))

(defun cogru--point-visible-p (point)
  "Return non-nil if POINT is visibile on screen."
  (and (< point (window-end nil t))
       (<= (window-start) point)))

;;
;;; Overlay

(defun cogru-delete-overlay (ov)
  "Delete OV."
  (when (overlayp ov) (delete-overlay ov)))

(provide 'cogru-util)
;;; cogru-util.el ends here

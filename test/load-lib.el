;;; load-lib.el ---   -*- lexical-binding: t; -*-
;;; Commentary: Load this library once.
;;; Code:

(require 'cl-lib)

(require 'project)

(defun jcs-project-reload ()
  "Reload current el project."
  (interactive)
  (when-let* ((project (project-current))
              (files (project-files project))
              (files (cl-remove-if-not (lambda (filename)
                                         (string-suffix-p ".el" filename))
                                       files)))
    (dolist (filename files)
      (load-file filename))))

;;; load-lib.el ends here

;;; load-lib.el ---   -*- lexical-binding: t; -*-
;;; Commentary: Load this library once.
;;; Code:

(require 'cl-lib)

(require 'project)

(let* ((files (project-files (project-current)))
       (files (cl-remove-if (lambda (filename)
                              (or (not (string-suffix-p ".el" filename))
                                  (string-suffix-p "load-lib.el" filename)))
                            files)))
  (dolist (filename files)
    (load-file filename)))

;;; load-lib.el ends here

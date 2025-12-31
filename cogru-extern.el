;;; cogru-extern.el --- Externals module  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Shen, Jen-Chieh

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
;; Externals module.
;;

;;; Code:

(require 'cogru-util)

;;
;;; tree-sitter.el

(defvar tree-sitter-tree)
(declare-function tree-sitter-mode "ext:tree-sitter.el")
(declare-function tree-sitter--do-parse "ext:tree-sitter.el")

(defun cogru-tree-sitter--after-edit (&rest _)
  "Handle `tree-sitter' after edit."
  (when (bound-and-true-p tree-sitter-mode)
    (setq tree-sitter-tree nil)
    (tree-sitter--do-parse)))

(add-hook 'cogru-after-edit-hook #'cogru-tree-sitter--after-edit)

;;
;;; treesit.el

;; TODO: ..

(provide 'cogru-extern)
;;; cogru-extern.el ends here

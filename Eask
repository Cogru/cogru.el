;; -*- mode: eask; lexical-binding: t -*-

(package "cogru"
         "0.1.0"
         "Cogru plugin for real-time collaborative editing")

(website-url "https://github.com/Cogru/cogru.el")
(keywords "convenience" "cogru")

(package-file "cogru.el")
(files "cogru-*.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)
(source 'jcs-elpa)

(depends-on "emacs" "28.1")
(depends-on "elenv")
(depends-on "msgu")
(depends-on "s")
(depends-on "ht")
(depends-on "log4e")
(depends-on "posframe")
(depends-on "named-timer")
(depends-on "show-eol")

(development
 (depends-on "ic")
 (depends-on "tree-sitter"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

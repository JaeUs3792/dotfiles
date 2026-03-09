;;; init-const.el -*- lexical-binding: t -*-
(defconst ON-LINUX (eq system-type 'gnu/linux)
  "Under Linux system")
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos))
  "Under Windows System")
(defconst custom-default-file
  (expand-file-name "custom-default.el" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-const)

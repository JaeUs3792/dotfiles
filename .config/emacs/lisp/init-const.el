;;; init-const.el -*- lexical-binding: t -*-
(defconst ON-LINUX (eq system-type 'gnu/linux)
		  "Under Linux system")
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos))
		  "Under Windows System")

(provide 'init-const)

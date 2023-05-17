;;; init-func.el -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'init-const)

(defun font-installed-p (font-name)
  (find-font (font-spec :name font-name)))
(defun too-long-file-p ()
  "Check whether the file is too long."
  (if (fboundp 'buffer-line-statistics)
      (> (car (buffer-line-statistics)) 10000)
    (> (buffer-size) 100000)))
(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (or (featurep 'nerd-icons)
      (require 'nerd-icons nil t)))

(provide 'init-func)
;;; init-func.el ends here

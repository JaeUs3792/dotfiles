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


(provide 'init-func)
;;; init-func.el ends here

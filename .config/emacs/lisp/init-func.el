;;; init-func.el -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'init-const)

(defun font-installed-p (font-name)
  (find-font (font-spec :name font-name)))

(provide 'init-func)
;;; init-func.el ends here

;;; init-package.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

(when (and (file-exists-p custom-default-file)
           (not (file-exists-p custom-file)))
  (copy-file custom-default-file custom-file))

(and (file-readable-p custom-file) (load custom-file))


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-package--warning-displayed t)
(setq use-package-always-ensure t)

; hide info in modeline
(use-package diminish
  :straight t
  :ensure t)

(provide 'init-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here

;;; init-package.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

(when (and (file-exists-p custom-default-file)
           (not (file-exists-p custom-file)))
  (copy-file custom-default-file custom-file))

(and (file-readable-p custom-file) (load custom-file))


; (when (version< emacs-version "28")
;   (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
; (add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

; (customize-set-variable 'package-archive-priorities
;                         '(("gnu"    . 99)   ; prefer GNU packages
;                           ("nongnu" . 80)   ; use non-gnu packages if
;                                         ; not found in GNU elpa
;                           ("stable" . 70)   ; prefer "released" versions
;                                         ; from melpa
;                           ("melpa"  . 0)))  ; if all else fails, get it
;                                         ; from melpa

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
(setq use-package-always-ensure t)

; hide info in modeline
(use-package diminish
  :straight t
  :ensure t)

(provide 'init-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here

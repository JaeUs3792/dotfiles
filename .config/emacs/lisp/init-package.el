;;; init-package.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

(when (and (file-exists-p custom-default-file)
           (not (file-exists-p custom-file)))
  (copy-file custom-default-file custom-file))

(and (file-readable-p custom-file) (load custom-file))

;; HACK: DO NOT save package-selected-packages to `custom-file'.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-package--save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'my-package--save-selected-packages)))
(advice-add 'package--save-selected-packages :override #'my-package--save-selected-packages)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))
;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package))

(use-package diminish)
(use-package bind-key)

(use-package paradox
  :custom-face
  (paradox-archive-face ((t (:inherit font-lock-doc-face))))
  (paradox-description-face ((t (:inherit completions-annotations))))
  :hook
  (emacs-startup . paradox-enable)
  :init
  (setq paradox-execute-asynchronously t
        paradox-github-token nil
        paradox-display-star-count nil
        paradox-status-face-alist ;
        '(("built-in"   . font-lock-builtin-face)
          ("available"  . success)
          ("new"        . (success bold))
          ("held"       . font-lock-constant-face)
          ("disabled"   . font-lock-warning-face)
          ("avail-obso" . font-lock-comment-face)
          ("installed"  . font-lock-comment-face)
          ("dependency" . font-lock-comment-face)
          ("incompat"   . font-lock-comment-face)
          ("deleted"    . font-lock-comment-face)
          ("unsigned"   . font-lock-warning-face)))
  :config
  (add-hook 'paradox-after-execute-functions
            (lambda (_)
              "Display `page-break-lines' in \"*Paradox Report*\" buffer."
              (when (fboundp 'page-break-lines-mode)
                (let ((buf (get-buffer "*Paradox Report*"))
                      (inhibit-read-only t))
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (page-break-lines-mode 1))))))
            t))




(provide 'init-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here

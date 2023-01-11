;;; init.el -*- lexical-binding: t; -*-
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Crafted Emacs loaded in %s"
                     (emacs-init-time))))

(require 'package)
(add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

(add-hook 'before-save-hook #'whitespace-cleanup)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                dired-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")
(setq undo-limit 100000000
      auto-save-default t)

(set-default-coding-systems 'utf-8)
(customize-set-variable 'large-file-warning-threshold 100000000) ;; 100MB

(defconst ON-LINUX (eq system-type 'gnu/linux))
(defconst ON-MAC (eq system-type 'darwin))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(setq gc-cons-threshold (* 2 1024 1024)) ; decreasing the threshold to 2MB

(defvar my-config-file (expand-file-name "config.el" user-emacs-directory))
(when (file-exists-p my-config-file)
  (load my-config-file nil 'nomessage))

;;(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

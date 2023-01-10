;;; init.el -*- lexical-binding: t; -*-

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Crafted Emacs loaded in %s."
                     (emacs-init-time))))

(when (eq crafted-package-system 'package)
  (crafted-package-initialize))

;; Add the modules folder to the load path
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)
(customize-set-variable 'visible-bell 1)  ; turn off beeps, make them flash!
(customize-set-variable 'large-file-warning-threshold 100000000) ;; change to ~100 MB

(defun crafted-ensure-package (package &optional args)
  "Ensure that PACKAGE is installed on the system, either via
package.el or Guix depending on the value of
`crafted-prefer-guix-packages'."
  (if crafted-prefer-guix-packages
      (unless (featurep package)
        (message "Package '%s' does not appear to be installed by Guix: " package))
    (crafted-package-install-package package)))

;; Check the system used
(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-BSD     (or ON-MAC (eq system-type 'berkeley-unix)))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))


;; Defines the user configuration var and etc folders
;; and ensure they exist.
(defvar crafted-config-etc-directory (expand-file-name "etc/" crafted-config-path)
  "The user's configuration etc/ folder.")
(defvar crafted-config-var-directory (expand-file-name "var/" crafted-config-path)
  "The user's configuration var/ folder.")
(mkdir crafted-config-etc-directory t)
(mkdir crafted-config-var-directory t)

;; Find the user configuration file
(defvar crafted-config-file (expand-file-name "config.el" "~/.config/emacs")
  "The user's configuration file.")
;; Load the user configuration file if it exists
(when (file-exists-p crafted-config-file)
  (load crafted-config-file nil 'nomessage))

;; Make GC pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

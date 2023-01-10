(require 'crafted-defaults)		; Sensible Default settings for Emacs
(require 'crafted-updates)     ; Tools to upgrade Crafted Emacs
(require 'crafted-completion)	; selection framework based on 'vertico'
(require 'crafted-ui)			; Better UI experience (modeline etc..)
;;(require 'crafted-windows)     ; Window management configuration
(require 'crafted-evil)		; An 'evil-mode' configuration
;;(require 'custom-evil)			; An 'evil-mode' configuration

(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

(setq default-input-method "korean-hangul")
(global-set-key (kbd "<Hangul>") 'toggle-input-method)

(setq user-full-name "JaeYoo-Im"
      user-mail-address "cpu3792@gmail.com")

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                vterm-mode-hook
                dired-mode-hook
                ;;shell-mode-hook
                ;;treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (custom-set-faces
             `(default ((t (:font "Fira Code 10"))))
             `(fixed-pitch ((t (:inherit (default)))))
             `(fixed-pitch-serif ((t (:inherit (default)))))
             `(variable-pitch ((t (:font "Ubuntu 10")))))))
(set-fontset-font t `hangul (font-spec :name "NanumGothic"))

;; loading themes
(crafted-package-install-package 'doom-themes)
(progn
  (disable-theme 'deeper-blue)          ; first turn off the deeper-blue theme
  (if (display-graphic-p)
      (load-theme 'doom-palenight t)
    (load-theme 'doom-gruvbox t))
  (unless (display-graphic-p)
    (xterm-mouse-mode)))

(require 'custom-dashboard)

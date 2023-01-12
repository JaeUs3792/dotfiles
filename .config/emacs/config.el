;;; config.el -*- lexical-binding: t; -*-
(setq user-full-name "JaeYoo-Im"
      user-mail-address "cpu3792@gmail.com")

;;(add-to-list 'default-frame-alist `(font . "Fira Code Retina"))
;;(set-face-attribute 'default nil :font "Fira Code Retina" :height 12)
;;(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 12)
;;(set-face-attribute 'variable-pitch nil :font "Fira Code Retina" :height 12 :weight 'regular)
;;(set-fontset-font t 'hangul (font-spec :family "NanumGothic" :height 12))

(add-hook 'emacs-startup-hook
          (lambda ()
            (custom-set-faces
             `(default ((t (:font "Fira Code Retina 12"))))
             `(fixed-pitch ((t (:inherit (default)))))
             `(fixed-pitch-serif ((t (:inherit (default)))))
             `(variable-pitch ((t (:font "Ubuntu 12")))))))

(require 'custom-ui)
(require 'custom-keybindings)
(require 'custom-org)
(require 'custom-completion)

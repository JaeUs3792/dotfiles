;;; config.el -*- lexical-binding: t; -*-
(setq user-full-name "JaeYoo-Im"
      user-mail-address "cpu3792@gmail.com")

(add-to-list 'default-frame-alist `(font . "Fira Code Retina"))
(set-face-attribute 'default nil :font "Fira Code Retina" :height 12)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 12)
(set-face-attribute 'variable-pitch nil :font "Fira Code Retina" :height 12 :weight 'regular)
(set-fontset-font t 'hangul (font-spec :family "NanumGothic" :height 12))

(require 'custom-ui)
(require 'custom-keybindings)

;;; early-init.el -*- lexical-binding: t; -*-
;; garbage collection
(setq gc-cons-threshold (* 50 1024 1024)) ;; 50MB
;; prefers newest version of a file
(customize-set-variable 'load-prefer-newer t)

(setq package-enable-at-startup nil
      inhibit-startup-message t frame-resize-pixelwise t
      package-native-compile t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
;;(blink-cursor-mode 0)

;; initial load with blue theme
(load-theme 'deeper-blue)

(customize-set-variable 'initial-major-mode 'fundamental-mode)

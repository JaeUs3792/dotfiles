;;; early-init.el -*- lexical-binding: t -*-
;; do not garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
;; prefers newest version of a file
(customize-set-variable 'load-prefer-newer t)

;;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-deferred-compilation nil))


(setq package-enable-at-startup nil)
;; built-in use-package, set before loading
(setq use-package-enable-imenu-support t)

;;
(setq non-prefer-newer noninteractive)
(setq frame-inhibit-implied-resize t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(setq-default mode-line-format nil)

(setq inhibit-startup-message nil)

;; initial load with blue theme
;(load-theme 'deeper-blue)

;;(customize-set-variable 'initial-major-mode 'fundamental-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; early-init.el ends here

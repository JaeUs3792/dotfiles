;;; early-init.el -*- lexical-binding: t; -*-

;;; Garbage collection
;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;;; Emacs lisp source/compiled preference
;; Prefer loading newest compiled .el file
(customize-set-variable 'load-prefer-newer t)


(defvar crafted-config-path "~/.cache/emacs")
(unless (file-exists-p crafted-config-path)
  (mkdir crafted-config-path t))

;;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)

  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)

  ;; Set the right directory to store the native compilation cache
  ;; NOTE the method for setting the eln-cache directory depends on the emacs version
  (when (fboundp 'startup-redirect-eln-cache)
    (if (version< emacs-version "29")
        (add-to-list 'native-comp-eln-load-path (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))
      (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))))

  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;;; UI configuration
;; Remove some unneeded UI elements (the user can turn back on anything they wish)
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mouse-color . "white") default-frame-alist)

;; Loads a nice blue theme, avoids the white screen flash on startup.
(load-theme 'deeper-blue t)

;; Make the initial buffer load faster by setting its mode to fundamental-mode
(customize-set-variable 'initial-major-mode 'fundamental-mode)

;;; Package system
;; Load the package-system.  If needed, the user could customize the
;; system to use in `early-config.el'.
(defvar crafted-bootstrap-directory (expand-file-name "bootstrap/" user-emacs-directory)
  "Package system bootstrap configuration.")

(load (expand-file-name "crafted-package.el" crafted-bootstrap-directory))
(when (eq crafted-package-system 'package)
  ;; needed in case `early-config.el' has pinned packages configured
  (require 'package))

;; this is the default
;; (setq crafted-package-system 'package)
;; use this in `early-config.el' to switch to `straight.el'
;; (setq crafted-package-system 'straight)
(crafted-package-bootstrap crafted-package-system)

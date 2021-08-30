;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; Initial Setup
(setq user-full-name "JaeYoo,Im"
      user-mail-address "cpu3792@gmail.com")
(set-default-coding-systems 'utf-8)
(setq default-input-method "korean-hangul")

;; UI
;(set-frame-parameter (selected-frame) 'alpha '(80 . 80))
;(add-to-list 'default-frame-alist '(alpha . (80 . 80)))
;(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;(add-to-list 'default-frame-alist '(fullscreen . maximized))
(scroll-bar-mode 1)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Org/")
;;(setq org-roam-directory "~/Roam/")
(setq org-agenda-files
      '("~/Roam/Agenda/Agenda-2021-6th.org"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(after! org
  (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("hk" . "src haskell")))


;; Roam
;;(use-package org-roam-server
;;  :ensure t
;;  :config
;;  (setq org-roam-server-host "127.0.0.1"
;;        org-roam-server-port 8080
;;        org-roam-server-authenticate nil
;;        org-roam-server-export-inline-images t
;;        org-roam-server-serve-files nil
;;        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;        org-roam-server-network-poll t
;;        org-roam-server-network-arrows nil
;;        org-roam-server-network-label-truncate t
;;        org-roam-server-network-label-truncate-length 60
;;        org-roam-server-network-label-wrap-length 20))
;;
;;(defun org-roam-server-open ()
;;    "Ensure the server is active, then open the roam graph."
;;    (interactive)
;;    (smartparens-global-mode -1)
;;    (org-roam-server-mode 1)
;;    (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))
;;    (smartparens-global-mode 1))
;;
;;;; automatically enable server-mode
;;(after! org-roam
;;  (smartparens-global-mode -1)
;;  (org-roam-server-mode)
;;  (smartparens-global-mode 1))

;; Dired
(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file)
(add-hook 'dired-mode-hook 'dired-hide-dotfiles-mode)
(evil-define-key 'normal 'dired-mode-map
  (kbd "H") 'dired-hide-dotfiles-mode)

;; Tramp
(setq tramp-default-method "ssh")
(customize-set-variable 'tramp-inline-compress-start-size nil)
(customize-set-variable 'tramp-copy-size-limit nil)

;; Open with
(require 'openwith)
(setq large-file-warning-threshold nil)
(after! dired
  (openwith-mode t)
  (setq openwith-associations
        (list (list (openwith-make-extension-regexp
                     '("mpg" "mpeg" "mp3" "mp4" "m4v"
                       "avi" "wmv" "wav" "mov" "flv"
                       "ogm" "ogg" "mkv"))
                    "mpv"
                    '(file)))))
;; Nov mode
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Very large file
(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(use-package dap-mode
  :ensure
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
         :gdbpath "rust-lldb"
         :target nil
         :cwd nil))
  (dap-register-debug-template
   "Rust::GDB Run Configuration"
   (list :type "gdb"
         :request "launch"
         :name "GDB::Run"
         :gdbpath "rust-gdb"
         :target nil
         :cwd nil)))

;; Time
(setq display-time-world-list
      '(("Etc/UTC" "UTC")
        ("Asia/Seoul" "Seoul")
        ("Asia/Shanghai" "Shanghai")))
(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")


;; Key binding
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'swiper)
(map! :leader
      (:prefix-map ("e" . "eshell")
       :desc "open e-shell" "s" #'eshell))


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; -*- lexical-binding: t; -*-
;;User
(setq user-full-name "JaeYoo,Im"
      user-mail-address "cpu3792@gmail.com")
;; os
(setq custom-file (make-temp-file "emacs-custom"))
(setq windows? (eq system-type 'windows-nt))
(setq mac? (eq system-type 'darwin))
(setq large-file-warning-threshold nil)

(setq system-time-locale "C")
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
;; theme var
(defvar gvar/default-font-size 110)
(defvar gvar/default-variable-font-size 110)
(defvar gvar/frame-transparency '(75 . 75))
;;dir
(setq user-emacs-directory "~/.cache/emacs")
;; encoding
(set-language-environment "Korean")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
;; font
(defun available-font? (font) (member font (font-family-list)))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         gvar/frame-transparency '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
;;(unless package-archive-contents
;;  (package-refresh-contents))
(package-initialize)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;(use-package auto-package-update
;;  :custom
;;  (auto-package-update-interval 7)
;;  (auto-package-update-prompt-before-update t)
;;  (auto-package-update-hide-results t)
;;  :config
;;  (auto-package-update-maybe)
;;  (auto-package-update-at-time "09:00"))

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
;; Use package install from git
(straight-use-package 'use-package)

(use-package paradox
  :init
  (setq paradox-github-token t)
  (setq paradox-execute-asynchronously t)
  (setq paradox-automatically-star t))

;; tab
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)
;; verilog
(custom-set-variables
 '(verilog-indent-level 4)
 '(verilog-indent-level-module 0)
 '(verilog-indent-level-declaration 0)
 '(verilog-indent-level-behavioral 0)
 '(verilog-indent-level-directive 0)
 '(verilog-cexp-indent 0)
 '(verilog-case-indent 4)

 '(verilog-auto-newline nil))

(use-package no-littering) ; make ~/.emacs.d clean
;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-message t)

;;  (setq gc-cons-threshold most-positive-fixnum)
;;  (defconst 1mb 1048576)
;;  (defconst 20mb 20971520)
;;  (defconst 30mb 31457280)
;;  (defconst 50mb 52428800)
;;
;;  (defun fk/defer-garbage-collection ()
;;    (setq gc-cons-threshold most-positive-fixnum))
;;
;;  (defun fk/restore-garbage-collection ()
;;    (run-at-time 1 nil (lambda () (setq gc-cons-threshold 30mb))))
;;
;;  (add-hook 'emacs-startup-hook 'fk/restore-garbage-collection 100)
;;  (add-hook 'minibuffer-setup-hook 'fk/defer-garbage-collection)
;;  (add-hook 'minibuffer-exit-hook 'fk/restore-garbage-collection)
;;
;;  (setq read-process-output-max 1mb)  ;; lsp-mode's performance suggest

;; Using garbage magic hack.
(use-package gcmh
  :config
  (gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Silence compiler warnings as they can be pretty disruptive (setq comp-async-report-warnings-errors nil)

(defun ju/evil-hook()
  (dolist (mode '(custom-mode
                  erc-mode))
    (add-to-list 'evil-emacs-state-modes mode)))
(use-package undo-tree
  :init
  (global-undo-tree-mode 1))
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-tree)
  :config
  (add-hook 'evil-mode-hook 'ju/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;;(setq-default evil-symbol-word-search t) ;; evil can identify the word with underscore.
  ;; Http://blog.binchen.org/posts/auto-complete-word-in-emacs-mini-buffer-when-using-evil.html
  ;;(defun minibuffer-inactive-mode-hook-setup ()
  ;;;; make `try-expand-dabbrev' from `hippie-expand' work in mini-buffer
  ;;;; @see `he-dabbrev-beg', so we need re-define syntax for '/'
  ;;(set-syntax-table (let* ((table (make-syntax-table)))
  ;;(modify-syntax-entry ?/ "." table)
  ;;table)))
  ;;(add-hook 'minibuffer-inactive-mode-hook 'minibuffer-inactive-mode-hook-setup)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
(use-package evil-numbers
  :after evil
  :config
  ;; unfortunately C-x is emacs common key binding.
  (define-key evil-normal-state-map (kbd "g =") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "g -") 'evil-numbers/dec-at-pt)
  (define-key evil-visual-state-map (kbd "g =") 'evil-numbers/inc-at-pt)
  (define-key evil-visual-state-map (kbd "g -") 'evil-numbers/dec-at-pt))
(use-package evil-nerd-commenter
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "g c") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "g c") 'evilnc-comment-or-uncomment-lines))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(modify-syntax-entry ?_ "w")
;;(defalias 'forward-evil-word 'forward-evil-symbol) ;; include underscore to word

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(xterm-mouse-mode)

(menu-bar-mode -1)            ; Disable the menu bar
;;(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                dired-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package doom-themes
  :init
  (setq doom_themes-enable-bold t
        doom-themes-enablbe-italic t)
  :config
  ;;(load-theme 'doom-palenight t)
  ;;(load-theme 'doom-gruvbox t)
  ;;(load-theme 'doom-one t)
  (load-theme 'doom-dracula t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; hilight line at the cursor.
(global-hl-line-mode t)
;;(use-package beacon
;;  :straight t
;;  :config
;;  (beacon-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha gvar/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,gvar/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;;(setq dashboard-banner-logo-title "Emacs is more than a text editor!")
  ;;(dashboard-startup-banner 'logo)
  (setq dashboard-startup-banner "/home/jaeus/.emacs.d/logos/black-hole.png")
  (setq dashboard-center-content t)
  (setq dashboard-week-agenda t)
  (setq dashboard-agenda-time-string-format "%d/%m/%Y %A %H:%M")
  (setq dashboard-items '((recents . 10)
                          (agenda . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  (setq dashboard-set-navigator t)
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons
        `((;; Github
           (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Github"
            "Browse github"
            (lambda (&rest _) (browse-url "https://github.com/JaeYoo-Im/")))
           (,(all-the-icons-octicon "history" :height 1.1 :v-adjust 0.0)
            "Reload last session"
            "Reload last session"
            (lambda (&rest _) (persp-state-load persp-state-default-file))))))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

;; NanumGothicCoding
;;(add-to-list 'default-frame-alist `(font . "NanumGothicCoding"))
;;(set-face-attribute 'default nil :font "NanumGothicCoding" :height gvar/default-font-size)
;;(set-face-attribute 'fixed-pitch nil :font "NanumGothicCoding" :height gvar/default-font-size)
;;(set-face-attribute 'variable-pitch nil :font "NanumGothicCoding" :height gvar/default-variable-font-size :weight 'regular)
;;(set-fontset-font t 'hangul (font-spec :family "NanumGothicCoding" :height gvar/default-font-size))

;; D2Coding
;;(add-to-list 'default-frame-alist `(font . "D2Coding"))
;;(set-face-attribute 'default nil :font "D2Coding" :height gvar/default-font-size)
;;(set-face-attribute 'fixed-pitch nil :font "D2Coding" :height gvar/default-font-size)
;;(set-face-attribute 'variable-pitch nil :font "D2Coding" :height gvar/default-variable-font-size :weight 'regular)
;;(set-fontset-font t 'hangul (font-spec :family "D2Coding" :height gvar/default-font-size))

;; Fira Code Retina(English) / D2Coding(Hangul) - Fixed pitch only
(add-to-list 'default-frame-alist `(font . "Fira Code Retina"))
(set-face-attribute 'default nil :font "Fira Code Retina" :height gvar/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height gvar/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Fira Code Retina" :height gvar/default-variable-font-size :weight 'regular)
(set-fontset-font t 'hangul (font-spec :family "D2Coding" :height gvar/default-font-size))
(setq face-font-rescale-alist
      '(("D2Coding" . 1.25)))

;; Dejavu Sans Mono(English) / D2Coding(Hangul) - Fixed pitch only
;;(add-to-list 'default-frame-alist `(font . "Dejavu Sans Mono"))
;;(set-face-attribute 'default nil :font "Dejavu Sans Mono" :height gvar/default-font-size)
;;(set-face-attribute 'fixed-pitch nil :font "Dejavu Sans Mono" :height gvar/default-font-size)
;;(set-face-attribute 'variable-pitch nil :font "Dejavu Sans Mono" :height gvar/default-variable-font-size :weight 'regular)
;;(set-fontset-font t 'hangul (font-spec :family "D2Coding" :height gvar/default-font-size))
;;(setq face-font-rescale-alist
;;      '(("D2Coding" . 1.25)))
;;(setq-default line-spacing 2)
;;(global-prettify-symbols-mode +1)

;; somtimes need to check white space. this configuration will be helpful
;;(custom-set-faces
;; ;; custom-set-faces was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.
;; ;; Your init file should contain only one such instance.
;; ;; If there is more than one, they won't work right.
;; '(whitespace-line ((nil (:bold t :background "yellow"))))
;; '(whitespace-tab ((nil (:bold t :background "linen"))))
;; '(whitespace-trailing ((nil (:bold t :background "red1")))))
;;(global-whitespace-mode t)
;;(add-hook
;; 'after-change-major-mode-hook
;; '(lambda ()
;;    (setq whitespace-line-column nil
;;          whitespace-style '(face trailing))))
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-create-definer ju/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(ju/leader-key-def
  "." 'find-file
  ;; Buffer
  "b" '(:ignore t :which-key "buffer handling")
  "b b" '(ibuffer :which-key "IBuffer")
  "b k" '(kill-current-buffer :which-key "Kill current buffer")
  "b n" '(next-buffer :which-key "Next buffer")
  "b p" '(previous-buffer :which-key "Previous buffer")
  "b B" '(ibuffer-list-buffers :which-key "IBuffer List Buffers")
  "b K" '(kill-buffer :which-key "IBuffer Kill Buffers")
  ;; Eshell
  "e" '(:ignore t :which-key "eshell")
  "e h" '(counsel-esh-history :which "Kill history")
  "e s" '(eshell :which "run eshell")
  ;; Workspace
  ;; Counsel
  "f" '(:ignore t :which-key "file op.")
  "f r" '(consult-recent-file :which-key "Recent files")
  "f R" '(revert-buffer :which-key "Revert Buffer")
  "t t" '(toggle-truncate-lines :which-key "Toggle truncate lines")
  ;; Shortcut
  "f d d" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/desktop.org")) :which-key "open exwm config")
  "f d e" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/emacs.org")) :which-key "open emacs config"))

;; Key binding
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; this annoying binding.
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "S-SPC"))    ;; use only S-\

(use-package diminish)
(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; tramp default is scp
(setq tramp-default-method "ssh")

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :config
  (setq company-idle-delay 0
        company-show-numbers "on"
        company-dabbrev-downcase nil
        )
  (add-to-list 'company-backends 'org-keyword-backend)
  )
;;(use-package company
;;:after lsp-mode
;;:hook (lsp-mode . company-mode)
;;:bind (:map company-active-map
;;("<tab>" . company-complete-selection))
;;(:map lsp-mode-map
;;("<tab>" . company-indent-or-complete-common))
;;:custom
;;(company-minimum-prefix-length 1)
;;(setq company-show-numbers "on")
;;(company-idle-delay 0.0))

;;(use-package company-box
;;:hook (company-mode . company-box-mode))

(defun org-keyword-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'org-keyword-backend))
    (prefix (and (eq major-mode 'org-mode)
                 (cons (company-grab-line "^#\\+\\(\\w*\\)" 1)
                       t)))
    (candidates (mapcar #'upcase
                        (cl-remove-if-not
                         (lambda (c) (string-prefix-p arg c))
                         (pcomplete-completions))))
    (ignore-case t)
    (duplicates t)))

(use-package prescient
  :hook (dashboard-after-initialize . prescient-persist-mode))
;;(use-package company-prescient
;;  :after company
;;  :config (company-prescient-mode))

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode t))

(use-package yasnippet-snippets)
(use-package yasnippet
  :init
  (defvar yas-snippet-dirs nil)
  :hook (prog-mode . yas-minor-mode)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode 1))
(ju/leader-key-def
  "i" '(:ignore t :which-key "insert something.")
  "is" '(yas-insert-snippet :which-key "snippet")
  "ie" '(emojify-insert-emoji :which-key "emoji"))

(setq display-time-world-list
      '(("Etc/UTC" "UTC")
        ("Asia/Seoul" "Seoul")
        ("Asia/Shanghai" "Shanghai")))
(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")

;;  (use-package ivy
;;    :diminish
;;    :bind (("C-s" . swiper)
;;           :map ivy-minibuffer-map
;;           ("TAB" . ivy-alt-done)
;;           ("C-l" . ivy-alt-done)
;;           ("C-j" . ivy-next-line)
;;           ("C-k" . ivy-previous-line)
;;           :map ivy-switch-buffer-map
;;           ("C-k" . ivy-previous-line)
;;           ("C-l" . ivy-done)
;;           ("C-d" . ivy-switch-buffer-kill)
;;           :map ivy-reverse-i-search-map
;;           ("C-k" . ivy-previous-line)
;;           ("C-d" . ivy-reverse-i-search-kill))
;;    :config
;;    (ivy-mode 1))
;;
;;  (use-package ivy-rich
;;    :after ivy
;;    :init
;;    (ivy-rich-mode 1)
;;    (ivy-rich-project-root-cache-mode 1))
;;
;;  (use-package ivy-posframe
;;    ;;:disabled
;;    :after ivy
;;    :diminish
;;    :init
;;    (setq ivy-posframe-display-functions-alist
;;          '((swiper                     . ivy-posframe-display-at-point)
;;            (emojify-insert-emoji  . ivy-display-function-fallback)
;;            (t                        . ivy-posframe-display)))
;;    :custom-face
;;    (ivy-posframe-border ((t (:background "#ffffff"))))
;;    :config
;;    (setq ivy-posframe-height-minibuffer nil)
;;    (setq ivy-posframe-parameters '((internal-border-width . 1)))
;;    (ivy-posframe-mode 1))
;;
;;  (use-package ivy-prescient
;;    :after counsel
;;    :custom
;;    (ivy-prescient-enable-filtering nil)
;;    :config
;;    ;; Uncomment the following line to have sorting remembered across sessions!
;;                                          ;(prescient-persist-mode 1)
;;    (ivy-prescient-mode 1))
;;  (use-package all-the-icons-ivy
;;    :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

;;  (use-package counsel
;;    :bind (("M-x" . counsel-M-x)
;;           ;;("C-M-j" . counsel-switch-buffer)
;;           :map minibuffer-local-map
;;           ("C-r" . 'counsel-minibuffer-history))
;;    :custom
;;    (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
;;    :config
;;    ;; Don't use ^ as initial input. Set this here because `counsel' defines more
;;    ;; of its own, on top of the defaults.
;;    (setq ivy-initial-inputs-alist nil)
;;    (counsel-mode 1))

(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))
(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))
(use-package vertico-posframe
  :disabled
  :after vertico
  :ensure t
  :init
  (setq vertico-posframe-parameters
        `((left-fringe . 8)
          (right-fringe . 8) (alpha . 100)))
  (vertico-posframe-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package embark
  :bind (("C-S-a" . embark-act)
         :map minibuffer-local-map
         ("C-d" . embark-act))
  :config

  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(use-package embark-consult
  :after embark)

(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))
(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :custom
  (consult-dir-project-list-function nil))

(use-package avy
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line))
(ju/leader-key-def
  "v" '(:ignore t :which-key "Avy")
  "vc" '(avy-goto-char :which-key "Avy Goto Char")
  "vw" '(avy-goto-word-0 :which-key "Avy Goto Word")
  "vl" '(avy-goto-line :which-key "Avy Goto Line"))

(defun dw/switch-project-action ()
  "Switch to a workspace with the project name and start `magit-status'."
  (persp-switch (projectile-project-name))
  (magit-status))
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'hybrid)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'dw/switch-project-action))

(use-package counsel-projectile
  :disabled
  :after projectile
  :config (counsel-projectile-mode))
(ju/leader-key-def
  "p" '(:ignore t :which-key "Projectile")
  "p." 'projectile-find-file
  "ps" 'projectile-switch-project
  "pg" 'consult-ripgrep
  "pc" 'projectile-compile-project
  "pd" 'projectile-dired)
;;(defun dw/switch-project-action ()
;;  "Switch to a workspace with the project name and start `magit-status'."
;;  ;; TODO: Switch to EXWM workspace 1?
;;  (persp-switch (projectile-project-name))
;;  (magit-status))

(use-package perspective
  :demand t
  :bind (("C-M-j" . consult-buffer)
         ("C-M-k" . persp-switch)
         ("C-M-n" . persp-next)
         ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-initial-frame-name "Main")
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode)))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . describe-variable)
  ([remap describe-key] . helpful-key))

;;(use-package hydra
;;  :defer t)

;;(defhydra hydra-text-scale (:timeout 4)
;;  "scale text"
;;  ("j" text-scale-increase "in")
;;  ("k" text-scale-decrease "out")
;;  ("f" nil "finished" :exit t))

;;(gvar/leader-keys
;; "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold))))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  ;;(variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config

  (setq org-todo-keywords
        '((sequence "TODO(t)" "HOLD(h)" "|" "KILL(k)" "DONE(d)")))
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  (setq org-src-fontify-natively t)
  (setq org-fontify-quote-and-verse-blocks t)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8)))
(use-package org-superstar :after org :hook (org-mode . org-superstar-mode))
(use-package org-attach-screenshot)
(use-package org-download)

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (latex . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
  (add-to-list 'org-structure-template-alist '("cc" . "src c"))
  (add-to-list 'org-structure-template-alist '("cp" . "src c++"))
  (add-to-list 'org-structure-template-alist '("rs" . "src rust"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("oc" . "src octave"))
  (add-to-list 'org-structure-template-alist '("vl" . "src verilog"))
  (add-to-list 'org-structure-template-alist '("vh" . "src vhdl"))
  (org-reload))

;; ODT export to docx
(setq org-odt-preferred-output-format "docx")

;; Do not ask when run code block
(setq org-confirm-babel-evaluate nil)


(ju/leader-key-def
  "o" '(:ignore t :which-key "Org mode")
  "oa" '(:ignore t :which-key "Org Agenda")
  "oar" '(my/org-roam-refresh-agenda-list :which-key "Org agenda refresh list")
  "oaa" '(org-agenda :which-key "open org agenda"))

(use-package org-contrib)

(use-package org-roam
  :ensure t
  :demand t ;; Ensure org-roam is loaded by default
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Roam")
  (org-roam-completion-everywhere t)

  :config
  (org-roam-db-autosync-mode))
  ;;(org-roam-setup))
(ju/leader-key-def
  "r" '(:ignore t :which-key "Org Roam")
  "ru" 'org-roam-ui-open
  "rl" 'org-roam-buffer-toggle
  "rf" 'org-roam-node-find
  "ri" 'org-roam-node-insert
  "rI" 'org-roam-node-insert-immediate
  "rp" 'my/org-roam-find-project)

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

;; Build the agenda list the first time for the session
(my/org-roam-refresh-agenda-list)

(defun my/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Project")
   :templates
   '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* History\n\n* Notes\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t))))

(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (my/org-roam-copy-todo-to-today))))

;;(use-package org-roam-server
;;  :ensure t
;;  :config
;;  (setq org-roam-server-host "127.0.0.1"
;;        org-roam-server-port 23799
;;        org-roam-server-authenticate nil
;;        org-roam-server-export-inline-images t
;;        org-roam-server-serve-files t
;;        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;        org-roam-server-network-poll t
;;        org-roam-server-network-arrows nil
;;        org-roam-server-network-label-truncate t
;;        org-roam-server-network-label-truncate-length 60
;;        org-roam-server-network-label-wrap-length 20))

(use-package websocket
  :after org-roam)
(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(defun dw/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun dw/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-bloc (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-display-inline-images)
  (dw/org-present-prepare-slide))

(defun dw/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images))

(defun dw/org-present-prev ()
  (interactive)
  (org-present-prev)
  (dw/org-present-prepare-slide))

(defun dw/org-present-next ()
  (interactive)
  (org-present-next)
  (dw/org-present-prepare-slide))

(use-package org-present
  :bind (:map org-present-mode-keymap
              ("C-c C-j" . dw/org-present-next)
              ("C-c C-k" . dw/org-present-prev))
  :hook ((org-present-mode . dw/org-present-hook)
         (org-present-mode-quit . dw/org-present-quit-hook)))

(use-package ob-translate
  :config
  (setq ob-translate:default-dest "ko"))

(use-package pdf-tools
  :straight t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))
(use-package org-pdfview
  :straight t)
(use-package org-noter)

(setq org-publish-use-timestamps-flag nil)
(setq org-export-with-broken-lilnks t)
(setq org-publish-project-alist
      '(("jaeus.net"
        :base-directory "~/Roam/www/"
        :base-extension "org"
        :publishing-directory "~/Projects/html/"
        :recursive t
        :publishing-function org-html-publish-to-html
        :headline-levels 4
        :auto-preamble t)))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)
(use-package magit-todos
  :defer t)
(use-package git-link)
(ju/leader-key-def
  "g" 'magit)
(use-package git-gutter
  :straight git-gutter-fringe
  ;;:diminish
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 2)
  (require 'git-gutter-fringe)
  (set-face-foreground 'git-gutter-fr:added "LightGreen")
  (fringe-helper-define 'git-gutter-fr:added nil
    ".XXXXXX."
    "XX....XX"
    "X......X"
    "X......X"
    "XXXXXXXX"
    "XXXXXXXX"
    "X......X"
    "X......X")

  (set-face-foreground 'git-gutter-fr:modified "LightGoldenrod")
  (fringe-helper-define 'git-gutter-fr:modified nil
    "XXXXXXXX"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X")

  (set-face-foreground 'git-gutter-fr:deleted "LightCoral")
  (fringe-helper-define 'git-gutter-fr:deleted nil
    "XXXXXX.."
    "XX....X."
    "XX.....X"
    "XX.....X"
    "XX.....X"
    "XX.....X"
    "XX....X."
    "XXXXXX..")

  ;; These characters are used in terminal mode
  (setq git-gutter:modified-sign "≡")
  (setq git-gutter:added-sign "≡")
  (setq git-gutter:deleted-sign "≡")
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass[a4paper,11pt,titlepage]{memoir}
        \\usepackage{kotex}
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\hypersetup{pdfborder=0 0 0}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("ethz"
                 "\\documentclass[a4paper,11pt,titlepage]{memoir}
        \\usepackage{kotex}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{mathpazo}
\\usepackage{color}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\hypersetup{pdfborder=0 0 0}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass[11pt,a4paper]{article}
\\usepackage{kotex}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{mathpazo}
\\usepackage{color}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\hypersetup{pdfborder=0 0 0}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")))


  (add-to-list 'org-latex-classes '("ebook"
                                    "\\documentclass[11pt, oneside]{memoir}
\\setstocksize{9in}{6in}
\\settrimmedsize{\\stockheight}{\\stockwidth}{*}
\\setlrmarginsandblock{2cm}{2cm}{*} % Left and right margin
\\setulmarginsandblock{2cm}{2cm}{*} % Upper and lower margin
\\checkandfixthelayout
% Much more laTeX code omitted
"
                                    ("\\chapter{%s}" . "\\chapter*{%s}")
                                    ("\\section{%s}" . "\\section*{%s}")
                                    ("\\subsection{%s}" . "\\subsection*{%s}"))))

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted) ;; Use minted to highlight source code
(setq org-latex-minted-options
      '(("breaklines" "true")
        ("tabsize" "4")
        ("autogobble")
        ("bgcolor" "monokaibg")))
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq package-check-signature nil)
(use-package org-gcal
  :ensure t
  :config
  (setq org-gcal-client-id "928945571655-4h4kclj8etm6qfm27jk4jhgil6jsotbc.apps.googleusercontent.com"
        org-gcal-client-secret "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"
        org-gcal-file-alist '(("cpu3792@gmail.com" .  "~/Roam/gcal.org"))))
;;(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))


(add-to-list 'org-agenda-files "~/Roam/Agenda/gcal.org")
;;(add-to-list 'org-agenda-files "~/Roam/Agenda/links.org")
;;(add-to-list 'org-agenda-files "~/Roam/Agenda/i.org")


(setq org-capture-templates
      '(("a" "Appointment" entry (file  "~/Roam/Agenda/gcal.org" )
         "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ("l" "Link" entry (file+headline "~/Roam/Agenda/links.org" "Links")
         "* %? %^L %^g \n%T" :prepend t)
        ("b" "Blog idea" entry (file+headline "~/Roam/Agenda/i.org" "Blog Topics:")
         "* %?\n%T" :prepend t)
        ("t" "To Do Item" entry (file+headline "~/Roam/Agenda/i.org" "To Do")
         "* TODO %?\n%u" :prepend t)
        ("n" "Note" entry (file+headline "~/Roam/Agenda/i.org" "Note space")
         "* %?\n%u" :prepend t)
        ("j" "Journal" entry (file+datetree "~/Roam/Agenda/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("s" "Screencast" entry (file "~/Roam/Agenda/screencastnotes.org")
         "* %?\n%i\n")))

;; Don't let ediff break EXWM, keep it in one frame
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))
(defun dw/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
        (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

(defun dw/eshell-prompt ()
  (let ((current-branch (magit-get-current-branch)))
    (concat
     "\n"
     (propertize (system-name) 'face `(:foreground "#62aeed"))
     (propertize " ॐ " 'face `(:foreground "white"))
     (propertize (dw/get-prompt-path) 'face `(:foreground "#82cfd3"))
     (when current-branch
       (concat
        (propertize " • " 'face `(:foreground "white"))
        (propertize (concat " " current-branch) 'face `(:foreground "#c475f0"))))
     (propertize " • " 'face `(:foreground "white"))
     (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground "#5a5b7f"))
     (if (= (user-uid) 0)
         (propertize "\n#" 'face `(:foreground "red2"))
       (propertize "\nλ" 'face `(:foreground "#aece4a")))
     (propertize " " 'face `(:foreground "white")))))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-prompt-function      'dw/eshell-prompt
        eshell-prompt-regexp        "^λ "
        eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim"))))

(use-package dired
  :ensure nil
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))
(autoload 'dired-omit-mode "dired-x")
(add-hook 'dired-load-hook
          (lambda ()
            (interactive)
            (dired-collapse)))
;; to use h,l key
(use-package dired-single
  :commands (dired dired-jump))
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; from doom emacs
(defun +dired-enable-git-info-h ()
  "Enable `dired-git-info-mode' in git repos."
  (and (not (file-remote-p default-directory))
       (locate-dominating-file "." ".git")
       (dired-git-info-mode 1)))
(use-package dired-git-info)
(use-package diff-hl
  :hook (dired-mode . diff-hl-dired-mode-unless-remote)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; use margin instead of fringe
  (diff-hl-margin-mode))
(use-package dired-rsync
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  ;; HACK Fixes #1929: icons break file renaming in Emacs 27+, because the icon
  ;;      is considered part of the filename, so we disable icons while we're in
  ;;      wdired-mode.
  ;;(when EMACS27+
  (defvar +wdired-icons-enabled -1))

;;(defadvice! +dired-disable-icons-in-wdired-mode-a (&rest _)
;;  :before #'wdired-change-to-wdired-mode
;;  (setq-local +wdired-icons-enabled (if all-the-icons-dired-mode 1 -1))
;;  (when all-the-icons-dired-mode
;;    (all-the-icons-dired-mode -1)))

;;(defadvice! +dired-restore-icons-after-wdired-mode-a (&rest _)
;;  :after #'wdired-change-to-dired-mode
;;  (all-the-icons-dired-mode +wdired-icons-enabled))))
;;(use-package fd-dired)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))
;;(use-package ranger
;;  :config
;;  (setq ranger-preview-file t)
;;  (setq ranger-show-preview t)
;;  (setq ranger-show-literal nil)
;;  (ranger-override-dired-mode t))

(use-package openwith
  :after dired
  :config
  (setq larget-file-warning-threshold nil)
  (openwith-mode t)
  (setq openwith-associations
        (list (list (openwith-make-extension-regexp
                     '("mpg" "mpeg" "mp3" "mp4" "m4v"
                       "avi" "wmv" "wav" "mov" "flv"
                       "ogm" "ogg" "mkv"))
                    "mpv"
                    '(file)))))
              ;;(list (openwith-make-extension-regexp
              ;;       '("pdf"))
              ;;      "evince"
              ;;      '(file)))))

(use-package vlf
  :config (progn
            (require 'vlf-setup)))

(use-package nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c++-mode . lsp)
         (verilog-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package elfeed
  :config
  (setq elfeed-search-feed-face ":foreground #fff :weight bold"
        elfeed-feeds (quote
                      (("https://www.reddit.com/r/linux.rss" reddit linux)
                       ("https://www.reddit.com/r/emacs.rss" reddit emacs)
                       ("https://www.gamingonlinux.com/article_rss.php" gaming linux)
                       ("https://hackaday.com/blog/feed/" hackaday linux)
                       ("https://opensource.com/feed" opensource linux)
                       ("https://linux.softpedia.com/backend.xml" softpedia linux)
                       ("https://itsfoss.com/feed/" itsfoss linux)
                       ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
                       ("https://www.phoronix.com/rss.php" phoronix linux)
                       ("http://feeds.feedburner.com/d0od" omgubuntu linux)
                       ("https://www.computerworld.com/index.rss" computerworld linux)
                       ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
                       ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
                       ("https://betanews.com/feed" betanews linux)
                       ("http://lxer.com/module/newswire/headlines.rss" lxer linux)
                       ("https://coolenjoy.net/rss?bo_table=38" coolenjoy IT)
                       ("https://gigglehd.com/gg/ggnews/rss" giggle IT)))))

(use-package elfeed-goodies
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/entry-pane-size 0.5))

(add-hook 'elfeed-show-mode-hook 'visual-line-mode)
(evil-define-key 'normal elfeed-show-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)
(evil-define-key 'normal elfeed-search-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)

(use-package haskell-mode)

(use-package rust-mode)

(use-package markdown-mode
  :disabled
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package cmake-mode)

(use-package yaml-mode)

(use-package ytel
  :straight t
  :config
  (setq ytel-invidious-api-url "https://invidious.snopyta.org")
  )
(defun ytel-watch ()
  "Stream video at point in mpv."
  (interactive)
  (let* ((video (ytel-get-current-video))
         (id    (ytel-video-id video)))
    (start-process "ytel mpv" nil
                   "mpv"
                   (concat "https://www.youtube.com/watch?v=" id))
    "--ytdl-format=bestvideo[height<=?720]+bestaudio/best")
  (message "Starting streaming..."))

(use-package jupyter
  :straight t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) ;; Other languages
   (shell . t)
   ;; Python & Jupyter
   (python . t)
   (jupyter . t)))
(org-babel-jupyter-override-src-block "python")
;;; example-config.el -- Example Rational Emacs user customization file -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Rational Emacs supports user customization through a `config.el' file
;; similar to this one.  You can copy this file as `config.el' to your
;; Rational Emacs configuration directory as an example.
;;
;; In your configuration you can set any Emacs configuration variable, face
;; attributes, themes, etc as you normally would.
;;
;; See the README.org file in this repository for additional information.

;;; Code:
(require 'rational-defaults)
;;(require 'rational-screencast)
(require 'rational-ui)
;;(require 'rational-editing)
(require 'rational-evil)
(require 'rational-completion)
;;(require 'rational-windows)
(require 'rational-use-package)

;; Load auctex
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("." . "~/.emacs.rational.d/backups")))
(customize-set-variable
 'tramp-backup-directory-alist backup-directory-alist)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.rational.d/undo")))


(setq user-full-name "JaeYoo-Im"
      user-mail-address "cpu3792@gmail.com")

(setq system-time-locale "C")
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;; Set further font and theme customizations
(custom-set-variables
   '(rational-ui-default-font
     '(:font "Fira Code Retina" :weight light :height 110)))

(add-to-list 'default-frame-alist `(font . "Fira Code Retina"))
(set-face-attribute 'default nil :font "Fira Code Retina" :height 110)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 110)
(set-face-attribute 'variable-pitch nil :font "Fira Code Retina" :height 110 :weight 'regular)
(set-fontset-font t 'hangul (font-spec :family "D2Coding" :height 110))
(setq face-font-rescale-alist
      '(("D2Coding" . 1.25)))

;; Fullscreen
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Favorite Themes
(load-theme 'doom-palenight t)
;;(load-theme 'doom-gruvbox t)
;;(load-theme 'doom-one t)
;;(load-theme 'doom-dracula t)


(straight-use-package 'use-package)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Additional Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVIL Numbers
(use-package evil-numbers
  :straight t
  :after evil
  :config
  ;; unfortunately C-x is emacs common key binding.
  (define-key evil-normal-state-map (kbd "g =") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "g -") 'evil-numbers/dec-at-pt)
  (define-key evil-visual-state-map (kbd "g =") 'evil-numbers/inc-at-pt)
  (define-key evil-visual-state-map (kbd "g -") 'evil-numbers/dec-at-pt))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Additional UI pkg
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(global-hl-line-mode)
(dolist (mode '(org-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                dired-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))
;; dashboard
(use-package dashboard
    :straight t
	:after all-the-icons
    :init
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    ;;(setq dashboard-banner-logo-title "Emacs is more than a text editor!")
    ;;(setq dashboard-startup-banner 'logo)
    (setq dashboard-startup-banner "/home/jaeus/.emacs.rational.d/logos/emacs-e.png")
    (setq dashboard-center-content t)
    (setq dashboard-week-agenda t)
    (setq dashboard-agenda-time-string-format "%d/%m/%Y %A %H:%M")
    (setq dashboard-items '((recents . 10)
                            (agenda . 5)
                            (bookmarks . 5)
                            ;;(projects . 5) <= related projectile
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

(use-package vertico-posframe
  :straight t
  :disabled
  :after vertico
  :ensure t
  :init
  (setq vertico-posframe-parameters
        `((left-fringe . 8)
          (right-fringe . 8) (alpha . 100)))
  (vertico-posframe-mode 1))

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
         '(80 . 80) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)
(set-frame-parameter (selected-frame) 'alpha '(80 . 80))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EMOJIFY ♌ 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emojify
  :straight t
  :hook (after-init . global-emojify-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :straight t
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))
(use-package general
  :straight t
  :ensure t
  :config
  (general-evil-setup t)
  (general-create-definer ju/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))
;; General key definer key-prefix SPC
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
  ;; Magit
  "g" 'magit
  ;; Workspace
  ;; Counsel
  "f" '(:ignore t :which-key "file op.")
  "f r" '(consult-recent-file :which-key "Recent files")
  "f R" '(revert-buffer :which-key "Revert Buffer")
  "t t" '(toggle-truncate-lines :which-key "Toggle truncate lines")
  ;; Shortcut
  "f d d" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.rational.d/desktop.el")) :which-key "open exwm config")
  "f d e" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.rational.d/config.el")) :which-key "open emacs config")
  ;; Insert something
  "i" '(:ignore t :which-key "insert something.")
  "ie" '(emojify-insert-emoji :which-key "emoji")
  ;; AVY (easymotion)
  "v" '(:ignore t :which-key "Avy")
  "vc" '(avy-goto-char :which-key "Avy Goto Char")
  "vw" '(avy-goto-word-0 :which-key "Avy Goto Word")
  "vl" '(avy-goto-line :which-key "Avy Goto Line")
  ;; Org mode
  "o" '(:ignore t :which-key "Org mode")
  "oa" '(:ignore t :which-key "Org Agenda")
  "oar" '(my/org-roam-refresh-agenda-list :which-key "Org agenda refresh list")
  "oaa" '(org-agenda :which-key "open org agenda")
  ;; project.el
  "p" '(:ignore t :which-key "Project")
  "p." 'project-find-file
  "ps" 'project-switch-project
  "pg" 'consult-ripgrep
  "pc" 'project-compile
  "pd" 'project-dired
  ;; Org roam
  "r" '(:ignore t :which-key "Org Roam")
  "ru" 'org-roam-ui-open
  "rl" 'org-roam-buffer-toggle
  "rf" 'org-roam-node-find
  "ri" 'org-roam-node-insert
  "rI" 'org-roam-node-insert-immediate
  "rp" 'my/org-roam-find-project)

(global-set-key (kbd "C-M-j") 'consult-buffer)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; this annoying binding.
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "S-SPC"))    ;; use only S-\
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tramp default is scp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tramp-default-method "ssh")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :straight t
  :ensure t
  :defer t
  :init (global-flycheck-mode t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; AVY (like easymotion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :straight t
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Hilight TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package hl-todo
  :straight t
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
(global-hl-todo-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  :straight t
  :commands (dired dired-jump))
(use-package diredfl
  :straight t
  :hook (dired-mode . diredfl-mode))

;; from doom emacs
(defun +dired-enable-git-info-h ()
  "Enable `dired-git-info-mode' in git repos."
  (and (not (file-remote-p default-directory))
       (locate-dominating-file "." ".git")
       (dired-git-info-mode 1)))
(use-package dired-git-info :straight t)
(use-package diff-hl
  :straight t
  :hook (dired-mode . diff-hl-dired-mode-unless-remote)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; use margin instead of fringe
  (diff-hl-margin-mode))
(use-package dired-rsync
  :straight t
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))
(use-package all-the-icons-dired
  :straight t
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
  :straight t
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; OpenWith
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package openwith
  :straight t
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
                    '(file))
              (list (openwith-make-extension-regexp
                     '("pdf"))
                    "evince"
                    '(file)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :straight t
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :straight t
  :after magit)
(use-package magit-todos
  :straight t
  :defer t)
(use-package git-link :straight t)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ORG mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defun efs/org-mode-setup ()
  (org-indent-mode)
  ;;(variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :straight t
  ;;:pin org
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

(use-package org-superstar :straight t :after org :hook (org-mode . org-superstar-mode))
(use-package org-attach-screenshot :straight t)
(use-package org-download
  :straight t
  :config
  (setq-default org-download-image-dir "./images")
  (setq-default org-download-heading-lvl nil))
(add-hook 'dired-mode-hook 'org-download-enable)
 
(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
 
(use-package visual-fill-column
  :straight t
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
  (add-to-list 'org-structure-template-alist '("vh" . "src vhdl")))

;; ODT export to docx
(setq org-odt-preferred-output-format "docx")

;; Do not ask when run code block
(setq org-confirm-babel-evaluate nil)
(use-package org-contrib :straight t)

;; translate
(use-package ob-translate
  :straight t
  :config
  (setq ob-translate:default-dest "ko"))

;; Noter
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
(use-package org-noter :straight t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ORG Roam
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-roam
  :straight t
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

(use-package websocket
  :straight t
  :after org-roam)
(use-package org-roam-ui
  :straight t
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

;; Build the agenda list the first time for the session
;;(my/org-roam-refresh-agenda-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Language Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 '(verilog-indent-level 2)
 '(verilog-indent-level-module 0)
 '(verilog-indent-level-declaration 0)
 '(verilog-indent-level-behavioral 0)
 '(verilog-indent-level-directive 0)
 '(verilog-cexp-indent 0)
 '(verilog-case-indent 2)

 '(verilog-auto-newline nil))

(use-package rust-mode :straight t)

(use-package jupyter
  :straight t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) ;; Other languages
   (shell . t)
   ;; Python & Jupyter
   (python . t)))
   ;;(jupyter . t)))
;;(org-babel-jupyter-override-src-block "python")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Eshell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  :straight t
  :after eshell)

(use-package eshell
  :straight t
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ELFEED (rss)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elfeed
  :straight t
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
  :straight t
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ETC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ediff, keep it in one frame
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; very large file
(use-package vlf
  :straight t
  :config (progn
            (require 'vlf-setup)))

;; epub
(use-package nov :straight t)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

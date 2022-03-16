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
(require 'rational-screencast)
(require 'rational-ui)
(require 'rational-editing)
(require 'rational-evil)
(require 'rational-completion)
(require 'rational-windows)
(require 'rational-use-package)

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


(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(load-theme 'doom-dracula t)

(column-number-mode)
(global-display-line-numbers-mode t)
(global-hl-line-mode)

;;; example-config.el ends here
(setq user-full-name "JaeYoo-Im"
      user-mail-address "cpu3792@gmail.com")
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
         '(75 . 75) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)
(set-frame-parameter (selected-frame) 'alpha '(75 . 75))

;; DASHBOARD
(straight-use-package 'dashboard)
(require 'all-the-icons)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content t)
(setq dashboard-week-agenda t)
(setq dashboard-agenda-time-string-format "%d/%m/%Y %A %H:%M")
(setq dashboard-items '((recents . 10)
			(agenda . 5)
			(bookmarks . 5)
			(registers . 5)))
(setq dashboard-set-navigator t)
;; Format: "(icon title help action face prefix suffix)"
(setq dashboard-navigator-buttons
      `((
	 (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
	   "Github"
	   "Browse github"
	   (lambda (&rest _) (browse-url "https://github.com/JaeUs3792")))
	 (,(all-the-icons-octicon "book" :height 1.1 :v-adjust 0.0)
	   "My Blog"
	   "Browse Blog"
	   (lambda (&rest _) (browse-url "https://jaeus.net"))))))
(setq dashboard-set-footer nil)
(dashboard-setup-startup-hook)


;; EMOJI
(straight-use-package 'emojify)
(global-emojify-mode)

;; Which Key
(straight-use-package 'which-key)
(setq which-key-idle-delay 0.5)
(which-key-mode)


;; GENERAL KEY MAP
(straight-use-package 'general)
(general-evil-setup t)
(general-create-definer ju/leader-key-def
			:keymaps '(normal insert visual emacs)
			:prefix "SPC"
			:global-prefix "C-SPC")

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
  "f d d" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.rational.d/desktop.el")) :which-key "open exwm config")
  "f d e" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.rational.d/config.el")) :which-key "open emacs config"))


(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "S-SPC"))    ;; use only S-\


;; tramp default is scp
(setq tramp-default-method "ssh")

(straight-use-package 'flycheck)
(global-flycheck-mode t)


;;(straight-use-package 'vertico-posframe)
;;(setq vertico-posframe-parameters
;;      `((left-fringe . 8)
;;	(right-fringe . 8) (alpha . 100)))
;;(vertico-posframe-mode 1)


;; TODO
(straight-use-package 'hl-todo)
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
	("XXX" font-lock-constant-face bold)))
(global-hl-todo-mode)

;; ORG Mode
(setq org-todo-keywords
      '((sequence "TODO(t)" "HOLD(h)" "|" "KILL(k)" "DONE(d)")))
(setq org-ellipsis " ▾")
(setq org-hide-emphasis-markers t)
(setq org-src-fontify-natively t)
(setq org-fontify-quote-and-verse-blocks t)

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(add-hook 'org-mode-hook (lambda () (org-indent-mode)))

;;(straight-use-package 'org-superstar)
;;(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(straight-use-package 'org-attach-screenshot)
(straight-use-package 'org-download)
(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(straight-use-package 'visual-fill-column)
(add-hook 'org-mode-hook (lambda () org-mode . efs/org-mode-visual-fill))

;;(with-eval-after-load 'org
;;  (org-babel-do-load-languages
;;   'org-babel-load-languages
;;   '((emacs-lisp . t)
;;     (python . t)
;;     (latex . t)))
;;
;;  (push '("conf-unix" . conf-unix) org-src-lang-modes))
;;
;;(with-eval-after-load 'org
;;  ;; This is needed as of Org 9.2
;;  (require 'org-tempo)
;;
;;  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
;;  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
;;  (add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
;;  (add-to-list 'org-structure-template-alist '("cc" . "src c"))
;;  (add-to-list 'org-structure-template-alist '("cp" . "src c++"))
;;  (add-to-list 'org-structure-template-alist '("rs" . "src rust"))
;;  (add-to-list 'org-structure-template-alist '("py" . "src python"))
;;  (add-to-list 'org-structure-template-alist '("oc" . "src octave"))
;;  (add-to-list 'org-structure-template-alist '("vl" . "src verilog"))
;;  (add-to-list 'org-structure-template-alist '("vh" . "src vhdl"))

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Input method
(setq default-input-method "korean-hangul")
(global-set-key (kbd "<Hangul>") 'toggle-input-method)

;; Tabs
;;(setq! indent-tabs-mode t)
(setq-default tab-width 4)

;;
(setq user-full-name "JaeYoo-Im"
      user-mail-address "cpu3792@gmail.com")

;; Frame parameter
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(unless (display-graphic-p)
  (xterm-mouse-mode))

;; Fonts
(setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 14))
(set-fontset-font t 'hangul (font-spec :name "NanumGothic"))

;; 'nil 't 'relative
(setq display-line-numbers-type t)

;; to use relative path for org
(setq org-directory "~/org/")
(setq visible-bell t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Eshell
(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))
(defun ju/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
        (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

(defun ju/eshell-prompt ()
  (let ((current-branch (magit-get-current-branch)))
    (concat
     "\n"
     (propertize (system-name) 'face `(:foreground "#62aeed"))
     (propertize " ॐ " 'face `(:foreground "white"))
     (propertize (ju/get-prompt-path) 'face `(:foreground "#82cfd3"))
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

(defun ju/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-prompt-function      'ju/eshell-prompt
        eshell-prompt-regexp        "^λ "
        eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))
(add-hook 'eshell-first-time-mode-hook #'ju/configure-eshell)
(setq eshell-prompt-function
      (lambda ()
        (concat (abbreviate-file-name (eshell/pwd))
                (if (= (user-uid) 0) " # " " λ ")))
      eshell-prompt-regexp "^[^#λ\n]* [#λ] ")
; Dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! dired
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))
(use-package! dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))
(setq! delete-by-moving-to-trash t
       trash-directory "~/.local/share/Trash/files/")
(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
              (:map dired-mode-map
               :desc "Peep-dired image previews" "d p" #'peep-dired
               :desc "Dired view file" "d v" #'dired-view-file)))
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; themes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (display-graphic-p)
    (setq doom-theme 'doom-palenight)
  (setq doom-theme 'doom-gruvbox))
; dashboard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! page-break-lines
  :init
  (global-page-break-lines-mode t))
(use-package! dashboard
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;;(setq dashboard-banner-logo-title "Emacs is more than a text editor!")
  ;;(setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-week-agenda t)
  (setq dashboard-set-footer t)
  (setq dashboard-page-separator "\n\f\n")
  (setq dashboard-agenda-time-string-format "%d/%m/%Y %A %H:%M")
  (setq dashboard-items '((recents . 10)
                          (agenda . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
        `(;; line1
          ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Github"
            "Browse my Github"
            (lambda (&rest _) (browse-url "https://github.com/JaeUs3792/")))
           (,(all-the-icons-octicon "home" :height 1.1 :v-adjust 0.0)
            "Homepage"
            "Browse my Homepage"
            (lambda (&rest _) (browse-url "https://jaeus.net")))
           (,(all-the-icons-octicon "zap" :height 1.1 :v-adjust 0.0)
            "Refresh"
            "Refresh Packages"
            (lambda (&rest _) (package-refresh-contents)) warning))))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; write room ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! visual-fill-column)
(defun write-room-enable ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
(add-hook 'org-mode-hook #'(lambda () (display-line-numbers-mode 0)))
(add-hook 'org-mode-hook #'write-room-enable)

;; orgmode
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
  (add-to-list 'org-structure-template-alist '("cc" . "src c"))
  (add-to-list 'org-structure-template-alist '("cp" . "src c++"))
  (add-to-list 'org-structure-template-alist '("rs" . "src rust"))
  (add-to-list 'org-structure-template-alist '("py" . "src jupyter-python"))
  (add-to-list 'org-structure-template-alist '("oc" . "src octave"))
  (add-to-list 'org-structure-template-alist '("vl" . "src verilog"))
  (add-to-list 'org-structure-template-alist '("vh" . "src vhdl")))

(after! org
  (setq! org-ellipsis                       " ⤵"
        org-hide-emphasis-markers          t
        org-image-actual-width             600
        org-redisplay-inline-images        t
        org-display-inline-images          t
        org-startup-with-inline-images     "inlineimages"
        org-src-fontify-natively           t
        org-fontify-quote-and-verse-blocks t
        org-agenda-start-with-log-mode     t
        org-startup-indented               t
        org-startup-align-all-tables       t
        org-log-done                       'time
        org-log-into-drawer                "LOGBOOK"
        org-clock-into-drawer              "CLOCKING"
        org-lowest-priority                68)
  ;;(setq org-todo-keywords
  ;;      '((sequence "TODO(t)" "HOLD(h)" "|" "KILL(k)" "DONE(d)")))
  (setq! org-superstar-item-bullet-alist
    '((?+ . ?➢)
      (?* . ?✰)
      (?- . ?➸)))
  (custom-set-faces!
    '(org-level-1 :height 1.7  :weight ultra-bold :foreground "#81a2be")         ;; :foreground "#81a2be"
    '(org-level-2 :height 1.6  :weight extra-bold :foreground "#b294bb")         ;; :foreground "#b294bb"
    '(org-level-3 :height 1.5  :weight bold :foreground "#b5bd68")               ;; :foreground "#b5bd68"
    '(org-level-4 :height 1.4  :weight semi-bold :foreground "#e6c547")          ;; :foreground "#e6c547"
    '(org-level-5 :height 1.3  :weight normal :foreground "#cc6666")             ;; :foreground "#cc6666"
    '(org-level-6 :height 1.2  :weight normal :foreground "#70c0ba")             ;; :foreground "#70c0ba"
    '(org-level-7 :height 1.1  :weight normal :foreground "#b77ee0")             ;; :foreground "#b77ee0"
    '(org-level-8 :height 1.0  :weight normal :foreground "#9ec400"))            ;; :foreground "#9ec400"
  (custom-set-faces!
    '(org-document-title :height 2.0)))
(setq! org-use-sub-superscripts '{})
(setq! org-export-with-sub-superscripts '{})

;; fancy priorities
(setq! org-fancy-priorities-list `(,(all-the-icons-faicon "flag"     :height 1.1 :v-adjust 0.0)
                                  ,(all-the-icons-faicon "arrow-up" :height 1.1 :v-adjust 0.0)
                                  ,(all-the-icons-faicon "square"   :height 1.1 :v-adjust 0.0)
                                  ,(all-the-icons-faicon "ban"      :height 1.1 :v-adjust 0.0)))
;; TODO somewhere
(add-hook 'markdown-mode-hook #'write-room-enable)
(add-hook 'nov-mode-hook #'write-room-enable)

;; valign
;; make table align
(use-package! valign
  :custom
  (valign-fancy-bar t))

;; org appear (bug.1)
;;(use-package! org-appear
;;  :hook (org-mode-hook . org-appear-mode)
;;  :config
;;  (setq org-appear-autoemphasis   t
;;        org-appear-autolinks      t
;;        org-appear-autoentities   t
;;        org-appear-autosubmarkers t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Screen Shot ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/self-screenshot (&optional type)
  "Save a screenshot of type TYPE of the current Emacs frame.
        As shown by the function `', type can weild the value `svg',
        `png', `pdf'.

        This function will output in /tmp a file beginning with \"Emacs\"
        and ending with the extension of the requested TYPE."
  (interactive)
  (let* ((type (if type type
                 (intern (completing-read "Screenshot Type: "
                                          '(png svg pdf postscript)))))
         (extension (pcase type
                      ('png        ".png")
                      ('svg        ".svg")
                      ('pdf        ".pdf")
                      ('postscript ".ps")
                      (otherwise (error "Cannot export screenshot of type %s" otherwise))))
         (filename (make-temp-file "Emacs-" nil extension))
         (data     (x-export-frames nil type)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (rename-file filename (expand-file-name (file-name-nondirectory filename) "~"))
    (message filename)))
;;Transparency;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; After Emacs 29
;;(set-frame-parameter nil 'alpha-background 0.9)
;;(add-to-list 'default-frame-alist '(alpha-background . 0.9))
;;(defun toggle-transparency ()
;;  "toggle transparency."
;;  (interactive)
;;  (let ((alpha-transparency 1.0))
;;    (if (eq (frame-parameter nil 'alpha-background) alpha-transparency)
;;        (set-frame-parameter nil 'alpha-background 0.9)
;;      (set-frame-parameter nil 'alpha-background alpha-transparency))))
;;(defun my/transparency-round (val)
;;  "Round VAL to the nearest tenth of an integer."
;;  (/ (round (* 10 val)) 10.0))
;;
;;(defun my/increase-frame-alpha-background ()
;;  "Increase current frame’s alpha background."
;;  (interactive)
;;  (set-frame-parameter nil
;;                       'alpha-background
;;                       (my/transparency-round
;;                        (min 1.0
;;                             (+ (frame-parameter nil 'alpha-background) 0.1))))
;;  (message "%s" (frame-parameter nil 'alpha-background)))
;;
;;(defun my/decrease-frame-alpha-background ()
;;  "Decrease current frame’s alpha background."
;;  (interactive)
;;  (set-frame-parameter nil
;;                       'alpha-background
;;                       (my/transparency-round
;;                        (max 0.0
;;                             (- (frame-parameter nil 'alpha-background) 0.1))))
;;  (message "%s" (frame-parameter nil 'alpha-background)))
;; bind key
;;(global-set-key (kbd "C-c t") 'toggle-transparency)

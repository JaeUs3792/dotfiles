;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq default-input-method "korean-hangul")
(global-set-key (kbd "<Hangul>") 'toggle-input-method)
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "JaeYoo-Im"
      user-mail-address "cpu3792@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; | 12345678 |   |
;; |----------+---|
;; | 일이삼사 |   |

(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 13))
(set-fontset-font t 'hangul (font-spec :name "NanumGothic"))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(if (display-graphic-p)
    ;;(setq doom-theme 'doom-palenight)
    ;;(setq doom-theme 'doom-monokai-pro)
    (setq doom-theme 'doom-material)
    ;;(setq doom-theme 'morning-star)
    ;;(setq doom-theme 'doom-dracula)
    ;;(setq doom-theme 'doom-one)
    (setq doom-theme 'doom-gruvbox))
(unless (display-graphic-p)
  (xterm-mouse-mode))
(beacon-mode 1)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; --------------------------------------------------------------------------------------------
;; -Transparency
;; --------------------------------------------------------------------------------------------
(if (display-graphic-p)
    (vertico-posframe-mode 1))
;; --------------------------------------------------------------------------------------------
;; -Transparency
;; --------------------------------------------------------------------------------------------
(defvar gvar/frame-transparency '(85 . 85))

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

;; --------------------------------------------------------------------------------------------
;; - Org Paste Clipboard image for windows, in linux(use org-screenshot-take)
;; --------------------------------------------------------------------------------------------
(defun my-org-paste-image ()
  "Paste an image into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (let* ((target-file
          (concat
           (make-temp-name
            (concat "~/org/images/"
                    (f-filename buffer-file-name)
                    "_"
                    (format-time-string "%Y%m%d_%H%M%S_"))) ".png"))
         (wsl-path
          (concat (as-windows-path(file-name-directory target-file))
                  "/"
                  (file-name-nondirectory target-file)))
         (ps-script
          (concat "(Get-Clipboard -Format image).Save('" wsl-path "')")))

    (powershell ps-script)

    (if (file-exists-p target-file)
        (progn (insert (concat "[[" target-file "]]"))
               (org-display-inline-images))
      (user-error
       "Error pasting the image, make sure you have an image in the clipboard!"))
    ))

(defun as-windows-path (unix-path)
  "Takes a unix path and returns a matching WSL path"
  ;; substring removes the trailing \n
  (substring
   (shell-command-to-string
    (concat "wslpath -w " unix-path)) 0 -1))

(defun powershell (script)
  "executes the given script within a powershell and returns its return value"
  (call-process "powershell.exe" nil nil nil
                "-Command" (concat "& {" script "}")))

;; --------------------------------------------------------------------------------------------
;; - Rainbow delimiter mode for programming.
;; --------------------------------------------------------------------------------------------
(add-hook 'verilog-mode-hook #'rainbow-delimiters-mode)
;; --------------------------------------------------------------------------------------------
;; Avy multi-buffer
;; --------------------------------------------------------------------------------------------
(setq! avy-all-windows t)
;; --------------------------------------------------------------------------------------------
;; - Calendar
;; --------------------------------------------------------------------------------------------
(use-package! calfw)
(use-package! calfw-org)
;; --------------------------------------------------------------------------------------------
;; - Neotree
;; --------------------------------------------------------------------------------------------
;; comment ~/.emacs.d/modules/ui/doom/config.el (add-hook 'doom-load-theme-hook #'doom-themes-neotree-config)
(setq! neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq! neo-smart-open t)
;;(doom-themes-neotree-config)
;;(setq! doom-themes-neotree-file-icons t)
;;;; --------------------------------------------------------------------------------------------
;; - Org Mode
;; --------------------------------------------------------------------------------------------
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
  (setq! org-hide-emphasis-markers t)
   (setq org-superstar-item-bullet-alist
        '((?+ . ?➤)
          (?* . ?–)
          (?- . ?•)))
   (custom-set-faces!
     '(org-level-1 :height 1.7  :weight ultra-bold)         ;; :foreground "#81a2be"
     '(org-level-2 :height 1.6  :weight extra-bold)         ;; :foreground "#b294bb"
     '(org-level-3 :height 1.5  :weight bold)               ;; :foreground "#b5bd68"
     '(org-level-4 :height 1.4  :weight semi-bold)          ;; :foreground "#e6c547"
     '(org-level-5 :height 1.3  :weight normal)             ;; :foreground "#cc6666"
     '(org-level-6 :height 1.2  :weight normal)             ;; :foreground "#70c0ba"
     '(org-level-7 :height 1.1  :weight normal)             ;; :foreground "#b77ee0"
     '(org-level-8 :height 1.0  :weight normal))            ;; :foreground "#9ec400"
   (custom-set-faces!
     '(org-document-title :height 2.0)))
;; --------------------------------------------------------------------------------------------
;; - Org Roam
;; --------------------------------------------------------------------------------------------
(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project"))
  (add-to-list 'org-agenda-files "~/org/agenda/agenda.org"))
(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

;;(with-eval-after-load 'org-roam
;;  (defun my/org-roam-project-finalize-hook ()
;;    "Adds the captured project file to `org-agenda-files' if the
;;capture was not aborted."
;;    ;; Remove the hook since it was added temporarily
;;    (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
;;
;;    ;; Add project file to the agenda list if the capture was confirmed
;;    (unless org-note-abort
;;      (with-current-buffer (org-capture-get :buffer)
;;        (add-to-list 'org-agenda-files (buffer-file-name)))))
;;  (defun my/org-roam-find-project ()
;;    (interactive)
;;    ;; Add the project file to the agenda after capture is finished
;;    (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
;;
;;    ;; Select a project file to open, creating it if necessary
;;    (org-roam-node-find
;;     nil
;;     nil
;;     (my/org-roam-filter-by-tag "Project")
;;     :templates
;;     '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
;;        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
;;        :unnarrowed t))))
;;  (defun my/org-roam-capture-inbox ()
;;    (interactive)
;;    (org-roam-capture- :node (org-roam-node-create)
;;                       :templates '(("i" "inbox" plain "* %?"
;;                                     :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))
;;
;;  (defun my/org-roam-copy-todo-to-today ()
;;    (interactive)
;;    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
;;          (org-roam-dailies-capture-templates
;;           '(("t" "tasks" entry "%?"
;;              :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
;;          (org-after-refile-insert-hook #'save-buffer)
;;          today-file
;;          pos)
;;      (save-window-excursion
;;        (org-roam-dailies--capture (current-time) t)
;;        (setq today-file (buffer-file-name))
;;        (setq pos (point)))
;;
;;      ;; Only refile if the target file is different than the current file
;;      (unless (equal (file-truename today-file)
;;                     (file-truename (buffer-file-name)))
;;        (org-refile nil nil (list "Tasks" today-file nil pos))))))

;; --------------------------------------------------------------------------------------------
;; - Org Agenda
;; --------------------------------------------------------------------------------------------
(use-package! org-super-agenda
  :after org
  :config
  (org-super-agenda-mode 1)
  (my/org-roam-refresh-agenda-list))
(setq org-agenda-custom-commands
      '(("z" "Org mode super agenda"
          ((alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Important"
                                 :tag "Important"
                                 :priority "A"
                                 :order 6)
                          (:name "Due Today"
                                 :deadline today
                                 :order 2)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 8)
                          (:name "Overdue"
                                 :deadline past
                                 :order 7)
                          (:name "Projects"
                                 :tag "Project"
                                 :order 14)
                          (:name "To read"
                                 :tag "Read"
                                 :order 30)
                          (:name "Waiting"
                                 :todo "WAIT"
                                 :order 20)
                          (:name "Holding"
                                 :todo "HOLD"
                                 :order 21)
                          (:name "trivial"
                                 :priority<= "C"
                                 :tag ("Trivial" "Unimportant")
                                 :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))
(after! org
  ;;(setq org-todo-keywords
  ;;    '((sequence "TODO(t@/!)" "NEXT(n)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")))
  (setq! org-log-into-drawer "LOGBOOK")
  (setq! org-clock-into-drawer "CLOCKING")
  (setq! org-lowest-priority 68))
(use-package! org-fancy-priorities
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))
;; --------------------------------------------------------------------------------------------
;; - Org Mode extra configuration
;; --------------------------------------------------------------------------------------------
(setq org-odt-preferred-output-format "docx")
;;(use-package! ob-translate
;;  :config
;;  (setq ob-translate:default-dest "ko")
;;  (setq google-translate-backend-method 'wget))
(setq google-translate-backend-method 'curl)
(use-package! org-journal
  :config
  (setq org-journal-file-type 'weekly))

;; --------------------------------------------------------------------------------------------
;; - Org Caldav
;; --------------------------------------------------------------------------------------------
;;(use-package! org-caldav
;;  :after org)
;;(defun my/caldav-sync-perso ()
;;  "Sync my local calendar in ~/org/calendar.org with my remote calendar"
;;  (interactive)
;;  (let ((org-caldav-inbox "~/org/cal_inbox.org")
;;        (org-caldav-calendar-id "org")
;;        (org-caldav-url "https://jaeus.net/webdav")
;;        (org-caldav-files '("~/org/calendar.org")))
;;    (call-interactively 'org-caldav-sync)))
;; --------------------------------------------------------------------------------------------
;; - Org Roam UI Mode
;; --------------------------------------------------------------------------------------------
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
    ;;:hook (after-init . org-roam-ui-mode)
    :config
    (setq! org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start nil))
;; --------------------------------------------------------------------------------------------
;; - Org Publish
;; --------------------------------------------------------------------------------------------
(setq org-publish-use-timestamps-flag nil)
(setq org-export-with-broken-lilnks t)
(setq org-publish-project-alist
      '(("jaeus.net"
        :base-directory "~/org/www/"
        :base-extension "org"
        :publishing-directory "~/Projects/html/"
        :recursive t
        :publishing-function org-html-publish-to-html
        :headline-levels 4
        :auto-preamble t)))
;; --------------------------------------------------------------------------------------------
;; - EPUB
;; --------------------------------------------------------------------------------------------
(use-package! nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;; --------------------------------------------------------------------------------------------
;; - Open with external program
;; --------------------------------------------------------------------------------------------
;;(use-package openwith
;;  :after-call pre-command-hook
;;  :config
;;  (setq large-file-warning-threshold nil)
;;  (openwith-mode t)
;;  (setq openwith-associations
;;        (list (list (openwith-make-extension-regexp
;;                     '("mpg" "mpeg" "mp3" "mp4" "m4v"
;;                       "avi" "wmv" "wav" "mov" "flv"
;;                       "ogm" "ogg" "mkv"))
;;                    "mpv"
;;                    '(file))
;;              (list (openwith-make-extension-regexp
;;                     '("pdf"))
;;                    "zathura"
;;                    '(file)))))
;; --------------------------------------------------------------------------------------------
;; - Lang - Latex
;; --------------------------------------------------------------------------------------------
(setq! +latex-viewers '(zathura))
;; --------------------------------------------------------------------------------------------
;; - Lang - Verilog
;; --------------------------------------------------------------------------------------------
(setq! verilog-auto-newline nil)
(setq! verilog-highlight-translate-off t)
(setq! verilog-indent-level 4)
(setq! verilog-indent-level-module 0)
(setq! verilog-indent-level-behavioral 4)
(setq! verilog-indent-level-declaration 0)
(setq! verilog-case-indent 4)
(setq! verilog-cexp-indent 4)
(setq! verilog-indent-lists nil)
; --------------------------------------------------------------------------------------------
;; - Dired
;; --------------------------------------------------------------------------------------------
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
; --------------------------------------------------------------------------------------------
;; - KeyBindings
;; --------------------------------------------------------------------------------------------
(map! "C-s" 'consult-line)
(map! "C-M-l" 'consult-imenu)
(map! "C-M-j" 'persp-switch-to-buffer)

(map! :leader
      ;; file bind
      :desc "Org Agenda Common"
      "oac" '(lambda () (interactive) (find-file (expand-file-name "~/org/agenda/agenda.org")))
      :desc "dOtfiles"
      "fo" '(:ignore t)
      :desc "doom config.el"
      "foc" '(lambda () (interactive) (find-file (expand-file-name "~/.doom.d/config.el")))
      :desc "doom package.el"
      "fop" '(lambda () (interactive) (find-file (expand-file-name "~/.doom.d/packages.el")))
      :desc "doom init.el"
      "foi" '(lambda () (interactive) (find-file (expand-file-name "~/.doom.d/init.el")))
      ;; Eshell
      :desc "eshell"
      "e" '(:ignore t)
      :desc "eshell"
      "e s" 'eshell
      ;; AVY
      :desc "AVY"
      "v" '(:ignore t)
      :desc "Avy goto char"
      "vc" 'avy-goto-char
      :desc "Avy goto word"
      "vw" 'avy-goto-word-0
      :desc "Avy goto line"
      "vl" 'avy-goto-line
      ;; Org mode
      :desc "org roam refresh agenda list"
      "oar" 'my/org-roam-refresh-agenda-list
      :desc "org roam ui open"
      "nru" 'org-roam-ui-open
      :desc "org roam find tag:project"
      "nrp" 'my/org-roam-find-project)

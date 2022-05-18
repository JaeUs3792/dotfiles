;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight)

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
;; --------------------------------------------------------------------------------------------
;; - Org Mode extra configuration
;; --------------------------------------------------------------------------------------------
(setq org-odt-preferred-output-format "docx")
(use-package! ob-translate
  :config
  (setq ob-translate:default-dest "ko"))
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
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))
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
(use-package! openwith
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
              ;;(list (openwith-make-extension-regexp
              ;;       '("pdf"))
              ;;      "zathura"
              ;;      '(file))
              (list (openwith-make-extension-regexp
                     '("html"))
                    "qutebrowser"
                    '(file)))))
;; --------------------------------------------------------------------------------------------
;; - KeyBindings
;; --------------------------------------------------------------------------------------------
(map! "C-s" 'consult-line)
(map! "C-M-l" 'consult-imenu)
(map! "C-M-j" 'persp-switch-to-buffer)
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
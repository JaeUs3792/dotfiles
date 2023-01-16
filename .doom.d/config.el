;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

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
(setq doom-theme 'doom-one)

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

;; Eshell
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

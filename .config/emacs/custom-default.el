(setq user-full-name "JaeYoo-Im"
      user-mail-address "cpu3792@gmail.com")
(setq default-input-method "korean-hangul")

;; The Hangul key reaches Emacs only when fcitx5 is detached (see bspwmrc).
;; Toggle the built-in input method with it; X sends either keysym depending on
;; the keyboard, so bind both.
(dolist (key '("<Hangul>" "<kana>"))
  (global-set-key (kbd key) #'toggle-input-method))
;; Hanja key would otherwise start fcitx5-style conversion; keep it inert.
(dolist (key '("<Hangul_Hanja>" "<hangul-hanja>"))
  (global-set-key (kbd key) #'ignore))

(defun custom-setup-fonts ()
  "setup fonts..."
    ;; default font
    (cl-loop for font in '("FiraCode Nerd Font Mono" "Jetbrains Mono"
                           "Source Code Pro" "DejaVu Sans Mono")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height 130))
    ;; uni code caracter
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

    ;; Noto font or Nanum
    (cl-loop for font in '("Noto Sans CJK KR" "Nanum Gothic")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.00)))
                      (set-fontset-font t '(#x1100 . #xffdc) (font-spec :family font)))))
(when (display-graphic-p)
  (custom-setup-fonts))
(add-hook 'window-setup-hook #'custom-setup-fonts)

(add-to-list 'default-frame-alist '(alpha-background . 85))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (custom-setup-fonts))))
  (custom-setup-fonts))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; select theme
(setq custom-theme-sel 'doom-one)

;; Org setup
(setq custom-org-directory "~/org"
      custom-org-roam-directory "~/org/roam"
      custom-org-agenda-file "~/org/agenda/agenda.org")

;; Line numbers, relative or comment this line
(setq display-line-numbers-type 'relative)

;; Chatgpt setup
(setq custom-chatgpt-my-key "my-key")

;; vertico posframe enable
(setq custom-vertico-posframe nil)

(setq user-full-name "JaeYoo-Im"
	  user-mail-address "cpu3792@gmail.com")
(setq default-input-method "korean-hangul")

(defun custom-setup-fonts ()
  "setup fonts..."
  (when (display-graphic-p)
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
					  (setq face-font-rescale-alist `((,font . 1.26)))
					  (set-fontset-font t '(#x1100 . #xffdc) (font-spec :family font))))))


(custom-setup-fonts)
(add-hook 'window-setup-hook #'custom-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)



;;; init-highlight.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

(use-package hl-line
  :ensure nil ;; built-in package
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil))))
  :config
  (set-face-background 'hl-line "gray8"))

(use-package symbol-overlay
  :straight t
  :ensure t
  :defer t
  :diminish
  :custom-face
  ;;(symbol-overlay-default-face ((t (:inherit region :background unspecified :foreground unspecified))))
  (symbol-overlay-default-face ((t (:inherit nerd-icons-purple :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-1 ((t (:inherit nerd-icons-blue :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-2 ((t (:inherit nerd-icons-pink :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-3 ((t (:inherit nerd-icons-yellow :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-4 ((t (:inherit nerd-icons-orange :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-5 ((t (:inherit nerd-icons-red :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-6 ((t (:inherit nerd-icons-purple :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-7 ((t (:inherit nerd-icons-green :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-8 ((t (:inherit nerd-icons-cyan :background unspecified :foreground unspecified :inverse-video t))))
  :bind (("M-i" . symbol-overlay-put)
         ("M-I" . symbol-overlay-remove-all)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev))
  :hook ((prog-mode yaml-mode) . symbol-overlay-mode)
  :init (setq symbol-overlay-idle-time 0.5)
  :config
  (with-no-warnings
    ;; Disable symbol highlighting while selecting
    (defun turn-off-symbol-overlay (&rest _)
      "Turn off symbol highlighting."
      (interactive)
      (symbol-overlay-mode -1))
    (advice-add #'set-mark :after #'turn-off-symbol-overlay)

    (defun turn-on-symbol-overlay (&rest _)
      "Turn on symbol highlighting."
      (interactive)
      (when (derived-mode-p 'prog-mode 'yaml-mode)
        (symbol-overlay-mode 1)))
    (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay))
    (define-key symbol-overlay-map (kbd "h") nil)
    (define-key symbol-overlay-map (kbd "d") nil)
    (define-key symbol-overlay-map (kbd "c") nil)
    (define-key symbol-overlay-map (kbd "y") nil)
    (define-key symbol-overlay-map (kbd "g") 'symbol-overlay-map-help))

;; Colorize color names in buffers
(use-package rainbow-mode
  :straight t
  :ensure t
  :defer t
  :diminish
  :defines helpful-mode-map
  :bind (:map help-mode-map
         ("w" . rainbow-mode))
  :hook ((html-mode php-mode helpful-mode) . rainbow-mode)
  :init (with-eval-after-load 'helpful
          (bind-key "w" #'rainbow-mode helpful-mode-map))
  :config
  (with-no-warnings
    ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
    ;; @see https://emacs.stackexchange.com/questions/36420
    (defun my-rainbow-colorize-match (color &optional match)
      (let* ((match (or match 0))
             (ov (make-overlay (match-beginning match) (match-end match))))
        (overlay-put ov 'ovrainbow t)
        (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                  "white" "black"))
                                (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

    (defun my-rainbow-clear-overlays ()
      "Clear all rainbow overlays."
      (remove-overlays (point-min) (point-max) 'ovrainbow t))
    (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays)))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :straight t
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; keywords from doom-emacs
(use-package hl-todo
  :straight t
  :ensure t
  :defer t
  :custom-face
  (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :underline nil :inverse-video t))))
  :hook (after-init . global-hl-todo-mode)
  :init
  (setq hl-todo-require-punctuation t
        hl-todo-highlight-punctuation ":")
  :config
  ;; TODO: For things that need to be done, just not today.
  (dolist (keyword '("TODO"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#ffcb6b")))
  ;; FIXME: For problems that will become bigger problems later if not fixed ASAP.
  ;; BUG: For a known bug that needs a workaround
  (dolist (keyword '("FIXME" "BUG"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#ff5370")))
  ;; HACK: For tidbits that are unconventional and not intended
  ;;       uses of the constituent parts, and may break in a future update.
  ;; XXX: For warning about a problematic or misguiding code
  (dolist (keyword '("HACK" "XXX"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#f78b69")))
  ;; REVIEW: For things that were done hastily and/or hasn't been thoroughly
  ;;         tested. It may not even be necessary!
  (dolist (keyword '("REVIEW"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#89ddff")))
  ;; NOTE: For especially important gotchas with a given implementation,
  ;;       directed at another user other than the author.
  (dolist (keyword '("NOTE"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#c2e88b")))
  ;; DEPRECATED: For things that just gotta go and will soon be gone.
  (dolist (keyword '("DEPRECATED"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#8d9eaf"))))

(use-package diff-hl
  :straight t
  :ensure t
  :defer t
  :hook ((after-init . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode-unless-remote))
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode t)

  ;; Set fringe style
  (setq-default fringes-outside-margins nil)

  (with-no-warnings
    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector (if ON-LINUX #b11111100 #b11100000))
        1 8
        '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

    (unless (display-graphic-p)
      ;; Fall back to the display margin since the fringe is unavailable in tty
      (diff-hl-margin-mode 1)
      ;; Avoid restoring `diff-hl-margin-mode'
      (with-eval-after-load 'desktop
        (add-to-list 'desktop-minor-mode-table
                     '(diff-hl-margin-mode nil))))

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))
;; Pulse current line
(use-package pulse
  :ensure nil ;; built-in
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region :background unspecified))))
  (pulse-highlight-face ((t (:inherit region :background unspecified :extend t))))
  :hook (((dumb-jump-after-jump imenu-after-jump) . my-recenter-and-pulse)
         ((bookmark-after-jump magit-diff-visit-file next-error) . my-recenter-and-pulse-line))
  :init
  (with-no-warnings
    (defun my-pulse-momentary-line (&rest _)
      "Pulse the current line."
      (pulse-momentary-highlight-one-line (point)))

    (defun my-pulse-momentary (&rest _)
      "Pulse the region or the current line."
      (if (fboundp 'xref-pulse-momentarily)
          (xref-pulse-momentarily)
        (my-pulse-momentary-line)))

    (defun my-recenter-and-pulse(&rest _)
      "Recenter and pulse the region or the current line."
      (recenter)
      (my-pulse-momentary))

    (defun my-recenter-and-pulse-line (&rest _)
      "Recenter and pulse the current line."
      (recenter)
      (my-pulse-momentary-line))

    (dolist (cmd '(recenter-top-bottom
                   other-window switch-to-buffer
                   aw-select toggle-window-split
                   windmove-do-window-select
                   pager-page-down pager-page-up
                   treemacs-select-window
                   symbol-overlay-basic-jump))
      (advice-add cmd :after #'my-pulse-momentary-line))

    (dolist (cmd '(pop-to-mark-command
                   pop-global-mark
                   goto-last-change))
      (advice-add cmd :after #'my-recenter-and-pulse))))



(provide 'init-highlight)
;;; init-highlight.el ends here

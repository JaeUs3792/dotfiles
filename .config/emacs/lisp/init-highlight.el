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

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; keywords from doom-emacs
(use-package hl-todo
  :ensure (:wait t)
  :demand t
  :custom-face
  (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :underline nil :inverse-video t))))
  :init
  (setq hl-todo-require-punctuation t
        hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO"       . "#ffcb6b")    ;; TODO:
          ("FIXME"      . "#ff5370")    ;; FIXME:
          ("BUG"        . "#ff5370")    ;; BUG:
          ("HACK"       . "#f78b69")    ;; HACK:
          ("XXX"        . "#f78b69")    ;; XXX:
          ("REVIEW"     . "#89ddff")    ;; REVIEW:
          ("NOTE"       . "#c2e88b")    ;; NOTE:
          ("DEPRECATED" . "#8d9eaf")))  ;; DEPRECATED:
  :config
  (global-hl-todo-mode))

(use-package diff-hl
  :ensure (:wait t)
  :demand t
  :hook ((dired-mode . diff-hl-dired-mode-unless-remote))
  :config
  (global-diff-hl-mode)
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
  :custom
  (pulse-delay 0.04)
  (pulse-iterations 15)
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

;;; init-transient.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

(use-package transient
  :ensure (:wait t)
  :bind ("<f6>" . my/toggles)
  :config
  (defun my/toggle-line-numbers ()
    "Toggle line numbers."
    (interactive)
    (if (bound-and-true-p display-line-numbers-mode)
        (display-line-numbers-mode -1)
      (display-line-numbers-mode 1)))

  (defun my/toggle-trailing-whitespace ()
    "Toggle trailing whitespace display."
    (interactive)
    (setq-default show-trailing-whitespace (not show-trailing-whitespace))
    (message "Show trailing whitespace: %s" show-trailing-whitespace))

  (transient-define-prefix my/toggles ()
    "Global toggles."
    [["Basic"
      ("n" "line number" my/toggle-line-numbers :transient t)
      ;("a" "aggressive indent" global-aggressive-indent-mode :transient t)
      ("d" "hungry delete" global-hungry-delete-mode :transient t)
      ("e" "electric pair" electric-pair-mode :transient t)
      ("c" "spell check" flyspell-mode :transient t)
      ("s" "pretty symbol" prettify-symbols-mode :transient t)
      ("l" "page break lines" global-page-break-lines-mode :transient t)
      ("b" "battery" display-battery-mode :transient t)
      ("i" "time" display-time-mode :transient t)
      ("m" "modern mode-line" doom-modeline-mode :transient t)]
     ["Highlight"
      ("hl" "line" global-hl-line-mode :transient t)
      ("hp" "paren" show-paren-mode :transient t)
      ("hs" "symbol" symbol-overlay-mode :transient t)
("hw" "whitespace" my/toggle-trailing-whitespace :transient t)
      ("hd" "delimiter" rainbow-delimiters-mode :transient t)
      ("hi" "indent" highlight-indent-guides-mode :transient t)
      ("ht" "todo" global-hl-todo-mode :transient t)]
     ["Program"
      ("f" "flycheck" flycheck-mode :transient t)
      ("F" "flymake" flymake-mode :transient t)
      ("O" "hideshow" hs-minor-mode :transient t)
      ("u" "subword" subword-mode :transient t)
      ("W" "which function" which-function-mode :transient t)
      ("E" "debug on error" toggle-debug-on-error :transient t)
      ("Q" "debug on quit" toggle-debug-on-quit :transient t)
      ("v" "gutter" global-diff-hl-mode :transient t)
      ("V" "live gutter" diff-hl-flydiff-mode :transient t)
      ("M" "margin gutter" diff-hl-margin-mode :transient t)
      ("D" "dired gutter" diff-hl-dired-mode :transient t)]])

  ) ;; end use-package

(provide 'init-transient)
;;; init-transient.el ends here

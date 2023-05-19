;;; init-window.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

;; Window swap C-w C-r on evil
(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :hook (emacs-startup . popper-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Compile-Log\\*"
          help-mode
          compilation-mode))

  (with-eval-after-load 'doom-modeline
    (setq popper-mode-line
          '(:eval (let ((face (if (doom-modeline--active)
                                  'mode-line-emphasis
                                'mode-line-inactive)))
                    (if (and (icons-displayable-p)
                             (bound-and-true-p doom-modeline-mode))
                        (format " %s "
                                (nerd-icons-octicon "nf-oct-pin" :face face))
                      (propertize " POP" 'face face))))))
  :config
  (popper-echo-mode +1))
(provide 'init-window)
;;; init-window.el ends here

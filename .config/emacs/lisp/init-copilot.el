;; init-copilot.el -*- lexical-binding: t -*-
(add-to-list 'load-path "~/.config/emacs/copilot.el")
(require 'copilot)

(add-hook 'prog-mode-hook 'copilot-mode)

(provide 'init-copilot)
;;; init-copilot.el ends here

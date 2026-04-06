;;; init-vterm.el -*- lexical-binding: t -*-
;; vterm: fully-featured terminal emulator based on libvterm

(use-package vterm
  :ensure t
  :defer t
  :custom
  (vterm-max-scrollback 10000)
  (vterm-kill-buffer-on-exit t)
  :config
  (setq vterm-shell (or explicit-shell-file-name shell-file-name)))

(defun ju/vterm-toggle ()
  "Toggle vterm popup at the bottom."
  (interactive)
  (require 'vterm)
  (let ((buf-name (format "*vterm-popup:%s*"
                          (if (bound-and-true-p persp-mode)
                              (persp-current-name)
                            "main"))))
    (if-let* ((buf (get-buffer buf-name))
              (win (get-buffer-window buf)))
        (delete-window win)
      (let ((buf (get-buffer-create buf-name)))
        (with-current-buffer buf
          (unless (eq major-mode 'vterm-mode)
            (vterm-mode)))
        (display-buffer buf
                        '((display-buffer-in-side-window)
                          (side . bottom)
                          (slot . 0)
                          (window-height . 0.3)))
        (select-window (get-buffer-window buf))))))

(provide 'init-vterm)
;;; init-vterm.el ends here

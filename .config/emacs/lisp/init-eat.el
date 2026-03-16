;;; init-eat.el -*- lexical-binding: t -*-
;; Eat: Emulate A Terminal - modern terminal emulator for Emacs

(use-package eat
  :ensure (:host codeberg
           :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el")))
  :hook
  ;; Integrate eat with eshell
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode)
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-mouse t)
  :config
  ;; evil integration: use semi-char mode so evil bindings work outside input
  (with-eval-after-load 'evil
    (evil-set-initial-state 'eat-mode 'insert)
    (add-hook 'eat-mode-hook
              (lambda ()
                (setq-local evil-insert-state-cursor '(bar . 2))))))

(defun ju/eat-toggle ()
  "Toggle eat terminal popup."
  (interactive)
  (require 'eat)
  (let ((buf (get-buffer-create
              (format "*eat-popup:%s*"
                      (if (bound-and-true-p persp-mode)
                          (persp-current-name)
                        "main")))))
    (if-let* ((win (get-buffer-window buf)))
        (delete-window win)
      (with-current-buffer buf
        (unless (eq major-mode 'eat-mode)
          (eat-mode)
          (eat-exec buf "eat" (or explicit-shell-file-name shell-file-name) nil nil)))
      (display-buffer buf
                      '((display-buffer-in-side-window)
                        (side . bottom)
                        (slot . 0)
                        (window-height . 0.3)))
      (select-window (get-buffer-window buf)))))

(with-eval-after-load 'init-general
  (ju/leader-key-def
    "e t" '(ju/eat-toggle :which-key "toggle eat terminal")
    "e T" '(eat-other-window :which-key "eat other window")
    "e p" '(eat-project :which-key "eat project terminal")))

(provide 'init-eat)
;;; init-eat.el ends here

;;; init-persp.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

;; Windows/buffers sets shared among frames + save/load.
(use-package persp-mode
  :straight t
  :ensure t
  :defer t
  :diminish
  :autoload (get-current-persp persp-contain-buffer-p)
  :hook (after-init . persp-mode)
  :init (setq persp-keymap-prefix (kbd "C-x p")
              persp-nil-name "default"
              persp-set-last-persp-for-new-frames nil
              persp-kill-foreign-buffer-behaviour 'kill
              persp-auto-resume-time 0)
  :config
  (with-no-warnings
    ;; Don't save if the state is not loaded
    (defvar persp-state-loaded nil
      "Whether the state is loaded.")

    (defun my-persp-after-load-state (&rest _)
      (setq persp-state-loaded t))
    (advice-add #'persp-load-state-from-file :after #'my-persp-after-load-state)
    (add-hook 'emacs-startup-hook
              (lambda ()
                (add-hook 'find-file-hook #'my-persp-after-load-state)))

    (defun my-persp-asave-on-exit (fn &optional interactive-query opt)
      (if persp-state-loaded
          (funcall fn interactive-query opt)
        t))
    (advice-add #'persp-asave-on-exit :around #'my-persp-asave-on-exit))

  ;; Don't save dead or temporary buffers
  (add-hook 'persp-filter-save-buffers-functions
            (lambda (b)
              "Ignore dead and unneeded buffers."
              (or (not (buffer-live-p b))
                  (string-prefix-p " *" (buffer-name b)))))
  (add-hook 'persp-filter-save-buffers-functions
            (lambda (b)
              "Ignore temporary buffers."
              (let ((bname (file-name-nondirectory (buffer-name b))))
                (or (string-prefix-p ".newsrc" bname)
                    (string-prefix-p "magit" bname)
                    (string-prefix-p "COMMIT_EDITMSG" bname)
                    (string-prefix-p "Pfuture-Callback" bname)
                    (string-prefix-p "treemacs-persist" bname)
                    (string-match-p "\\.elc\\|\\.tar\\|\\.gz\\|\\.zip\\'" bname)
                    (string-match-p "\\.bin\\|\\.so\\|\\.dll\\|\\.exe\\'" bname)))))

  ;; Don't save persp configs in `recentf'
  (with-eval-after-load 'recentf
    (push persp-save-dir recentf-exclude))

  ;; Eshell integration
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))

  ;; Shell integration
  (persp-def-buffer-save/load
   :mode 'shell-mode :tag-symbol 'def-shell-buffer
   :mode-restore-function (lambda (_) (shell))
   :save-vars '(major-mode default-directory)))

;; Project integration
(use-package persp-mode-project-bridge
  :straight t
  :ensure t
  :defer t
  :autoload (persp-mode-project-bridge-find-perspectives-for-all-buffers
             persp-mode-project-bridge-kill-perspectives)
  :hook
  (persp-mode-project-bridge-mode . (lambda ()
                                      (if persp-mode-project-bridge-mode
                                          (persp-mode-project-bridge-find-perspectives-for-all-buffers)
                                        (persp-mode-project-bridge-kill-perspectives))))
  (persp-mode . persp-mode-project-bridge-mode)
  :init (when (icons-displayable-p)
          (setq persp-mode-project-bridge-persp-name-prefix ""))
  :config
  (with-no-warnings
    ;; HACK: Allow saving to files
    (defun my-persp-mode-project-bridge-add-new-persp (name)
      (let ((persp (persp-get-by-name name *persp-hash* :nil)))
        (if (eq :nil persp)
            (prog1
                (setq persp (persp-add-new name))
              (when persp
                (set-persp-parameter 'persp-mode-project-bridge t persp)
                (persp-add-buffer (cl-remove-if-not #'get-file-buffer (project-files (project-current)))
                                  persp nil nil)))
          persp)))
    (advice-add #'persp-mode-project-bridge-add-new-persp
                :override #'my-persp-mode-project-bridge-add-new-persp)

    ;; HACK: Switch to buffer after switching perspective
    (defun my-persp-mode-project-bridge-hook-switch (fn &rest _args)
      "Switch to a perspective when hook is activated."
      (let ((buf (current-buffer)))
        (funcall fn)
        (when (buffer-live-p buf)
          (switch-to-buffer buf))))
    (advice-add #'persp-mode-project-bridge-hook-switch
                :around #'my-persp-mode-project-bridge-hook-switch)))

(provide 'init-persp)
;;; init-persp.el ends here

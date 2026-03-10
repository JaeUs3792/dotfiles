;;; init-edit.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))
(use-package avy
  :ensure t
  :defer t
  :hook (after-init . avy-setup-default)
  :config
  (setq avy-style 'de-bruijn
		avy-all-windows t
		avy-all-windows-alt nil
		avy-background t))
(use-package undo-tree
  :ensure t
  :demand t
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name (file-name-as-directory "undo-tree-hist")
                               user-emacs-directory))))
  :config
  (setq undo-tree-visualizer-diff       t
        undo-tree-visualizer-timestamps t
        undo-tree-auto-save-history     t
        undo-tree-enable-undo-in-region t
        undo-limit        (* 800 1024)
        undo-strong-limit (* 12 1024 1024)
        undo-outer-limit  (* 128 1024 1024)))

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :bind ("C-~" . my/hideshow-menu)
  :hook (prog-mode . hs-minor-mode)
  :config
  ;; More functions
  ;; @see https://karthinks.com/software/simple-folding-with-hideshow/
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-toggle-all ()
    "Toggle hide/show all."
    (interactive)
    (pcase last-command
      ('hs-toggle-all
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; Display line counts
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (concat
                    " "
                    (propertize
                     (if (char-displayable-p ?⏷) "⏷" "...")
                     'face 'shadow)
                    (propertize
                     (format " (%d lines)"
                             (count-lines (overlay-start ov)
                                          (overlay-end ov)))
                     'face '(:inherit shadow :height 0.8))
                    " "))))
  (setq hs-set-up-overlay #'hs-display-code-line-counts)

  (require 'transient)
  (transient-define-prefix my/hideshow-menu ()
    "HideShow fold menu."
    [["Fold"
      ("t" "toggle all" hs-toggle-all :transient t)
      ("a" "show all" hs-show-all :transient t)
      ("i" "hide all" hs-hide-all :transient t)
      ("g" "toggle hiding" hs-toggle-hiding :transient t)
      ("c" "cycle block" hs-cycle :transient t)
      ("s" "show block" hs-show-block :transient t)
      ("h" "hide block" hs-hide-block :transient t)
      ("l" "hide level" hs-hide-level :transient t)]]))

;; Hanlde minified code
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

;; remote edit
(use-package tramp
  :ensure nil
  :custom
  (tramp-default-method "ssh")
  (tramp-connection-timeout 10)
  ;; Performance
  (tramp-verbose 1)                        ; 로그 최소화로 속도 향상
  (tramp-completion-reread-directory-timeout nil) ; 원격 디렉토리 캐시 유지
  (remote-file-name-inhibit-cache nil)     ; 원격 파일 캐시 활성화
  (vc-ignore-dir-regexp                    ; 원격에서 vc 비활성화 (속도)
   (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))
  ;; Auto-save to local tmp
  (tramp-auto-save-directory
   (expand-file-name "tramp-autosave/" user-emacs-directory))
  ;; Backup files locally
  (tramp-backup-directory-alist backup-directory-alist)
  :config
  ;; Use ssh ControlMaster for persistent connections
  (setq tramp-ssh-controlmaster-options
        (concat "-o ControlMaster=auto "
                "-o ControlPath=~/.ssh/sockets/%%r@%%h:%%p "
                "-o ControlPersist=600"))
  ;; Exclude remote files from recentf
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude tramp-file-name-regexp))
  ;; Projectile: don't search remote dirs
  (with-eval-after-load 'projectile
    (advice-add 'projectile-project-root :before-while
                (lambda (&rest _) (not (file-remote-p default-directory))))))

(provide 'init-edit)
;;; init-edit.el ends here.

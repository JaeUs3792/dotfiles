;;; init-vcs.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

;; Git
;; See `magit-maybe-define-global-key-bindings'
(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
  :init (setq magit-diff-refine-hunk t)
  :config
  (setq magit-clone-default-directory "~/Project/")
  (when ON-WINDOWS
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  ;; Exterminate Magit buffers
  (with-no-warnings
    (defun my-magit-kill-buffers (&rest _)
      "Restore window configuration and kill all Magit buffers."
      (interactive)
      (magit-restore-window-configuration)
      (let ((buffers (magit-mode-get-buffers)))
        (when (eq major-mode 'magit-status-mode)
          (mapc (lambda (buf)
                  (with-current-buffer buf
                    (if (and magit-this-process
                             (eq (process-status magit-this-process) 'run))
                        (bury-buffer buf)
                      (kill-buffer buf))))
                buffers))))
    (setq magit-bury-buffer-function #'my-magit-kill-buffers)))

;; Show TODOs in magit
(use-package magit-todos
  :ensure t
  :defines magit-todos-nice
  :commands magit-todos--scan-with-git-grep
  :init
  (setq magit-todos-nice (if (executable-find "nice") t nil))
  (setq magit-todos-scanner #'magit-todos--scan-with-git-grep)
  (setq magit-todos-exclude-globs '(".git/" "*.json" "*.js"))
  (let ((inhibit-message t))
    (magit-todos-mode 1)))

;; Walk through git revisions of a file
(use-package git-timemachine
  :ensure t
  :defer t
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success :foreground unspecified))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning :foreground unspecified))))
  :hook ((git-timemachine-mode
          . (lambda ()
              "Improve `git-timemachine' buffers."
              ;; Display different colors in mode-line
              (if (facep 'mode-line-active)
                  (face-remap-add-relative 'mode-line-active 'custom-state)
                (face-remap-add-relative 'mode-line 'custom-state))

              ;; Highlight symbols in elisp
              (and (derived-mode-p 'emacs-lisp-mode)
                   (fboundp 'highlight-defined-mode)
                   (highlight-defined-mode t))

              ;; Display line numbers
              (and (derived-mode-p 'prog-mode 'yaml-mode)
                   (fboundp 'display-line-numbers-mode)
                   (display-line-numbers-mode t))))
         (before-revert . (lambda ()
                            (when (bound-and-true-p git-timemachine-mode)
                              (user-error "Cannot revert the timemachine buffer"))))))
;; Resolve diff3 conflicts
(use-package smerge-mode
  :ensure nil
  :defer t
  :diminish
  :bind (:map smerge-mode-map
              ("C-c m" . my/smerge-menu))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (my/smerge-menu)))))
  :config
  (require 'transient)
  (transient-define-prefix my/smerge-menu ()
    "Smerge menu."
    [["Move"
      ("n" "next" smerge-next :transient t)
      ("p" "previous" smerge-prev :transient t)]
     ["Keep"
      ("b" "base" smerge-keep-base :transient t)
      ("u" "upper" smerge-keep-upper :transient t)
      ("l" "lower" smerge-keep-lower :transient t)
      ("a" "all" smerge-keep-all :transient t)
      ("RET" "current" smerge-keep-current :transient t)]
     ["Diff"
      ("<" "upper/base" smerge-diff-base-upper :transient t)
      ("=" "upper/lower" smerge-diff-upper-lower :transient t)
      (">" "base/lower" smerge-diff-base-lower :transient t)
      ("R" "refine" smerge-refine :transient t)
      ("E" "ediff" smerge-ediff)]
     ["Other"
      ("C" "combine" smerge-combine-with-next :transient t)
      ("r" "resolve" smerge-resolve :transient t)
      ("k" "kill" smerge-kill-current :transient t)
      ("ZZ" "save and bury" (lambda ()
                              (interactive)
                              (save-buffer)
                              (bury-buffer)))]]))
;; Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :ensure t
  :defer t
  :bind (:map vc-prefix-map
              ("B" . browse-at-remote)))

;; Git configuration modes (ex: gitignore gitattributes gitconfig)
(use-package git-modes
  :ensure t
  :defer t)


;; Ediff
(use-package ediff
  :ensure nil
  :defer t
  :config
  (setq ediff-split-window-function 'split-window-horizontally) ; 좌우 분할
  (add-hook 'ediff-mode-hook 'evil-emacs-state))               ; evil 키 충돌 방지

(provide 'init-vcs)
;;; init-vcs.el ends here

;;; init-vcs.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

;; Git
;; See `magit-maybe-define-global-key-bindings'
(use-package magit
  :ensure nil
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
  :straight t
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
  :straight t
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
  :straight t
  :ensure t
  :defer t
  :diminish
  :pretty-hydra
  ((:title (pretty-hydra-title "Smerge" 'octicon "nf-oct-diff")
           :color pink :quit-key ("q" "C-g"))
   ("Move"
    (("n" smerge-next "next")
     ("p" smerge-prev "previous"))
    "Keep"
    (("b" smerge-keep-base "base")
     ("u" smerge-keep-upper "upper")
     ("l" smerge-keep-lower "lower")
     ("a" smerge-keep-all "all")
     ("RET" smerge-keep-current "current")
     ("C-m" smerge-keep-current "current"))
    "Diff"
    (("<" smerge-diff-base-upper "upper/base")
     ("=" smerge-diff-upper-lower "upper/lower")
     (">" smerge-diff-base-lower "upper/lower")
     ("R" smerge-refine "refine")
     ("E" smerge-ediff "ediff"))
    "Other"
    (("C" smerge-combine-with-next "combine")
     ("r" smerge-resolve "resolve")
     ("k" smerge-kill-current "kill")
     ("ZZ" (lambda ()
             (interactive)
             (save-buffer)
             (bury-buffer))
      "Save and bury buffer" :exit t))))
  :bind (:map smerge-mode-map
              ("C-c m" . smerge-mode-hydra/body))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-mode-hydra/body))))))
;; Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :straight t
  :ensure t
  :defer t
  :bind (:map vc-prefix-map
              ("B" . browse-at-remote)))

;; Git configuration modes (ex: gitignore gitattributes gitconfig)
(use-package git-modes
  :straight t
  :ensure t
  :defer t)


(provide 'init-vcs)
;;; init-vcs.el ends here

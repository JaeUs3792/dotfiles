;;; init-dashboard.el -*- lexical-binding: t -*-
(require 'init-funcs)

(use-package dashboard
  :straight t
  :ensure t
  :diminish dashboard-mode
  :functions (nerd-icons-faicon
              nerd-icons-mdicon
              winner-undo
              widget-forward)
  :custom-face
  (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  (dashboard-items-face ((t (:weight normal))))
  (dashboard-no-items-face ((t (:weight normal))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Dashboard" 'mdicon "nf-md-view_dashboard")
           :color pink :quit-key ("q" "C-g"))
   ("Navigator"
    (("U" update-dotfiles-and-packages "update" :exit t)
     ("H" browse-homepage "homepage" :exit t)
     ("R" restore-previous-session "recover session" :exit t)
     ("L" restore-session "list sessions" :exit t)
     ("S" find-custom-file "settings" :exit t))
    "Section"
    (("}" dashboard-next-section "next")
     ("{" dashboard-previous-section "previous")
     ("r" dashboard-goto-recent-files "recent files")
     ("m" dashboard-goto-bookmarks "bookmarks")
     ("p" dashboard-goto-projects "projects"))
    "Item"
    (("RET" widget-button-press "open" :exit t)
     ("<tab>" widget-forward "next")
     ("C-i" widget-forward "next")
     ("<backtab>" widget-backward "previous")
     ("C-n" next-line "next line")
     ("C-p" previous-line "previous  line"))
    "Misc"
    (("<f2>" open-dashboard "open" :exit t)
     ("g" dashboard-refresh-buffer "refresh" :exit t)
     ("Q" quit-dashboard "quit" :exit t))))
  :bind (("<f2>" . open-dashboard)
         :map dashboard-mode-map
         ("H" . browse-homepage)
         ("R" . restore-previous-session)
         ("L" . restore-session)
         ("S" . find-custom-file)
         ("U" . update-dotfiles-and-packages)
         ("q" . quit-dashboard)
         ("h" . dashboard-hydra/body)
         ("?" . dashboard-hydra/body))
  :hook (dashboard-mode . (lambda ()
                            ;; No title
                            (setq-local frame-title-format nil)
                            ;; Enable `page-break-lines-mode'
                            (when (fboundp 'page-break-lines-mode)
                              (page-break-lines-mode 1))))
  :init
  (setq dashboard-page-separator "\n\f\f\n"
        ;;dashboard-banner-logo-title "Emacs is more than a text editor!"
        ;; logo from github.com/egstatsml/emacs_fancy_logos
        dashboard-startup-banner "~/.config/emacs/logos/xemacs_color.svg"
        dashboard-projects-backend 'project-el
        dashboard-path-style 'truncate-middle
        dashboard-path-max-length 60
        dashboard-center-content t
        dashboard-show-shortcuts t
        dashboard-items '((recents  . 10)
                          (projects . 5)
                          (agenda . 5)
                          (bookmarks . 5)
                          (registers . 5))

        dashboard-set-init-info t
        dashboard-display-icons-p #'icons-displayable-p
        dashboard-icon-type 'nerd-icons
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-heading-icons '((recents   . "nf-oct-history")
                                  (bookmarks . "nf-oct-bookmark")
                                  (agenda    . "nf-oct-calendar")
                                  (projects  . "nf-oct-briefcase")
                                  (registers . "nf-oct-database"))

        dashboard-set-footer t
        dashboard-footer-icon (cond
                               ((icons-displayable-p)
                                (nerd-icons-octicon "nf-oct-heart" :height 1.2 :face 'nerd-icons-lred))

                               (t (propertize ">" 'face 'dashboard-footer)))

        dashboard-set-navigator t
        dashboard-navigator-buttons
        `(((,(when (icons-displayable-p)
               (nerd-icons-mdicon "nf-md-github" :height 1.5))
            " Github"
            "Browse my repository"
            (lambda (&rest _) (browse-url "https://github.com/JaeUs3792/")))
           (,(when (icons-displayable-p)
               (nerd-icons-mdicon "nf-md-home" :height 1.5))
            " Homepage"
            "Browse my homepage"
            (lambda (&rest _) (browse-url "https://jaeus.net")))
           (,(when (icons-displayable-p)
               (nerd-icons-mdicon "nf-md-backup_restore" :height 1.5))
            " Restore"
            "Restore previous session"
            (lambda (&rest _) (restore-previous-session)))
           (,(when (icons-displayable-p)
               (nerd-icons-mdicon "nf-md-update" :height 1.5))
            " Update"
            "Update dotfiles and packages"
            (lambda (&rest _) (update-dotfiles-and-packages)))
           (,(if (icons-displayable-p)
                 (nerd-icons-mdicon "nf-md-help" :height 1.5)
               "?")
            "" "Help (?/h)"
            (lambda (&rest _) (dashboard-hydra/body))
            font-lock-string-face))))

  (dashboard-setup-startup-hook)
  :config
  ;; Insert copyright
  ;; @see https://github.com/emacs-dashboard/emacs-dashboard/issues/219
  (defun restore-previous-session ()
    "Restore the previous session."
    (interactive)
    (when (bound-and-true-p persp-mode)
      (restore-session persp-auto-save-fname)))

  (defun restore-session (fname)
    "Restore the specified session."
    (interactive (list (read-file-name "Load perspectives from a file: "
                                       persp-save-dir)))
    (when (bound-and-true-p persp-mode)
      (message "Restoring session...")
      (quit-window t)
      (condition-case-unless-debug err
          (persp-load-state-from-file fname)
        (error "Error: Unable to restore session -- %s" err))
      (message "Restoring session...done")))

  (defun dashboard-goto-recent-files ()
    "Go to recent files."
    (interactive)
    (let ((func (local-key-binding "r")))
      (and func (funcall func))))

  (defun dashboard-goto-projects ()
    "Go to projects."
    (interactive)
    (let ((func (local-key-binding "p")))
      (and func (funcall func))))

  (defun dashboard-goto-bookmarks ()
    "Go to bookmarks."
    (interactive)
    (let ((func (local-key-binding "m")))
      (and func (funcall func))))

  (defvar dashboard-recover-layout-p nil
    "Wether recovers the layout.")

  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    ;; Check if need to recover layout
    (if (length> (window-list-1)
                 ;; exclude `treemacs' window
                 (if (and (fboundp 'treemacs-current-visibility)
                          (eq (treemacs-current-visibility) 'visible))
                     2
                   1))
        (setq dashboard-recover-layout-p t))

    ;; Display dashboard in maximized window
    (delete-other-windows)

    ;; Refresh dashboard buffer
    (dashboard-refresh-buffer)

    ;; Jump to the first section
    (dashboard-goto-recent-files))

  (defun quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (and dashboard-recover-layout-p
         (and (bound-and-true-p winner-mode) (winner-undo))
         (setq dashboard-recover-layout-p nil))))


;;  (setq dashboard-week-agenda t)
;;  (setq dashboard-agenda-time-string-format "%d/%m/%Y %A %H:%M")

;;  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))

(provide 'init-dashboard)
;;; init-dashboard.el ends here

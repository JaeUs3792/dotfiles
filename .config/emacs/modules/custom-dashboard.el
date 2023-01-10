;;; custom-dashboard.el -*- lexical-binding: t; -*-
(crafted-package-install-package 'dashboard)

(require 'dashboard)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
(setq dashboard-center-content t) ;; set to 't' for centered content
(setq dashboard-items '((recents . 10)
                        (bookmarks . 5)))
                        ;;(projects . 10))) // TODO after projectile
(setq dashboard-set-footer t)
(setq dashboard-page-separator "\n\n\f\n\n")
(setq dashboard-set-navigator t)
;; Format: "(icon title help action face prefix suffix)"
(setq dashboard-navigator-buttons
      `(;; line1
        ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
          "Github"
          "Browse my Github"
          (lambda (&rest _) (browse-url "https://github.com/JaeUs3792/")))
         (,(all-the-icons-octicon "home" :height 1.1 :v-adjust 0.0)
          "Homepage"
          "Browse my Homepage"
          (lambda (&rest _) (browse-url "https://jaeus.net"))))))

(dashboard-setup-startup-hook)
(dashboard-modify-heading-icons '((recents . "file-text")
                                  (bookmarks . "book")))

(setq doom-fallback-buffer-name "*dashboard*")

(require 'linum)
(add-hook 'dashboard-mode-hook page-break-lines-mode)


(provide 'custom-dashboard)
;;; custom-dashboard.el end here

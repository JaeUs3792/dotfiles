;;; init-reader.el -*- lexical-binding: t -*-

;; Nice reading / writing
(use-package olivetti
  :straight t
  :ensure t
  :defer t
  :diminish
  :bind ("<f7>" . olivetti-mode)
  :hook ((markdown-mode . olivetti-mode)
         (org-mode . olivetti-mode))
  :init (setq olivetti-body-width 0.62))

(unless ON-WINDOWS
  (use-package pdf-tools
			   :straight t
			   :ensure t
			   :defer t
			   :defines pdf-annot-activate-created-annotations
			   :hook ((pdf-tools-enabled . pdf-view-auto-slice-minor-mode)
					  (pdf-tools-enabled . pdf-isearch-minor-mode))
					  ;;(pdf-tools-enabled . pdf-view-themed-minor-mode))
			   :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
			   :magic ("%PDF" . pdf-view-mode)
			   :bind (:map pdf-view-mode-map
						   ("C-s" . isearch-forward))
			   :init
			   (pdf-tools-install)
			   :config
			   (setq-default pdf-view-display-size 'fit-page)
			   (setq-default pdf-view-resize-factor 1.1) ;; zoom in/out setting
			   (defun my/pdf-view-open-in-zathura ()
				 (interactive)
				 (save-window-excursion
				   (let ((current-file (buffer-file-name))
						 (current-page (number-to-string (pdf-view-current-page))))
					 (async-shell-command
					   (format "zathura -P %s \"%s\"" current-page current-file))))
				 (message "Sent to zathura"))))

;; Recover last viewed position
(use-package pdf-view-restore
  :straight t
  :ensure t
  :defer t
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename
        (expand-file-name "pdf-view-restore" user-emacs-directory)))

;; Epub reader
(use-package nov
  :straight t
  :ensure t
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . my-nov-setup)
  :init
  (defun my-nov-setup ()
    "Setup `nov-mode' for better reading experience."
    (visual-line-mode 1)
    (face-remap-add-relative 'variable-pitch :family "NanumGothic" :height 1.5))
  :config
  (with-no-warnings
    ;; WORKAROUND: errors while opening `nov' files with Unicode characters
    ;; @see https://github.com/wasamasa/nov.el/issues/63
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (let* ((name (nov-content-unique-identifier-name content))
             (selector (format "package>metadata>identifier[id='%s']"
                               (regexp-quote name)))
             (id (car (esxml-node-children (esxml-query selector content)))))
        (and id (intern id))))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier))

  ;; Fix encoding issue on Windows
  (when ON-WINDOWS
    (setq process-coding-system-alist
          (cons `(,nov-unzip-program . (gbk . gbk))
                process-coding-system-alist))))
;; Atom/RSS reader
(use-package elfeed
  :straight t
  :ensure t
  :defer t
  :pretty-hydra
  ((:title (pretty-hydra-title "Elfeed" 'faicon "nf-fa-rss_square" :face 'nerd-icons-orange)
           :color amaranth :quit-key ("q" "C-g"))
   ("Search"
    (("c" elfeed-db-compact "compact db")
     ("g" elfeed-search-update--force "refresh")
     ("G" elfeed-search-fetch "update")
     ("y" elfeed-search-yank "copy URL")
     ("+" elfeed-search-tag-all "tag all")
     ("-" elfeed-search-untag-all "untag all"))
    "Filter"
    (("l" elfeed-search-live-filter "live filter")
     ("s" elfeed-search-set-filter "set filter")
     ("*" (elfeed-search-set-filter "@6-months-ago +star") "starred")
     ("a" (elfeed-search-set-filter "@6-months-ago") "all")
     ("t" (elfeed-search-set-filter "@1-day-ago") "today"))
    "Article"
    (("b" elfeed-search-browse-url "browse")
     ("n" next-line "next")
     ("p" previous-line "previous")
     ("u" elfeed-search-tag-all-unread "mark unread")
     ("r" elfeed-search-untag-all-unread "mark read")
     ("RET" elfeed-search-show-entry "show"))))
  ;;:init (setq url-queue-timeout 30
  ;;            elfeed-show-entry-switch #'pop-to-buffer
  ;;            elfeed-show-entry-delete #'delete-window)
  :config
  (evil-collection-define-key 'normal 'elfeed-search-mode-map "?" 'elfeed-hydra/body)
  (evil-collection-define-key 'normal 'elfeed-show-mode-map "q" 'delete-window)
  ;; Ignore db directory in recentf
  (push elfeed-db-directory recentf-exclude))

(use-package elfeed-goodies
  :straight t
  :ensure t
  :defer t
  :hook (after-init . elfeed-goodies/setup))
(use-package elfeed-org
  :straight t
  :ensure t
  :after elfeed
  :config
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed.org" org-directory)))
  (elfeed-org))

(provide 'init-reader)
;;; init-reader.el ends here

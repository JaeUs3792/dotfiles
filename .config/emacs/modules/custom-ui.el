(use-package doom-themes)
(if (display-graphic-p)
    (load-theme 'doom-palenight t)
  (load-theme 'doom-gruvbox t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(setq visible-bell t)
  (set-frame-parameter nil 'alpha-background 0.9)
  (add-to-list 'default-frame-alist '(alpha-background . 0.9))
  (defun toggle-transparency ()
    "toggle transparency."
    (interactive)
    (let ((alpha-transparency 1.0))
      (if (eq (frame-parameter nil 'alpha-background) alpha-transparency)
          (set-frame-parameter nil 'alpha-background 0.9)
        (set-frame-parameter nil 'alpha-background alpha-transparency))))
(defun my/transparency-round (val)
  "Round VAL to the nearest tenth of an integer."
  (/ (round (* 10 val)) 10.0))

(defun my/increase-frame-alpha-background ()
  "Increase current frame’s alpha background."
  (interactive)
  (set-frame-parameter nil
                       'alpha-background
                       (my/transparency-round
                        (min 1.0
                             (+ (frame-parameter nil 'alpha-background) 0.1))))
  (message "%s" (frame-parameter nil 'alpha-background)))

(defun my/decrease-frame-alpha-background ()
  "Decrease current frame’s alpha background."
  (interactive)
  (set-frame-parameter nil
                       'alpha-background
                       (my/transparency-round
                        (max 0.0
                             (- (frame-parameter nil 'alpha-background) 0.1))))
  (message "%s" (frame-parameter nil 'alpha-background)))

(use-package all-the-icons)
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 15
        doom-modeline-env-version t
        doom-modeline-indent-info t))

(use-package writeroom-mode
  :disabled
  :defer t
  :straight (:build t)
  :init (global-writeroom-mode 1)
  :config
  (setq writeroom-width             100
        writeroom-fullscreen-effect nil
        writeroom-maximize-window   nil
        writeroom-mode-line         t
        writeroom-major-modes       '(text-mode org-mode markdown-mode nov-mode Info-mode)))

(defun write-room-enable ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
(use-package visual-fill-column
  :hook
  (org-mode . write-room-enable)
  (text-mode . write-room-enable)
  (markdown-mode . write-room-enable)
  (nov-mode . write-room-enable))

(use-package page-break-lines)
(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-projects-backend "project-el"
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-center-content t ;; set to 't' for centered content
        dashboard-items '((recents . 10)
                          (bookmarks . 5))
        ;;(projects . 10)))
        dashboard-set-footer t
        dashboard-page-separator "\n\f\n"
        dashboard-startup-banner 'logo ;; use standard emacs logo as banner
        dashboard-set-navigator t)

  ;;(setq dashboard-startup-banner "~/.config/doom/doom-emacs-dash.png")  ;; use custom image as banner
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
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))
(setq doom-fallback-buffer-name "*dashboard*")

(defun self-screenshot (&optional type)
  "Save a screenshot of type TYPE of the current Emacs frame.
As shown by the function `', type can weild the value `svg',
`png', `pdf'.

This function will output in /tmp a file beginning with \"Emacs\"
and ending with the extension of the requested TYPE."
  (interactive)
  (let* ((type (if type type
                 (intern (completing-read "Screenshot Type: "
                                          '(png svg pdf postscript)))))
         (extension (pcase type
                      ('png        ".png")
                      ('svg        ".svg")
                      ('pdf        ".pdf")
                      ('postscript ".ps")
                      (otherwise (error "Cannot export screenshot of type %s" otherwise))))
         (filename (make-temp-file "Emacs-" nil extension))
         (data     (x-export-frames nil type)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (rename-file filename (expand-file-name (file-name-nondirectory filename) "~"))
    (message filename)))

(provide 'custom-ui)
;;; custom-ui.el ends here

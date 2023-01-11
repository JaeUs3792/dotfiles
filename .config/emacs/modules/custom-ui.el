(use-package doom-themes
  ;;:init (load-theme 'doom-one t))
  :init (load-theme 'doom-palenight t))

(setq visible-bell t)
(set-frame-parameter nil 'alpha-background 1.0)
(add-to-list 'default-frame-alist '(alpha-background . 1.0))
(defun toggle-ttt ()
  "toggle transparency."
  (interactive)
  (let ((alpha-transparency 1.0))
    (if (eq (frame-parameter nil 'alpha-background) alpha-transparency)
        (set-frame-parameter nil 'alpha-background 0.7)
      (set-frame-parameter nil 'alpha-background alpha-transparency))))

(use-package all-the-icons)
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 15
        doom-modeline-env-version t
        doom-modeline-indent-info t))

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

;;; init-funcs.el -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'init-const)

(defun font-installed-p (font-name)
  (find-font (font-spec :name font-name)))
(defun too-long-file-p ()
  "Check whether the file is too long."
  (if (fboundp 'buffer-line-statistics)
      (> (car (buffer-line-statistics)) 10000)
    (> (buffer-size) 100000)))
(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (or (featurep 'nerd-icons)
      (require 'nerd-icons nil t)))
;;(or (featurep 'all-the-icons)
;;    (require 'all-the-icons nil t)))
(defun childframe-workable-p ()
  "Whether childframe is workable."
  (or (not (or noninteractive
               emacs-basic-display
               (not (display-graphic-p))))
      (daemonp)))


(defun toggle-transparency ()
  (interactive)
  (let ((current-alpha (frame-parameter nil 'alpha-background)))
    (let ((new-alpha (if (or (null current-alpha) (= current-alpha 100)) 85 100)))
      (set-frame-parameter nil 'alpha-background new-alpha)
      (message "Transparency set to: %d" new-alpha))))

(defun my/transparency-round (val)
  "Round VAL to the nearest tenth of an integer."
  (/ (round (* 10 val)) 10.0))

(defun my/update-dotfiles ()
  "update dotfiles & packages"
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (unless (file-exists-p dir)
      (user-error "\"%s\" doesn't exist" dir))
    (message "pull dotfiles from github")
    (cd dir)
    (shell-command "git pull")
    (message "pull... done")))
(defun update-packages ()
  "Refresh package contents and update all packages."
  (interactive)
  (message "Updating packages...")
  (package-upgrade-all)
  (message "Updating packages...done"))
(defun update-dotfiles-and-packages()
  "Update dotfiles and packages."
  (interactive)
  (my/update-dotfiles)
  (update-packages))

(defun my/increase-global-font-size ()
  (interactive)
  (let ((new-height (+ (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height new-height)))

(defun my/decrease-global-font-size ()
  (interactive)
  (let ((new-height (- (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height (max new-height 60)))) ;; 최소 크기 제한

(provide 'init-funcs)
;;; init-funcs.el ends here

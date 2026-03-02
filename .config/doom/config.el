;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "JaeYoo-Im"
      user-mail-address "cpu3792@gmail.com")
(setq default-input-method "korean-hangul")

;; Transparency
(defun my/set-transparency (&optional frame)
  (set-frame-parameter (or frame (selected-frame)) 'alpha '(85 . 85)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/set-transparency)
  (my/set-transparency))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 85) '(100 . 100)))))

(map! :leader
      :desc "Toggle transparency" "t a" #'toggle-transparency)

;; Fonts
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 20)
      doom-symbol-font (font-spec :family "Symbola"))

(defun my/setup-fontsets ()
  ;; Emoji
  (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
           when (find-font (font-spec :name font))
           return (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))
  ;; Korean/CJK
  (cl-loop for font in '("Noto Sans CJK KR" "Nanum Gothic")
           when (find-font (font-spec :name font))
           return (progn
                    (setq face-font-rescale-alist `((,font . 1.00)))
                    (set-fontset-font t '(#x1100 . #xffdc) (font-spec :family font)))))

(add-hook 'after-setting-font-hook #'my/setup-fontsets)


(setq doom-theme 'doom-tomorrow-night)
(setq display-line-numbers-type 'relative)
(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/agenda/agenda.org"))
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; Org super agenda
(use-package! org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "오늘 할 일"
           :time-grid t
           :scheduled today
           :deadline today)
          (:name "중요"
           :priority "A")
          (:name "지난 일정"
           :scheduled past
           :deadline past)
          (:name "예정"
           :scheduled future
           :deadline future)
          (:name "기타"
           :anything t))))


;; General
(setq which-key-idle-delay 0.5)

(map! "C-=" #'text-scale-increase
      "C--" #'text-scale-decrease)

(map! "C-s" #'consult-line
      "C-j" nil
      "C-k" nil)

;; Nov (epub reader)
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . my-nov-setup)
  :config
  (defun my-nov-setup ()
    (visual-line-mode 1)
    (face-remap-add-relative 'variable-pitch :family "NanumGothic" :height 1.5))
  ;; Workaround: errors with Unicode characters in nov files
  (defun my-nov-content-unique-identifier (content)
    (let* ((name (nov-content-unique-identifier-name content))
           (selector (format "package>metadata>identifier[id='%s']"
                             (regexp-quote name)))
           (id (car (esxml-node-children (esxml-query selector content)))))
      (and id (intern id))))
  (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier))

;; Olivetti
(after! olivetti
  (setq olivetti-body-width 0.62)
  (add-hook 'olivetti-mode-on-hook  (lambda () (display-line-numbers-mode -1)))
  (add-hook 'olivetti-mode-off-hook (lambda () (display-line-numbers-mode +1))))
(add-hook 'markdown-mode-hook #'olivetti-mode)
(add-hook 'org-mode-hook #'olivetti-mode)
(map! "<f7>" #'olivetti-mode)

;; Ztree
(after! ztree
  (setq ztree-draw-unicode-lines t
        ztree-show-number-of-children t)
  (custom-set-faces
   '(ztreep-header-face           ((t (:inherit diff-header))))
   '(ztreep-arrow-face            ((t (:inherit font-lock-comment-face))))
   '(ztreep-leaf-face             ((t (:inherit diff-index))))
   '(ztreep-node-face             ((t (:inherit font-lock-variable-name-face))))
   '(ztreep-expand-sign-face      ((t (:inherit font-lock-function-name-face))))
   '(ztreep-diff-header-face      ((t (:inherit (diff-header bold)))))
   '(ztreep-diff-header-small-face ((t (:inherit diff-file-header))))
   '(ztreep-diff-model-normal-face ((t (:inherit font-lock-doc-face))))
   '(ztreep-diff-model-ignored-face ((t (:inherit font-lock-doc-face :strike-through t))))
   '(ztreep-diff-model-diff-face  ((t (:inherit diff-removed))))
   '(ztreep-diff-model-add-face   ((t (:inherit diff-nonexistent))))))
(map! :leader
      :prefix ("z T" . "ztree")
      :desc "Directory tree" "d" #'ztree-dir
      :desc "Diff dirs"      "D" #'ztree-diff)

;; Magit todos
(after! magit
  (magit-todos-mode 1))

;; List environment
(after! list-environment
  (defun my-list-environment-entries ()
    "Generate environment variable entries list for tabulated-list."
    (mapcar (lambda (env)
              (let* ((kv (split-string env "="))
                     (key (car kv))
                     (val (mapconcat #'identity (cdr kv) "=")))
                (list key (vector
                           `(,key face font-lock-keyword-face)
                           `(,val face font-lock-string-face)))))
            process-environment))
  (advice-add #'list-environment-entries :override #'my-list-environment-entries))

;; Rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Symbol overlay
(map! "M-i" #'symbol-overlay-put
      "M-o" #'symbol-overlay-remove-all)

;; Hideshow (folding)
(map! :leader
      :prefix ("z" . "fold")
      :desc "Toggle fold"  "t" #'hs-toggle-hiding
      :desc "Hide all"     "a" #'hs-hide-all
      :desc "Show all"     "s" #'hs-show-all)

;; Trash linked file in org
(defun my/org-trash-linked-file ()
  "Trash the file linked at point."
  (interactive)
  (let* ((context (org-element-context))
         (type    (org-element-type context))
         (path    (when (eq type 'link)
                    (expand-file-name (org-element-property :path context)))))
    (cond
     ((null path)          (user-error "No link at point"))
     ((not (file-exists-p path)) (user-error "File not found: %s" path))
     ((yes-or-no-p (format "Trash %s? " path))
      (move-file-to-trash path)
      (message "Trashed: %s" path)))))

;; Org download
(after! org-download
  (setq org-download-method 'directory
        org-download-image-dir "./images"
        org-download-heading-lvl nil
        org-download-abbreviate-filename-function #'file-relative-name
        org-download-link-format "[[file:%s]]\n"))

;; Verilog
(after! verilog-mode
  (setq verilog-indent-level             4
        verilog-indent-level-module      0
        verilog-indent-level-declaration 0
        verilog-indent-level-behavioral  0
        verilog-indent-level-directive   0
        verilog-indent-lists             nil
        verilog-cexp-indent              4
        verilog-case-indent              4
        verilog-auto-newline             nil))

(use-package! verilog-ext
  :hook (verilog-mode . verilog-ext-mode)
  :init
  (setq verilog-ext-feature-list
        '(font-lock
          xref
          capf
          hierarchy
          eglot
          flycheck
          beautify
          navigation
          template
          compilation
          imenu
          which-func
          hideshow
          typedefs
          time-stamp
          ports))
  :config
  (verilog-ext-mode-setup))

;; Gptel (ollama)
(after! gptel
  (setq gptel-backend
        (gptel-make-ollama "Ollama"
          :host "localhost:11434"
          :stream t
          :models '(qwen3-coder:30b gemma3:27b))
        gptel-model 'gemma3:27b))

;; Org-roam-ui
(after! org-roam
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(map! :leader
      :desc "Open org-roam UI" "n r u" #'org-roam-ui-open)



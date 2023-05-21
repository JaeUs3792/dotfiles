;;; init-org.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)
(require 'init-custom)

(use-package org
  :ensure nil
  :custom-face (org-ellipsis ((t (:foreground unspecified))))
  :pretty-hydra
  ;; See `org-structure-template-alist'
  ((:title (pretty-hydra-title "Org Template" 'sucicon "nf-custom-orgmode" :face 'nerd-icons-green)
    :color blue :quit-key ("q" "C-g"))
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("e" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("p" (hot-expand "<s" "perl") "perl")
     ("w" (hot-expand "<s" "powershell") "powershell")
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c x" . org-capture)
         :map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; HACK: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil)))
         (org-mode . org-fold-hide-drawer-all)
         (org-mode . visual-line-mode))
  :config
  ;; For hydra
  (defun hot-expand (str &optional mod)
    "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  ;; To speed up startup, don't put to init section
  (setq org-modules nil                 ; Faster loading
        org-directory custom-org-directory
        org-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head
                    "%<%Y%m%d%H%M%S>-${slug}.org"
                    "#+title: ${title}\n")
           :unnarrowed t)
          ("p" "project" plain "* TODO %?"
           :target (file+head+olp
                    "%<%Y%m%d%H%M%S>-${slug}.org"
                    "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                    ("Tasks"))
           :unnarrowed t))

        org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence "⚑(T)" "🏴(S)" "❓(W)" "|" "✔(D)" "✘(C)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("KILL" . +org-todo-cancel))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))

        ;; Agenda styling
        org-agenda-files (list custom-org-agenda-files)
        org-agenda-block-separator ?─
        org-agenda-start-with-log-mode t
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────"

        ;; formula a_{i}
        org-use-sub-superscripts '{}
        ;; scale org-latex
        org-format-latex-options (plist-put org-format-latex-options :scale 2.0)

        org-image-actual-width nil

        ;; export options
        org-odt-preferred-output-format "docx" ;; opt -> docx
        org-export-with-sub-superscripts '{}   ;; ODT export to docx
        org-latex-compiler "xelatex"

        org-tags-column -80
        org-log-done 'time
        org-log-into-drawer "LOGBOOK"
        org-clock-into-drawer "CLOCKING"
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?⏷) "\t⤵" nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t)

  (use-package org-journal
    :defer t
    :config
    (setq org-journal-dir (expand-file-name "journal/" org-directory))
    (setq org-journal-file-type 'weekly))
  (use-package calfw)
  (use-package calfw-org
    :after calfw)

  ;; Add new template
  (add-to-list 'org-structure-template-alist '("n" . "note"))

  (use-package org-modern
    :hook ((org-mode . org-modern-mode)
           (org-agenda-finalize . org-modern-agenda))
           (org-modern-mode . (lambda ()
                                "Adapt `org-modern-mode'."
                                ;; Disable Prettify Symbols mode
                                (setq prettify-symbols-alist nil)
                                (prettify-symbols-mode -1))))
  (use-package org-superstar
    :if (and (display-graphic-p) (char-displayable-p ?◉))
    :hook (org-mode . org-superstar-mode)
    :init
    (setq org-superstar-headline-bullets-list '("◉""○""◈""◇""⁕")
          org-superstar-item-bullet-alist ;; dont work with org-modern
          '((?+ . ?➢)
            (?* . ?✰)
            (?- . ?➸))))

  (use-package org-fancy-priorities
    :diminish
    :hook (org-mode . org-fancy-priorities-mode)
    :init (setq org-fancy-priorities-list
                (if (and (display-graphic-p) (char-displayable-p ?🅐))
                    '("🅐" "🅑" "🅒" "🅓")
                  '("HIGH" "MEDIUM" "LOW" "OPTIONAL"))))

  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defconst load-language-alist
    '((emacs-lisp . t)
      (latex-as-png . t)
      (python     . t)
      (ruby       . t)
      (rust       . t)
      (C          . t))
    "Alist of org ob languages.")
  ;; ob-sh renamed to ob-shell since 26.1.
  (cl-pushnew '(shell . t) load-language-alist)

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-alist))

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-alist))
  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-alist)

  ;; Auto-toggle Org LaTeX fragments
  (use-package org-fragtog
    :diminish
    :hook (org-mode . org-fragtog-mode))
  ;; Make invisible parts of Org elements appear visible.
  (use-package org-appear
    :hook (org-mode)
    :config
    (setq org-appear-autoemphasis   t
          org-appear-autolinks      t
          org-appear-autoentities   t
          org-appear-autosubmarkers t))
  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
                ("s-<f7>" . org-tree-slide-mode))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :init (setq org-tree-slide-header nil
                org-tree-slide-slide-in-effect t
                org-tree-slide-heading-emphasis nil
                org-tree-slide-cursor-init t
                org-tree-slide-modeline-display 'outside
                org-tree-slide-skip-done nil
                org-tree-slide-skip-comments t
                org-tree-slide-skip-outline-level 3))
; attachment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package org-contrib ;; to use org-screenshot-take
    :defer t)
  (use-package org-attach-screenshot
    :defer t)
  (use-package org-download
    :defer t)
  (use-package ob-latex-as-png
    :ensure t)
  (defun my/org-remove-link-and-trash-linked-file ()
    "Remove `org-mode' link at point and trash linked file."
    (interactive)
    (let* ((link (org-element-context))
           (path (org-element-property :path link)))
      (move-file-to-trash path)
      (delete-region (org-element-property :begin link)
                     (org-element-property :end link))))
  (defun my/powershell (script)
    "executes the given script within a powershell and returns its return value"
    (call-process "powershell.exe" nil nil nil
                  "-Command" (concat "& {" script "}")))
  (defun my/as-windows-path (unix-path)
    "Takes a unix path and returns a matching WSL path"
    ;; substring removes the trailing \n
    (substring
     (shell-command-to-string
      (concat "wslpath -w " unix-path)) 0 -1))
  (defun my/org-paste-image-win2wsl ()
    "Paste an image into a time stamped unique-named file in the
          same directory as the org-buffer and insert a link to this file."
    (interactive)
    (let* ((target-file
            (concat
             (make-temp-name
              (concat org-directory
                      "/images/"
                      (f-filename buffer-file-name)
                      "_"
                      (format-time-string "%Y%m%d_%H%M%S_"))) ".png"))
           (wsl-path
            (concat (my/as-windows-path(file-name-directory target-file))
                    "/"
                    (file-name-nondirectory target-file)))
           (ps-script
            (concat "(Get-Clipboard -Format image).Save('" wsl-path "')")))

      (my/powershell ps-script)

      (if (file-exists-p target-file)
          (progn (insert (concat "[[" target-file "]]"))
                 (org-display-inline-images))
        (user-error
         "Error pasting the image, make sure you have an image in the clipboard!"))))
  (defun org-time-stamp-with-time()
        "Insert org mode timestamp at point with current date and time"
        (interactive)
        (org-insert-time-stamp (current-time) t))
; org-roam ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package org-roam
    :demand t ;; ensure org-roam is loaded by default
    :custom
    (org-roam-directory "~/org/roam")
    (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:*}" 'face 'org-tag)))
    ;;(org-roam-completion-everywhere t)
    :config
    (setq org-roam-capture-templates
          '(("d" "default" plain "%?"
             :target
             (file+head
              "%<%Y%m%d%H%M%S>-${slug}.org"
              "#+title: ${title}\n")
             :unnarrowed t)
            ("p" "project" plain "* TODO %?"
             :target
             (file+head+olp
              "%<%Y%m%d%H%M%S>-${slug}.org"
              "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
              ("Tasks"))
             :unnarrowed t)))
    (org-roam-db-autosync-mode))
  ;; Org roam ui
  (use-package org-roam-ui
    :defer t
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start nil))
  ;; functions
  (defun my/org-roam-rg-search ()
    "Search org-roam directory using consult-ripgrep. With live-preview."
    (interactive)
    (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
      (consult-ripgrep org-roam-directory)))
  (defun my/org-roam-filter-by-tag (tag-name)
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))
  (defun my/org-roam-list-notes-by-tag (tag-name)
    (mapcar #'org-roam-node-file
            (seq-filter
             (my/org-roam-filter-by-tag tag-name)
             (org-roam-node-list))))
  (defun my/org-roam-refresh-agenda-list ()
    (interactive)
    (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project"))
    (add-to-list 'org-agenda-files custom-org-agenda-files))
  (defun my/org-roam-project-finalize-hook ()
    "Adds the captured project file to `org-agenda-files' if the
      capture was not aborted."
    ;; Remove the hook since it was added temporarily
    (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Add project file to the agenda list if the capture was confirmed
    (unless org-note-abort
      (with-current-buffer (org-capture-get :buffer)
        (add-to-list 'org-agenda-files (buffer-file-name)))))
  (my/org-roam-refresh-agenda-list)
; Tools   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package ob-async
    :config
    (setq ob-async-no-async-languages-alist '("ipython")))
  (use-package org-pdftools
    :hook (org-mode . org-pdftools-setup-link))
; Exporter   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package ox-hugo
    :defer t
    :after ox)
  (setq org-latex-minted-options '(("breaklines" "true")
                                   ("tabsize" "4")
                                   ("autogobble")
                                   ("breakanywhere" "true")
                                   ("bgcolor" "gray!40")
                                   ("frame" "single")
                                   ("numbers" "left")))
  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("article"
                   "\\documentclass[11pt,a4paper]{article}
    \\usepackage[left=2.5cm,right=2.5cm,top=3cm,bottom=3cm,a4paper]{geometry}
    [DEFAULT-PACKAGES]
    \\usepackage{kotex}
    [PACKAGES]
    [EXTRA]
    \\linespread{1.1}
    \\hypersetup{pdfborder=0 0 0}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("org-plain-latex"
                   "\\documentclass[a4paper,11pt,titlepage]{memoir}
  \\usepackage[left=2.5cm,right=2.5cm,top=3cm,bottom=3cm,a4paper]{geometry}
  [DEFAULT-PACKAGES]
  \\usepackage{kotex}
  [PACKAGES]
  [EXTRA]
  \\linespread{1.1}
  \\hypersetup{pdfborder=0 0 0}"
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))
(provide 'init-org)
;;; init-org.el ends here

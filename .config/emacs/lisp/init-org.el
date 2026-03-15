;;; init-org.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)
(require 'init-custom)

(use-package org
  :ensure nil
  :custom-face (org-ellipsis ((t (:foreground unspecified))))
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
         (org-indent-mode . (lambda()
                              (when (fboundp 'diminish) (diminish 'org-indent-mode))
                              ;; HACK: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil)))
         (org-mode . org-fold-hide-drawer-all)
         (org-mode . visual-line-mode))
  :custom
  (org-preview-latex-default-process 'dvisvgm)
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

  (transient-define-prefix my/org-template-menu ()
    "Org Template"
    ["Basic"
     ("a" "ascii"    (lambda () (interactive) (hot-expand "<a")))
     ("c" "center"   (lambda () (interactive) (hot-expand "<c")))
     ("C" "comment"  (lambda () (interactive) (hot-expand "<C")))
     ("e" "example"  (lambda () (interactive) (hot-expand "<e")))
     ("E" "export"   (lambda () (interactive) (hot-expand "<E")))
     ("h" "html"     (lambda () (interactive) (hot-expand "<h")))
     ("l" "latex"    (lambda () (interactive) (hot-expand "<l")))
     ("n" "note"     (lambda () (interactive) (hot-expand "<n")))
     ("o" "quote"    (lambda () (interactive) (hot-expand "<q")))
     ("v" "verse"    (lambda () (interactive) (hot-expand "<v")))]
    ["Head"
     ("i" "index"   (lambda () (interactive) (hot-expand "<i")))
     ("A" "ASCII"   (lambda () (interactive) (hot-expand "<A")))
     ("I" "INCLUDE" (lambda () (interactive) (hot-expand "<I")))
     ("H" "HTML"    (lambda () (interactive) (hot-expand "<H")))
     ("L" "LaTeX"   (lambda () (interactive) (hot-expand "<L")))]
    ["Source"
     ("s" "src"        (lambda () (interactive) (hot-expand "<s")))
     ("m" "emacs-lisp" (lambda () (interactive) (hot-expand "<s" "emacs-lisp")))
     ("y" "python"     (lambda () (interactive) (hot-expand "<s" "python :results output")))
     ("p" "perl"       (lambda () (interactive) (hot-expand "<s" "perl")))
     ("w" "powershell" (lambda () (interactive) (hot-expand "<s" "powershell")))
     ("r" "ruby"       (lambda () (interactive) (hot-expand "<s" "ruby")))
     ("S" "sh"         (lambda () (interactive) (hot-expand "<s" "sh")))
     ("g" "golang"     (lambda () (interactive) (hot-expand "<s" "go :imports '(\"fmt\")")))
     ("u" "plantuml"   (lambda () (interactive) (hot-expand "<s" "plantuml :file CHANGE.png")))
     ("Y" "ipython"    (lambda () (interactive) (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0")))
     ("P" "Perl tangled" (lambda () (interactive)
                           (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
                           (hot-expand "<s" "perl")))])

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
        org-agenda-files (list custom-org-agenda-file)
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
        org-format-latex-options (plist-put org-format-latex-options :scale 1.3)

        org-image-actual-width nil

        ;; export options
        org-odt-preferred-output-format "docx" ;; opt -> docx
        org-export-with-sub-superscripts '{}   ;; ODT export to docx
        org-export-with-toc nil                ;; TOC 기본 비활성화
        org-export-headline-levels 5           ;; 기본 3 → 5 (paragraph/subparagraph까지)
        org-latex-compiler "pdflatex"

        org-tags-column -80
        org-log-done 'time
        org-log-into-drawer "LOGBOOK"
        org-clock-into-drawer "CLOCKING"
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?⏷) "\t⤵" nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t

        org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  ;; fontspec은 xelatex/lualatex 전용 → pdflatex 사용 시 제거
  (setq org-latex-default-packages-alist
        (seq-remove (lambda (x) (equal (cadr x) "fontspec"))
                    org-latex-default-packages-alist))

  (defconst load-language-alist
    '((emacs-lisp . t)
      (python     . t)
      (ruby       . t)
      (C          . t)
      (dot        . t)
      (shell      . t)
      (latex      . t))
    "Alist of org ob languages.")
  ;;(unless ON-WINDOWS
  ;;  (add-to-list 'load-language-alist '(latex-as-png . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cc" . "src c"))
  (add-to-list 'org-structure-template-alist '("cp" . "src c++"))
  (add-to-list 'org-structure-template-alist '("rs" . "src rust"))
  (add-to-list 'org-structure-template-alist '("rb" . "src ruby"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("oc" . "src octave"))
  (add-to-list 'org-structure-template-alist '("vl" . "src verilog"))
  (add-to-list 'org-structure-template-alist '("vh" . "src vhdl"))
  (add-to-list 'org-structure-template-alist '("n" . "note"))
  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-alist)

  ;; ob-sh renamed to ob-shell since 26.1.
  (cl-pushnew '(shell . t) load-language-alist)
; useful functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (org-insert-time-stamp (current-time) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Org mode improvement                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-journal
  :ensure t
  :defer t
  :config
  (setq org-journal-dir (expand-file-name "journal/" org-directory))
  (setq org-journal-file-type 'weekly))
(use-package org-noter
  :ensure t
  :defer t
  :config
  (setq org-noter-always-create-frame nil
        org-noter-hide-other nil
        org-noter-notes-search-path '("~/org/notes")
        org-noter-separate-notes-from-heading t
        org-noter-highlight-selected-text t)
  (org-noter-enable-org-roam-integration))

(use-package org-modern
  :ensure t
  :defer t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  (setq org-modern-table nil)
  (setq org-modern-todo-faces
        '(("TODO" :background "#f1fa8c" :foreground "#282a36" :weight bold)  ; 옐로우 — 할 일 대기중
          ("PROJ" :background "#bd93f9" :foreground "#282a36" :weight bold)  ; 퍼플  — 프로젝트
          ("STRT" :background "#50fa7b" :foreground "#282a36" :weight bold)  ; 그린  — 진행중
          ("WAIT" :background "#8be9fd" :foreground "#282a36" :weight bold)  ; 파랑  — 대기/보류
          ("HOLD" :background "#ffb86c" :foreground "#282a36" :weight bold)  ; 오렌지 — 일시정지
          ("IDEA" :background "#ff79c6" :foreground "#282a36" :weight bold)  ; 핑크  — 아이디어
          ("DONE" :background "#44475a" :foreground "#6272a4" :weight bold)  ; 흐린회색 — 완료
          ("KILL" :background "#ff5555" :foreground "#282a36" :weight bold)))) ; 다크그레이



;; Auto-toggle Org LaTeX fragments
(use-package org-fragtog
  :ensure t
  :defer t
  :diminish
  :hook (org-mode . org-fragtog-mode))
;; Make invisible parts of Org elements appear visible.
(use-package org-appear
  :ensure t
  :defer t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis   t
        org-appear-autolinks      t
        org-appear-autoentities   t
        org-appear-autosubmarkers t))
;; support drawio
(use-package org-drawio
  :ensure t
  :defer t
  :commands (org-drawio-add
             org-drawio-open)
  :custom ((org-drawio-input-dir "./draws")
           (org-drawio-output-dir "./images")
           (org-drawio-output-page "0")
           ;; set to t, if you want to crop the image.
           (org-drawio-crop t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Org mode attachment                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-contrib ;; to use org-screenshot-take
  :ensure t
  :defer t)
(use-package org-attach-screenshot
  :ensure t
  :defer t)
(use-package org-download
  :after org
  :ensure t
  :demand t
  :init
  (setq org-download-image-dir "./images")
  (setq org-download-heading-lvl nil)
  :config
  (org-download-enable))
(use-package ob-latex-as-png
  :ensure t
  :defer t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Org roam                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-roam
  :ensure t
  :defer t
  :demand t ;; ensure org-roam is loaded by default
  :custom
  (org-roam-directory custom-org-roam-directory)
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
    (add-to-list 'org-agenda-files custom-org-agenda-file))
  (defun my/org-roam-project-finalize-hook ()
    "Adds the captured project file to `org-agenda-files' if the
      capture was not aborted."
    ;; Remove the hook since it was added temporarily
    (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Add project file to the agenda list if the capture was confirmed
    (unless org-note-abort
      (with-current-buffer (org-capture-get :buffer)
        (add-to-list 'org-agenda-files (buffer-file-name)))))

  ; database sync
  (org-roam-db-autosync-mode))
;; Org roam ui
(use-package org-roam-ui
  :ensure t
  :defer t
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Org mode exporter                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ox-hugo
  :ensure t
  :defer t
  :after ox)

;; engrave-faces: Emacs font-lock 기반 코드 하이라이팅 (minted 대체)
;; minted 대비 8배 빠름, Python/pygments 의존성 없음, -shell-escape 불필요
(use-package engrave-faces
  :ensure t
  :after ox-latex)

(with-eval-after-load 'ox-latex
  ;; example 블록도 engraved 스타일로 통일
  (advice-add 'org-latex-example-block :around
              (lambda (orig-fn example-block contents info)
                (let ((output-block (funcall orig-fn example-block contents info)))
                  (if (eq 'engraved (plist-get info :latex-listings))
                      (format "\\begin{Code}[alt]\n%s\n\\end{Code}" output-block)
                    output-block))))

  ;; KOMA-script 클래스 설정
  ;; scrartcl/scrbook: 표준 article/book 대체, 여백/헤더/캡션 등 내장 커스터마이징 지원
  (let* ((article-sections '(("\\section{%s}" . "\\section*{%s}")
                             ("\\subsection{%s}" . "\\subsection*{%s}")
                             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                             ("\\paragraph{%s}" . "\\paragraph*{%s}")
                             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
         (book-sections (append '(("\\chapter{%s}" . "\\chapter*{%s}"))
                                article-sections))
         ;; 섹션 번호를 마진에 내어쓰기 (KOMA 전용)
         (hanging-secnum
          "
\\renewcommand\\sectionformat{\\llap{\\thesection\\autodot\\enskip}}
\\renewcommand\\subsectionformat{\\llap{\\thesubsection\\autodot\\enskip}}
\\renewcommand\\subsubsectionformat{\\llap{\\thesubsubsection\\autodot\\enskip}}")
         ;; 큼직한 챕터 헤딩 (scrbook 전용)
         (big-chapter
          "
\\RedeclareSectionCommand[afterindent=false, beforeskip=0pt, afterskip=0pt, innerskip=0pt]{chapter}
\\setkomafont{chapter}{\\normalfont\\Huge}
\\renewcommand*{\\chapterheadstartvskip}{\\vspace*{0\\baselineskip}}
\\renewcommand*{\\chapterheadendvskip}{\\vspace*{0\\baselineskip}}
\\renewcommand*{\\chapterformat}{%
  \\fontsize{60}{30}\\selectfont\\rlap{\\hspace{6pt}\\thechapter}}
\\renewcommand*\\chapterlinesformat[3]{%
  \\parbox[b]{\\dimexpr\\textwidth-0.5em\\relax}{%
    \\raggedleft{{\\large\\scshape\\bfseries\\chapapp}\\vspace{-0.5ex}\\par\\Huge#3}}%
    \\hfill\\makebox[0pt][l]{#2}}"))
    (setcdr (assoc "article" org-latex-classes)
            `(,(concat "\\documentclass{scrartcl}" hanging-secnum)
              ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("report" ,(concat "\\documentclass{scrartcl}" hanging-secnum)
                   ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("book" ,(concat "\\documentclass[twoside=false]{scrbook}"
                                   big-chapter hanging-secnum)
                   ,@book-sections))
    (add-to-list 'org-latex-classes
                 `("blank" "[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                   ,@article-sections)))

  ;; KOMA와 capt-of 충돌 방지: KOMA가 \captionof를 이미 제공함
  (setq org-latex-default-packages-alist
        (seq-remove (lambda (x) (equal (cadr x) "capt-of"))
                    org-latex-default-packages-alist))

  ;; 컬러 hyperref 설정
  ;; \providecolor 대신 인라인 xcolor HTML 문법 사용 (xcolor 로드 순서 무관)
  (setq org-latex-hyperref-template
        "\\hypersetup{
  pdfauthor={%a},
  pdftitle={%t},
  pdfkeywords={%k},
  pdfsubject={%d},
  pdfcreator={%c},
  pdflang={%L},
  breaklinks=true,
  colorlinks=true,
  linkcolor={[HTML]{882255}},
  urlcolor={[HTML]{0077bb}},
  citecolor={[HTML]{999933}}}
\\urlstyle{same}\n"))

(setq org-latex-listings 'engraved
      ;org-latex-engraved-theme 'modus-operandi   ; 밝은 배경 테마 (PDF 출력에 적합)
      org-latex-packages-alist '(("" "kotex"))
      org-latex-tables-booktabs t
      org-latex-pdf-process
      '("LC_ALL=en_US.UTF-8 latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))
(add-to-list 'org-latex-packages-alist '("" "booktabs"))
(add-to-list 'org-latex-packages-alist '("" "tabularx"))

; (use-package cdlatex
;   :ensure t
;   :hook (org-mode . org-cdlatex-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Org babel related                             ;
;; org-babel-process-params 가 :file 을 expand-file-name 으로 절대경로 변환함
;; 결과 링크를 버퍼 디렉토리 기준 상대경로로 되돌림
(defun my/org-babel-result-use-relative-path (result)
  (when (and (stringp result) buffer-file-name)
    (let ((buf-dir (file-name-directory (buffer-file-name))))
      (setq result
            (replace-regexp-in-string
             "file:\\(/[^][\n]+\\)"
             (lambda (m)
               ;; m = "file:/abs/path" → substring 5 = "/abs/path"
               (concat "file:" (file-relative-name (substring m 5) buf-dir)))
             result))))
  result)
(advice-add 'org-babel-result-to-file :filter-return
            #'my/org-babel-result-use-relative-path)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package gnuplot
  :ensure t
  :defer t)

(use-package ob-async
  :ensure t
  :config
  (setq ob-async-no-async-languages-alist '("ipython")))

;; ob-wavedrom: wavedrom-cli 사용 (npm install -g wavedrom-cli)
(defvar org-babel-default-header-args:wavedrom
  '((:results . "raw") (:exports . "results"))
  "Default header args for wavedrom babel blocks.")

(defun org-babel-execute:wavedrom (body params)
  "Execute a wavedrom block using wavedrom-cli."
  (let* ((file (cdr (assq :file params)))
         (in-file (org-babel-temp-file "wavedrom-" ".json")))
    (with-temp-file in-file (insert body))
    (org-babel-eval
     (format "wavedrom-cli -i %s -s %s"
             (shell-quote-argument in-file)
             (shell-quote-argument file))
     "")
    (format "[[file:%s]]" file)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Tools                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless ON-WINDOWS
  (use-package org-pdftools
    :ensure t
    :defer t
    :hook (org-mode . org-pdftools-setup-link)))
(provide 'init-org)
;;; init-org.el ends here

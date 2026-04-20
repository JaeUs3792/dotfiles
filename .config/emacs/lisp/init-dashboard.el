;;; init-dashboard.el -*- lexical-binding: t -*-
(require 'init-funcs)

;; Session helpers
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
    (condition-case-unless-debug err
        (persp-load-state-from-file fname)
      (error "Error: Unable to restore session -- %s" err))
    (message "Restoring session...done")))

(defun my/hsl-to-hex (h s l)
  "Convert H S L (0.0-1.0) to hex color string."
  (let* ((q (if (< l 0.5) (* l (+ 1.0 s)) (+ l s (- (* l s)))))
         (p (- (* 2.0 l) q))
         (channel (lambda (t-val)
                    (setq t-val (cond ((< t-val 0.0) (+ t-val 1.0))
                                      ((> t-val 1.0) (- t-val 1.0))
                                      (t t-val)))
                    (round (* 255.0
                              (cond ((< t-val (/ 1.0 6)) (+ p (* (- q p) 6.0 t-val)))
                                    ((< t-val 0.5)       q)
                                    ((< t-val (/ 2.0 3)) (+ p (* (- q p) (- (/ 2.0 3) t-val) 6.0)))
                                    (t p)))))))
    (format "#%02x%02x%02x"
            (funcall channel (+ h (/ 1.0 3)))
            (funcall channel h)
            (funcall channel (- h (/ 1.0 3))))))

(defun my/enlight-rainbow-banner ()
  (let* ((lines '("▓█████  ███▄ ▄███▓ ▄▄▄       ▄████▄    ██████  "
                  "▓█   ▀ ▓██▒▀█▀ ██▒▒████▄    ▒██▀ ▀█  ▒██    ▒ "
                  "▒███   ▓██    ▓██░▒██  ▀█▄  ▒▓█    ▄ ░ ▓██▄   "
                  "▒▓█  ▄ ▒██    ▒██ ░██▄▄▄▄██ ▒▓▓▄ ▄██▒  ▒   ██▒"
                  "░▒████▒▒██▒   ░██▒ ▓█   ▓██▒▒ ▓███▀ ░▒██████▒▒"))
         (nrows (length lines))
         (ncols (apply #'max (mapcar #'length lines)))
         (diag-max (float (+ (1- nrows) (1- ncols)))))
    (mapconcat
     (lambda (row)
       (let* ((line (nth row lines))
              (len (length line)))
         (concat
          (propertize " " 'display '(space :align-to (- center 25)))
          (mapconcat
           (lambda (col)
             (let* ((diag (/ (+ col row) diag-max))
                    (hue (* diag 0.80))
                    (color (my/hsl-to-hex hue 1.0 0.65)))
               (propertize (string (aref line col))
                           'face `(:foreground ,color :weight bold :height 1.5))))
           (number-sequence 0 (1- len))
           ""))))
     (number-sequence 0 (1- nrows))
     "\n")))

(defun my/enlight-image-banner ()
  "Display logos/xemacs_color.svg as the dashboard banner.
Falls back to the rainbow text banner in terminal mode."
  (if (display-graphic-p)
      (let* ((image-path (expand-file-name "logos/xemacs_color.svg" user-emacs-directory))
             (img-width 300)
             (img (create-image image-path 'svg nil :width img-width))
             (half-chars (/ img-width (* 2.0 (frame-char-width))))
             (spacer (propertize " " 'display `(space :align-to (- center ,half-chars)))))
        (concat spacer (propertize " " 'display img)))
    (my/enlight-rainbow-banner)))

(use-package grid
  :ensure (:host github :repo "ichernyshovvv/grid.el"))

(defun my/enlight-recent-files ()
  (require 'recentf)
  (concat
   (propertize "Recent Files\n" 'face '(:weight bold))
   (mapconcat
    (lambda (f)
      (propertize (concat "  " (abbreviate-file-name f)) 'face 'font-lock-doc-face))
    (seq-take recentf-list 10)
    "\n")))

(defun my/enlight-git-log-for-root (git-root n)
  (let* ((project-name (file-name-nondirectory (directory-file-name git-root)))
         (log (shell-command-to-string
               (format "git -C %s log --oneline -%d 2>/dev/null"
                       (shell-quote-argument (expand-file-name git-root)) n))))
    (when (not (string-empty-p (string-trim log)))
      (concat
       (propertize (concat project-name " - Recent Commits\n") 'face '(:weight bold))
       (mapconcat
        (lambda (line)
          (if (string-match "^\\([a-f0-9]+\\) \\(.*\\)" line)
              (concat
               (propertize (concat "  " (match-string 1 line)) 'face '(:foreground "#6272a4"))
               (propertize (concat " " (match-string 2 line)) 'face 'font-lock-doc-face))
            line))
        (split-string (string-trim log) "\n" t)
        "\n")))))

(defun my/enlight-git-log ()
  (require 'recentf)
  (let* ((roots (seq-take
                 (seq-uniq
                  (seq-filter #'identity
                              (mapcar (lambda (f) (locate-dominating-file f ".git"))
                                      recentf-list))
                  #'string=)
                 3))
         (sections (seq-filter #'identity
                               (mapcar (lambda (r) (my/enlight-git-log-for-root r 5))
                                       roots))))
    (string-join sections "\n\n")))



(defvar my/enlight-quotes
  '("Any sufficiently advanced technology is indistinguishable from magic. — Arthur C. Clarke"
    "Talk is cheap. Show me the code. — Linus Torvalds"
    "Emacs is a great operating system, lacking only a decent editor. — Unknown"
    "The best program is the one that works. — Unknown"
    "First, solve the problem. Then, write the code. — John Johnson"
    "Simplicity is the soul of efficiency. — Austin Freeman"
    "Make it work, make it right, make it fast. — Kent Beck"
    "The most disastrous thing that you can ever learn is your first programming language. — Alan Kay"
    "Debugging is twice as hard as writing the code in the first place. — Brian Kernighan"
    "Walking on water and developing software from a specification are easy if both are frozen. — Edward V. Berard"
    "It works on my machine. — Every developer, ever"
    "There are only two hard things in CS: cache invalidation, naming things, and off-by-one errors."
    "In order to understand recursion, one must first understand recursion. — Unknown"
    "Real programmers count from 0. — Unknown"
    "Lisp is worth learning for the profound enlightenment experience you will have when you finally get it. — Eric Raymond"
    ;; Linus Torvalds
    "Intelligence is the ability to avoid doing work, yet getting the work done. — Linus Torvalds"
    "Most good programmers do programming not because they expect to get paid, but because it is fun. — Linus Torvalds"
    "Software is like sex: it's better when it's free. — Linus Torvalds"
    "Bad programmers worry about the code. Good programmers worry about data structures and their relationships. — Linus Torvalds"
    "If you need more than 3 levels of indentation, you're screwed anyway, and should fix your program. — Linus Torvalds"
    "I'm a bastard. I have absolutely no clue why people can tolerate me. — Linus Torvalds"
    "Portability is for people who cannot write new programs. — Linus Torvalds"
    "Only wimps use tape backup: real men just upload their important stuff on ftp. — Linus Torvalds"
    "I claim that Subversion has been the single largest waster of my time. — Linus Torvalds"
    "An individual developer like me cares about writing the kernel. I don't care about the average user. — Linus Torvalds"
    ;; Programmers
    "Programs must be written for people to read, and only incidentally for machines to execute. — Harold Abelson"
    "The function of good software is to make the complex appear simple. — Grady Booch"
    "Measuring programming progress by lines of code is like measuring aircraft building progress by weight. — Bill Gates"
    "Always code as if the guy who ends up maintaining your code will be a violent psychopath who knows where you live. — John Woods"
    "The best error message is the one that never shows up. — Thomas Fuchs"
    "Before software can be reusable it first has to be usable. — Ralph Johnson"
    "Without requirements or design, programming is the art of adding bugs to an empty text file. — Louis Srygley"
    "The most important property of a program is whether it accomplishes the intention of its user. — C.A.R. Hoare"
    "One bad programmer can easily create two new jobs a year. — David Parnas"
    "Deleted code is debugged code. — Jeff Sickel"
    "The computer was born to solve problems that did not exist before. — Bill Gates"
    "It's not a bug — it's an undocumented feature. — Unknown"
    "Documentation is like sex: when it's good, it's very good; when it's bad, it's better than nothing. — Dick Brandon"
    "A language that doesn't affect the way you think about programming is not worth knowing. — Alan Perlis"
    "There are two ways to write error-free programs; only the third one works. — Alan Perlis"
    ;; More Linus
    "In real open source, you have the right to control your own destiny. — Linus Torvalds"
    "If you think your users are idiots, only idiots will use it. — Linus Torvalds"
    "Given enough eyeballs, all bugs are shallow. — Linus Torvalds"
    "I want my office to be quiet. The loudest thing in the room should be the clicky-clack of keyboards. — Linus Torvalds"
    "Those that can, do. Those that can't, complain. — Linus Torvalds"
    "I do not have a five-year plan. I do not even have a five-week plan. — Linus Torvalds"
    "Avoid using the word 'paradigm'. — Linus Torvalds"
    "Nobody actually creates perfect code the first time around, except me. But there's only one of me. — Linus Torvalds"
    "The Linux philosophy is 'Laugh in the face of danger'. Oops. Wrong one. 'Do it yourself'. — Linus Torvalds"
    "If Microsoft ever does applications for Linux it means I've won. — Linus Torvalds"
    ;; Classic CS
    "There are only two hard things in CS: cache invalidation, naming things, and off-by-one errors. — Phil Karlton"
    "UNIX is basically a simple operating system, but you have to be a genius to understand the simplicity. — Dennis Ritchie"
    "C is quirky, flawed, and an enormous success. — Dennis Ritchie"
    "The C programming language — a language which combines the flexibility of assembly language with the power of assembly language. — Unknown"
    "Premature optimization is the root of all evil. — Donald Knuth"
    "An algorithm must be seen to be believed. — Donald Knuth"
    "If you find that you're spending almost all your time on theory, start turning some attention to practical things. — Donald Knuth"
    "The best way to predict the future is to invent it. — Alan Kay"
    "Simple things should be simple, complex things should be possible. — Alan Kay"
    "Object-oriented programming is an exceptionally bad idea which could only have originated in California. — Edsger W. Dijkstra"
    "The question of whether computers can think is like the question of whether submarines can swim. — Edsger W. Dijkstra"
    "Testing shows the presence, not the absence of bugs. — Edsger W. Dijkstra"
    "Simplicity is a great virtue but it requires hard work to achieve it. — Edsger W. Dijkstra"
    "Computer science is no more about computers than astronomy is about telescopes. — Edsger W. Dijkstra"
    ;; Wisdom
    "Weeks of coding can save you hours of planning. — Unknown"
    "When in doubt, use brute force. — Ken Thompson"
    "One of my most productive days was throwing away 1000 lines of code. — Ken Thompson"
    "You can't trust code that you did not totally create yourself. — Ken Thompson"
    "Everyone knows that debugging is twice as hard as writing a program in the first place. — Brian Kernighan"
    "Controlling complexity is the essence of computer programming. — Brian Kernighan"
    "The most effective debugging tool is still careful thought, coupled with judiciously placed print statements. — Brian Kernighan"
    "Any fool can write code that a computer can understand. Good programmers write code that humans can understand. — Martin Fowler"
    "When you feel the need to write a comment, first try to refactor the code so that any comment becomes superfluous. — Martin Fowler"
    "The first step of any project is to grossly underestimate its complexity and difficulty. — Nicoll Hunt"
    "If debugging is the process of removing software bugs, then programming must be the process of putting them in. — Edsger W. Dijkstra"
    "A good programmer is someone who always looks both ways before crossing a one-way street. — Doug Linder"
    "Code is like humor. When you have to explain it, it's bad. — Cory House"
    "Fix the cause, not the symptom. — Steve Maguire"
    "Optimism is an occupational hazard of programming; feedback is the treatment. — Kent Beck"
    "When to use iterative development? You should use iterative development only on projects that you want to succeed. — Martin Fowler"
    "The trouble with programmers is that you can never tell what a programmer is doing until it's too late. — Seymour Cray"
    "The best performance improvement is the transition from the nonworking state to the working state. — John Ousterhout"
    "Correctness is clearly the prime quality. If a system does not do what it is supposed to do, then everything else about it matters little. — Bertrand Meyer"
    "Good software, like wine, takes time. — Joel Spolsky"
    "Don't document bad code — rewrite it. — Brian Kernighan"
    "There's nothing more permanent than a temporary solution. — Unknown"
    "It always takes longer than you expect, even when you take into account Hofstadter's Law. — Douglas Hofstadter"
    "Hofstadter's Law: It always takes longer than you expect, even when you take into account Hofstadter's Law. — Douglas Hofstadter"
    "A ship in port is safe, but that's not what ships are for. — Grace Hopper"
    "The most dangerous phrase in the language is: we've always done it this way. — Grace Hopper"
    "In computing, turning the obvious approach into the working approach is sometimes 90% of the work. — Unknown"
    "Programs don't just happen. They have to be designed. — Grady Booch"
    "The computer is incredibly fast, accurate, and stupid. Man is unbelievably slow, inaccurate, and brilliant. — Leo Cherne"
    "To iterate is human, to recurse divine. — L. Peter Deutsch"
    "Walking on water and developing software from a specification are easy if both are frozen. — Edward V. Berard"
    "Real programmers don't comment their code. If it was hard to write, it should be hard to understand. — Unknown"))

(defun my/enlight-quote ()
  (let* ((quote (nth (random (length my/enlight-quotes)) my/enlight-quotes))
         (half (/ (string-width quote) 2)))
    (concat
     (propertize " " 'display `(space :align-to (- center ,half)))
     (propertize quote 'face '(:foreground "#6272a4" :slant italic)))))

(defun my/enlight-center-block (str)
  "Add per-line centering display property to each line of STR."
  (mapconcat
   (lambda (line)
     (let ((half (/ (string-width line) 2)))
       (concat
        (propertize " " 'display `(space :align-to (- center ,half)))
        line)))
   (split-string str "\n" t)
   "\n"))

(defun my/enlight-make-content ()
  (require 'grid)
  (let* ((grid (grid-make-row
                (list
                 (enlight-menu
                  '(("Files"
                     ("Recent Files"     consult-recent-file          "r")
                     ("Find File"        find-file                    "f")
                     ("Open Dired"       dired-jump                   "d"))
                    ("Projects"
                     ("Switch Project"   project-switch-project       "p")
                     ("Find in Project"  project-find-file            "F"))
                    ("Session"
                     ("Restore Previous" restore-previous-session     "R")
                     ("Load Session..."  restore-session              "L"))
                    ("Emacs"
                     ("Update"           update-dotfiles-and-packages "U")
                     ("Settings"         (find-file custom-file)      "s")
                     ("Quit"             save-buffers-kill-terminal   "q"))))
                 "    "
                 (my/enlight-recent-files))))
         (git (my/enlight-git-log)))
    (concat
     (my/enlight-image-banner)
     "\n\n\n"
     (my/enlight-quote)
     "\n\n\n"
     (my/enlight-center-block grid)
     "\n\n"
     git)))

(use-package enlight
  :ensure t
  :bind ("<f2>" . enlight-open)
  :init
  (advice-add 'enlight :before (lambda (&rest _)
                                 (enlight--update 'enlight-content (my/enlight-make-content))))
  :config
  (evil-define-key 'normal enlight-mode-map
    "r" #'consult-recent-file
    "f" #'find-file
    "d" #'dired-jump
    "p" #'project-switch-project
    "F" #'project-find-file
    "R" #'restore-previous-session
    "L" #'restore-session
    "U" #'update-dotfiles-and-packages
    "s" (lambda () (interactive) (find-file custom-file)))
  :custom
  (enlight-center-vertically t)
  (enlight-center-horizontally t)
  (initial-buffer-choice #'enlight))

(provide 'init-dashboard)
;;; init-dashboard.el ends here

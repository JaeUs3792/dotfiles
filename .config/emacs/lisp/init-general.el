;;; init-general.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; this annoying binding.
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "S-SPC"))    ;; input method / use only S-\
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

                                        ; TODO move to hideshow
(global-set-key (kbd "C-<tab>") 'hs-toggle-hiding)

;; When you begin a keybind, whichkey will show you all keybinds you can follow the first one with in order to form a full keywords.
(use-package which-key
  :straight t
  :ensure t
  :defer t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package general
  :straight t
  :ensure t
  :defer t
  :init
  (general-auto-unbind-keys)
  :config
  (general-evil-setup t)
  (general-create-definer
    ju/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))
(ju/leader-key-def
  ;; workspace
  "TAB" '(persp-key-map :which-key "persp-mode")
  ;; file
  "." 'find-file
  ;; Buffer
  "b" '(:ignore t :which-key "buffer handling")
  "b i" '(ibuffer :which-key "IBuffer")
  "b r" '(revert-buffer :which-key "Revert Buffer")
  "b k" '(kill-current-buffer :which-key "Kill current buffer")
  "b n" '(next-buffer :which-key "Next buffer")
  "b p" '(previous-buffer :which-key "Previous buffer")
  "b B" '(ibuffer-list-buffers :which-key "IBuffer List Buffers")
  "b K" '(kill-buffer :which-key "IBuffer Kill Buffers")
  "b s" '(consult-buffer :which-key "switch buffer")
  ;; Citre
  "c" '(:ignore t :which-key "citre")
  "c j" 'citre-jump+
  "c J" 'dumb-jump-go-other-window
  "c k" 'citre-jump-back+
  "c p" 'citre-peek
  "c a" 'citre-ace-peek
  "c u" 'citre-update-tags-file
  ;; Eshell
  "e" '(:ignore t :which-key "eshell")
  "e h" '(counsel-esh-history :which-key "Kill history")
  "e s" '(eshell :which-key "run eshell")
  "e e" '(elfeed :which-key "elfeed")
  ;; Counsel
  "f" '(:ignore t :which-key "file op.")
  "f r" '(consult-recent-file :which-key "Recent files")
  "f i" '((lambda () (interactive) (find-file "~/.config/emacs/init.el")) :which-key "open emacs init file")
  "f l" '((lambda () (interactive) (find-file "~/.config/emacs/lisp")) :which-key "open emacs lisp dir")
  ;; Magit
  "g" '(:ignore t :which-key "magit")
  "g g" '(magit :which-key "magit")
  ;; Hydra
  "h" '(:ignore t :which-key "hydra / hideshow")
  "h h" '(:ignore t :which-key "hydra")
  "h t" '(hs-toggle-hiding :which-key "hideshow toggle hiding")
  "h a" '(hs-hide-all :which-key "hideshow hide all")
  "h s" '(hs-show-all :which-key "hideshow show all")
  ;; "h t" '(hydra-text-scale/body :which-key "scale text")
  ;; "h w" '(hydra-writeroom-scale/body :which-key "scale whiteroom")
  ;; "h a" '(hydra-modify-alpha/body :which-key "modify alpha background")
  "h h j" '(dumb-jump-hydra/body :which-key "dumb jump hydra")
  "h h o" '(org-hydra/body :which-key "org hydra")
  "h h h" '(hideshow-hydra/body :which-key "hideshow hydra")
  ;; Insert something
  "i" '(:ignore t :which-key "insert something.")
  "i s" '(yas-insert-snippet :which-key "snippet")
  "i e" '(emojify-insert-emoji :which-key "emoji")
  ;; Managements
  "m" '(:ignore t :which "Managements")
  "m d" '(docker :which-key "Docker")
  ;; Org Journal / Org Roam
  "n" '(:ignore t :which-key "Journal / Roam")
  "n j" '(:ignore t :which-key "Org Journal")
  "n j j" '(org-journal-new-entry :which-key "new Entry")
  "n j J" '(org-journal-new-scheduled-entry :which-key "New Scheduled entry")
  "n j s" '(org-journal-search :which-key "Journal Search")
  "n r" '(:ignore t :which-key "Org Roam")
  "n r c" '(org-roam-capture :which-key "capture")
  "n r u" '(org-roam-ui-open :which-key "UI")
  "n r l" '(org-roam-buffer-toggle :which-key "buffer toggle")
  "n r f" '(org-roam-node-find :which-key "node find")
  "n r i" '(org-roam-node-insert :which-key "node insert")
  "n r I" '(org-id-get-create :which-key "create id")
  "n r g" '(my/org-roam-rg-search :which-key "roam ripgrep")
  "n r t" '(:ignore t :which-key "Org Roam Tags")
  "n r t t" '(org-roam-tag-add :which-key "new tag")
  "n r t d" '(org-roam-tag-remove :which-key "delete tag")
  ;; Org mode
  "o" '(:ignore t :which-key "Org mode")
  "o a" '(:ignore t :which-key "Org Agenda")
  "o a a" '(org-agenda :which-key "open org agenda")
  "o a r" '(my/org-roam-refresh-agenda-list :which-key "Org agenda refresh list")
  "o a c" '((lambda () (interactive) (find-file (expand-file-name "agenda/agenda.org" org-directory))) :which-key "org agenda file")
  "o c" '(cfw:open-org-calendar :which-key "org calendar")
  "o t" '(org-insert-structure-template :which-key "org insert structure template")
  ;; Project-el
  "p" '(:ignore t :which-key "project")
  "p ." '(project-switch-project :which-key "switch project")
  "p p" '(project-switch-project :which-key "switch project")
  "p c" '(project-compile :which-key "compile")
  "p f" '(project-find-file :which-key "find-file")
  "p k" '(project-kill-buffers :which-key "kill buffers")
  "p s" '(project-shell :which-key "shell")
  "p e" '(project-eshell :which-key "eshell")
  "p d" '(project-dired :which-key "dired")
  ;;"p g" '(project-find-regexp :which-key "find-regexp")
  "p g" '(consult-ripgrep :which-key "ripgrep")
  ;; register
  "r" '(:ignore t :which-key "register")
  "r y" '(consult-register-save :which-key "yank to register")
  "r p" '(consult-register-load :which-key "paste to buffer")
  ;; Extra
  "t" '(:ignore t :which-key "extra")
  "t a" '(toggle-transparency :which-key "Toggle Transparency")
  "t t" '(toggle-truncate-lines :which-key "Toggle truncate lines")
  "t g" '(gts-do-translate :which-key "Goggle Translate")
  ;; Avy
  "v" '(:ignore t :which-key "Avy")
  "vc" '(avy-goto-char :which-key "Avy Goto Char")
  "ve" '(avy-goto-char-2 :which-key "Avy Goto Char - 2")
  "vq" '(avy-goto-word-0 :which-key "Avy Goto Word")
  "vw" '(avy-goto-word-1 :which-key "Avy Goto Word - 1")
  "vl" '(avy-goto-line :which-key "Avy Goto Line"))

(provide 'init-general)
;;; init-general.el ends here.

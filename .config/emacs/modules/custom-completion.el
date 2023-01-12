(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("M-h" . vertico-directory-up))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))
(use-package vertico-posframe
  :disabled
  :after vertico
  :ensure t
  :init
  (setq vertico-posframe-parameters
        `((left-fringe . 8)
          (right-fringe . 8) (alpha . 100)))
  (vertico-posframe-mode 1))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;;(defun my/get-project-root ()   ;; TODO after projectile
;;  (when (fboundp 'projectile-project-root)
;;    (projectile-project-root)))
(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  ;;(consult-project-root-function #'get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (style basic partial-completion)))))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu)
(use-package popon
  :defer t
  :straight (popon :build t
                   :type git
                   :host nil
                   :repo "https://codeberg.org/akib/emacs-popon.git"))
(use-package confu-terminal
:defer t
:straight (confu-terminal :build t
                     :type git
                     :host nil
                     :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
(add-to-list 'load-path
             (expand-file-name "straight/build/corfu/extensions"
                               user-emacs-directory))
(require 'corfu-popupinfo)
(require 'corfu)
(unless (display-graphic-p)
  (require 'corfu-terminal)
  (corfu-terminal-mode +1))

(customize-set-variable 'corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
(customize-set-variable 'corfu-auto t)                 ;; Enable auto completion
(customize-set-variable 'corfu-auto-prefix 2)
(customize-set-variable 'corfu-auto-delay 0.0)
;; (customize-set-variable 'corfu-separator ?\s)          ;; Orderless field separator
;; (customize-set-variable 'corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;; (customize-set-variable 'corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;; (customize-set-variable 'corfu-preview-current nil)    ;; Disable current candidate preview
;; (customize-set-variable 'corfu-preselect 'prompt)      ;; Preselect the prompt
;; (customize-set-variable 'corfu-on-exact-match nil)     ;; Configure handling of exact matches
;; (customize-set-variable 'corfu-scroll-margin 5)        ;; Use scroll margin
(customize-set-variable 'corfu-echo-documentation 0.25)
(global-corfu-mode 1)
(corfu-popupinfo-mode 1)
(eldoc-add-command #'corfu-insert)
(define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down)
(define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up)
(define-key corfu-map (kbd "M-d") #'corfu-popupinfo-toggle)

(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  )
(add-hook 'eshell-mode-hook
          (lambda () (setq-local corfu-quit-at-boundary t
                                 corfu-quit-no-match t
                                 corfu-auto nil)
            (corfu-mode)))

(provide 'custom-completion)
;;; custom-completion.el ends here

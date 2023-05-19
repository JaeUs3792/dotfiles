;;; init-vertico.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :hook (after-init . vertico-mode)
  :custom
  (vertico-cycle t))

;; simple, but effective sorting and filtering for emacs.
(use-package vertico-prescient
  :defer t
  :config
  (vertico-prescient-mode))

;; annotations placed at the margin of the minibuffer
(use-package marginalia
  :after vertico
  :defer t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))


(use-package consult
  :demand t
  :bind (:map minibuffer-local-map
              ("C-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (global-set-key (kbd "C-s") 'consult-line)
  (global-set-key (kbd "C-M-j") 'consult-buffer))

(use-package orderless
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (style basic partial-completion)))))

;; Quick action in minibuffer
(use-package embark
  :defer t
  :bind (("C-." . embark-act))
  :config
  (with-eval-after-load 'embark-consult
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))
  (global-set-key [remap describe-bindings] #'embark-bindings)
  ;; Use Embark to show bindings in a key prefix with `C-h`
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :defer t
  :after embark)

;;; Corfu
(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-echo-documentation 0.25)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :init
  (global-corfu-mode)
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1)
  (eldoc-add-command #'corfu-insert)
  (define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down)
  (define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up)
  (define-key corfu-map (kbd "M-d") #'corfu-popupinfo-toggle)

  (use-package kind-icon
    :defer nil
    :custom
    (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))


(use-package corfu-terminal
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode)))


;;; Cape
;; Setup Cape for better completion-at-point support and more
(use-package cape
  :config
  ;; Add useful defaults completion sources from cape
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))))

(provide 'init-vertico)
;;; init-vertico.el ends here

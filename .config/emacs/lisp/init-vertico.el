;;; init-vertico.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

(use-package vertico
  :straight t
  :ensure t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :hook (after-init . vertico-mode)
  :custom
  (vertico-cycle t))
(use-package nerd-icons-completion
  :straight t
  :ensure t
  :hook (vertico-mode . nerd-icons-completion-mode))

(use-package vertico-posframe
  :straight t
  :ensure t
  :when (and custom-vertico-posframe 'display-graphic-p)
  :after vertico
  :hook (vertico-mode . vertico-posframe-mode)
  :config
  (setq vertico-posframe-border-width 5))
    ;; (setq vertico-posframe-parameters
    ;;       '((left-fringe . 20)
    ;;         (right-fringe . 20)))))

;; simple, but effective sorting and filtering for emacs.
(use-package vertico-prescient
  :straight t
  :ensure t
  :defer t
  :config
  (vertico-prescient-mode))

;; annotations placed at the margin of the minibuffer
(use-package marginalia
  :straight t
  :ensure t
  :after vertico
  :defer t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))


(use-package consult
  :straight t
  :ensure t
  :demand t
  :bind (:map minibuffer-local-map
              ("C-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (global-set-key (kbd "C-s") 'consult-line)
  (defun my/consult-switch-to-buffer ()
    "`consult-buffer' with buffers provided by persp."
    (interactive)
    (with-persp-buffer-list () (consult-buffer)))
  (defun my/consult-project-switch-to-buffer ()
    "`consult-project-buffer' with buffers provided by persp."
    (interactive)
    (with-persp-buffer-list () (consult-project-buffer)))
  (global-set-key (kbd "C-M-j") #'my/consult-switch-to-buffer)
  (global-set-key (kbd "C-M-h") #'my/consult-project-switch-to-buffer))

(use-package orderless
  :straight t
  :ensure t
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (style basic partial-completion)))))

;; Quick action in minibuffer
(use-package embark
  :straight t
  :ensure t
  :defer t
  :bind (("C-," . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export))
  :config
  (with-eval-after-load 'embark-consult
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))
  (global-set-key [remap describe-bindings] #'embark-bindings)
  ;; Use Embark to show bindings in a key prefix with `C-h`
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :straight t
  :ensure t
  :defer t
  :after embark)

;;; Corfu
(use-package corfu
  :straight t
  :ensure t
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
  (define-key corfu-map (kbd "M-d") #'corfu-popupinfo-toggle))

(use-package kind-icon
  :straight t
  :ensure t
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (when (icons-displayable-p)
    (setq kind-icon-use-icons nil)
    (setq kind-icon-mapping
          `(
            (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
            (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
            (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
            (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
            (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
            (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
            (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
            (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
            (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
            (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
            (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
            (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
            (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
            (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
            (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
            (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
            (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
            (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
            (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
            (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
            (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
            (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
            (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
            (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
            (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
            (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
            (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
            (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
            (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
            (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
            (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
            (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
            (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
            (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
            (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
            (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))))


;;; Cape
;; Setup Cape for better completion-at-point support and more
(use-package cape
  :straight t
  :ensure t
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

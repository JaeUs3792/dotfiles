;;; init-ctags.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-custom)

;; Ctags IDE on the True Editor
;; @see https://github.com/universal-ctags/citre#quick-start
(use-package citre
  :straight t
  :ensure t
  :defer t
  :diminish
  :commands citre-jump-back
  :functions xref-go-back
  :init
  (setq citre-auto-enable-citre-mode-modes '(prog-mode)
        citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t)

  (defun citre-jump+ ()
    "Jump to the definition of the symbol at point.
Fallback to `xref-find-definitions'."
    (interactive)
    (condition-case _
        (citre-jump)
      (error (call-interactively #'xref-find-definitions))))

  (defun citre-jump-back+ ()
    "Go back to the position before last `citre-jump'.
Fallback to `xref-go-back'."
    (interactive)
    (condition-case _
        (citre-jump-back)
      (error (if (fboundp #'xref-go-back)
                 (call-interactively #'xref-go-back)
               (call-interactively #'xref-pop-marker-stack)))))
  :config
  (with-eval-after-load 'cc-mode (require 'citre-lang-c))
  (with-eval-after-load 'dired (require 'citre-lang-fileref))
  (with-eval-after-load 'verilog-mode (require 'citre-lang-verilog))

  (with-no-warnings
    ;; Use Citre xref backend as a fallback
    (define-advice xref--create-fetcher (:around (fn &rest args) fallback)
      (let ((fetcher (apply fn args))
            (citre-fetcher
             (let ((xref-backend-functions '(citre-xref-backend t)))
               (ignore xref-backend-functions)
               (apply fn args))))
        (lambda ()
          (or (with-demoted-errors "%s, fallback to citre"
                (funcall fetcher))
              (funcall citre-fetcher)))))

    ;; Combine completions from Citre and lsp
    (defun lsp-citre-capf-function ()
      "A capf backend that tries lsp first, then Citre."
      (let ((lsp-result (cond
                         ((bound-and-true-p lsp-mode)
                          (and (fboundp #'lsp-completion-at-point)
                               (lsp-completion-at-point)))
                         ((bound-and-true-p eglot--managed-mode)
                          (and (fboundp #'eglot-completion-at-point)
                               (eglot-completion-at-point))))))
        (if (and lsp-result
                 (try-completion
                  (buffer-substring (nth 0 lsp-result)
                                    (nth 1 lsp-result))
                  (nth 2 lsp-result)))
            lsp-result
          (citre-completion-at-point))))

    (defun enable-lsp-citre-capf-backend ()
      "Enable the lsp + Citre capf backend in current buffer."
      (add-hook 'completion-at-point-functions #'lsp-citre-capf-function nil t))

    (add-hook 'citre-mode-hook #'enable-lsp-citre-capf-backend)))



(provide 'init-ctags)
;;; init-ctags.el ends here

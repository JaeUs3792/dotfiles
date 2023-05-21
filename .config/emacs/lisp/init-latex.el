;;; init-latex.el -*- lexical-binding: t -*-
(use-package auctex
  :defer t
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  ;; to use pdfview with auctex
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (TeX-source-correlate-start-server t)
  ;; electric pairs in auctex
  (TeX-electric-sub-and-superscript t)
  (LaTeX-electric-left-right-brace t)
  (TeX-electric-math (cons "$" "$"))
  :config
  (setq-default TeX-master nil)
  ;; compile to pdf
  (tex-pdf-mode)
  ;; correlate the source and the output
  (TeX-source-correlate-mode)
  ;; set a correct indentation in a few additional environments
  (add-to-list 'LaTeX-indent-environment-list '("lstlisting" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("tikzcd" LaTeX-indent-tabular))
  (add-to-list 'LaTeX-indent-environment-list '("tikzpicture" current-indentation))
  ;; add a few macros and environment as verbatim
  (add-to-list 'LaTeX-verbatim-environments "lstlisting")
  (add-to-list 'LaTeX-verbatim-environments "Verbatim")
  (add-to-list 'LaTeX-verbatim-macros-with-braces "lstinline")
  (add-to-list 'LaTeX-verbatim-macros-with-delims "lstinline")

  ;; open all buffers with the math mode and auto-fill mode
  (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)

  ;; add support for references
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (customize-set-variable 'reftex-plug-into-AUCTeX t)

  ;; to have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(provide 'init-latex)

;;; init-latex.el ends here

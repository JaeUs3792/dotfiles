;;; init-utils.el -*- lexical-binding: t -*-
(use-package rg
  :ensure t
  :defer t)

;; text mode directory tree
(use-package ztree
  :ensure t
  :defer t
  :custom-face
  (ztreep-header-face ((t (:inherit diff-header))))
  (ztreep-arrow-face ((t (:inherit font-lock-comment-face))))
  (ztreep-leaf-face ((t (:inherit diff-index))))
  (ztreep-node-face ((t (:inherit font-lock-variable-name-face))))
  (ztreep-expand-sign-face ((t (:inherit font-lock-function-name-face))))
  (ztreep-diff-header-face ((t (:inherit (diff-header bold)))))
  (ztreep-diff-header-small-face ((t (:inherit diff-file-header))))
  (ztreep-diff-model-normal-face ((t (:inherit font-lock-doc-face))))
  (ztreep-diff-model-ignored-face ((t (:inherit font-lock-doc-face :strike-through t))))
  (ztreep-diff-model-diff-face ((t (:inherit diff-removed))))
  (ztreep-diff-model-add-face ((t (:inherit diff-nonexistent))))
  :bind (:map ztreediff-mode-map
         ("<f6>" . my/ztree-menu))
  :config
  (require 'transient)
  (transient-define-prefix my/ztree-menu ()
    "Ztree menu."
    [["Diff"
      ("C" "copy" ztree-diff-copy)
      ("h" "show/hide equals" ztree-diff-toggle-show-equal-files :transient t)
      ("H" "show/hide ignores" ztree-diff-toggle-show-filtered-files :transient t)
      ("D" "delete" ztree-diff-delete-file)
      ("v" "view" ztree-diff-view-file)
      ("d" "simple diff" ztree-diff-simple-diff-files)
      ("r" "partial rescan" ztree-diff-partial-rescan :transient t)
      ("R" "full rescan" ztree-diff-full-rescan :transient t)]
     ["View"
      ("RET" "expand/collapse or view" ztree-perform-action)
      ("SPC" "view in other" ztree-perform-soft-action)
      ("TAB" "jump side" ztree-jump-side :transient t)
      ("g" "refresh" ztree-refresh-buffer :transient t)
      ("x" "expand/collapse" ztree-toggle-expand-subtree :transient t)
      ("<backspace>" "go to parent" ztree-move-up-in-tree :transient t)]])
  :init (setq ztree-draw-unicode-lines t
              ztree-show-number-of-children t))

(use-package list-environment
  :ensure t
  :defer t
  :hook (list-environment-mode . (lambda ()
                                   (setq tabulated-list-format
                                         (vconcat `(("" ,(if (icons-displayable-p) 2 0)))
                                                  tabulated-list-format))
                                   (tabulated-list-init-header)))
  :init
  (with-no-warnings
    (defun my-list-environment-entries ()
      "Generate environment variable entries list for tabulated-list."
      (mapcar (lambda (env)
                (let* ((kv (split-string env "="))
                       (key (car kv))
                       (val (mapconcat #'identity (cdr kv) "=")))
                  (list key (vector
                             ""
                             ;;(if (icons-displayable-p)
                             ;;    (nerd-icons-octicon "key" :height 0.8 :v-adjust -0.05)
                             ;;  "")
                             `(,key face font-lock-keyword-face)
                             `(,val face font-lock-string-face)))))
              process-environment))
    (advice-add #'list-environment-entries :override #'my-list-environment-entries)))

(unless ON-WINDOWS
  (use-package daemons :ensure t :defer t)
  (use-package tldr :ensure t :defer t))


(provide 'init-utils)
;;; init-utils.el ends here

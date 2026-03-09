;; init-custom.el -*- lexical-binding: t -*-
(customize-set-variable 'large-file-warning-threshold 100000000) ;; 100MB

(with-no-warnings
  (custom-declare-face
   '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
  (custom-declare-face
   '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
  (custom-declare-face
   '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
  (custom-declare-face
   '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))

(provide 'init-custom)
;;; init-custom.el ends here

(setq vc-follow-symlinks t)
(org-babel-load-file
 (expand-file-name
  "emacs.org"
  user-emacs-directory))
(setq-default indent-tabs-mode t)
(setq tab-width 4) ; or any other preferred value
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

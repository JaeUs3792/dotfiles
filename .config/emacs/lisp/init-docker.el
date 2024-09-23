;;; init-docker.el -*- lexical-binding: t -*-
;; Docker
(use-package docker
  :straight t
  :ensure t
  :defer t
  :defines docker-image-run-arguments
  :init (setq docker-image-run-arguments '("-i" "-t" "--rm")
              docker-container-shell-file-name "/bin/bash"))

(use-package dockerfile-mode
  :straight t
  :ensure t
  :defer t)

(provide 'init-docker)
;;; init-docker.el ends here

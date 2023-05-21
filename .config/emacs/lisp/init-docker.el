;;; init-docker.el -*- lexical-binding: t -*-
;; Docker
(use-package docker
  :defines docker-image-run-arguments
  :init (setq docker-image-run-arguments '("-i" "-t" "--rm")
              docker-container-shell-file-name "/bin/bash"))
;;(use-package docker-tramp)
(use-package dockerfile-mode)

(provide 'init-docker)
;;; init-docker.el ends here

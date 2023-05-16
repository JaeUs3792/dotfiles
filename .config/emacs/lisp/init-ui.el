;; init-ui.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-func)

;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Make certain buffers grossly incandescent
(use-package solaire-mode
			 :hook (after-load-theme . solaire-global-mode))
(use-package doom-themes
			 :init
			 (if (display-graphic-p)
			   ;;(load-theme 'doom-palenight t)
			   (load-theme 'doom-one t)
			   (load-theme 'doom-gruvbox t))
			 :config
			 (doom-themes-visual-bell-config))
(use-package doom-modeline
			 :hook (after-init . doom-modeline-mode)
			 :init
			 (setq doom-modeline-window-width-limit 110
				   doom-modeline-minor-modes t)
			 :config
			 (setq doom-modeline-height 15
				   doom-modeline-env-version t
				   doom-modeline-persp-name t
				   doom-modeline-persp-icon t
				   doom-modeline-display-default-persp-name t
				   doom-modeline-indent-info t))



(provide 'init-ui)
;;; init-ui.el ends here.

;;; init-dict.el -*- lexical-binding: t -*-
(require 'init-const)
(use-package go-translate
  :init (setq gts-translate-list '(("en" "ko") ("ko" "en")))
  :config
  ;; config the default translator, it will be used by command gts-do-translate
  (setq gts-default-translator
        (gts-translator
         :picker ; used to pick source text, from, to. choose one.

         ;;(gts-noprompt-picker)
         ;;(gts-noprompt-picker :texter (gts-whole-buffer-texter))
         (gts-prompt-picker)
         ;;(gts-prompt-picker :single t)
         ;;(gts-prompt-picker :texter (gts-current-or-selection-texter) :single t)

         :engines ; engines, one or more. Provide a parser to give different output.

         (list
          (gts-bing-engine)
          (gts-google-engine)
          ;;(gts-google-rpc-engine)
          ;;(gts-deepl-engine :auth-key [YOUR_AUTH_KEY] :pro nil)
          ;;(gts-google-engine :parser (gts-google-summary-parser))
          ;;(gts-google-engine :parser (gts-google-parser))
          ;;(gts-google-rpc-engine :parser (gts-google-rpc-summary-parser) :url "https://translate.google.com")
          ;;(gts-google-rpc-engine :parser (gts-google-rpc-parser) :url "https://translate.google.com")
          )

         :render ; render, only one, used to consumer the output result. Install posframe yourself when use gts-posframe-xxx

         (gts-buffer-render)
         ;;(gts-posframe-pop-render)
         ;;(gts-posframe-pop-render :backcolor "#333333" :forecolor "#ffffff")
         ;;(gts-posframe-pin-render)
         ;;(gts-posframe-pin-render :position (cons 1200 20))
         ;;(gts-posframe-pin-render :width 80 :height 25 :position (cons 1000 20) :forecolor "#ffffff" :backcolor "#111111")
         ;;(gts-kill-ring-render)

         :splitter ; optional, used to split text into several parts, and the translation result will be a list.

         (gts-paragraph-splitter))))

(provide 'init-dict)
;;; init-dict.el ends here

;;<!-- 0120: google translation -->
(use-package! google-translate-default-ui
  :config
  ;;(require 'google-translate-smooth-ui)
  ;;(global-set-key "" 'google-translate-smooth-translate)
  (setq-default google-translate-translation-directions-alist '(("en" . "ko")("ko" . "en")))
  (setq-default google-translate-base-url
                "https://translate.google.com/translate_a/single"
                google-translate--tkk-url
                "https://translate.google.com/"
                google-translate-default-source-language
                "en"
                google-translate-default-target-language
                "ko"
                )
  (global-set-key "\C-ct" 'google-translate-at-point)
  (global-set-key "\C-cT" 'google-translate-query-translate)

  ;; package 버전 0.11.14 이하의 경우 반드시 (package-upgrade) 명령을 통해서 0.11.15 이상으로 업그레이드 해야 함
  (when (and (string-match "0.11.14" (google-translate-version))
             (>= (time-to-seconds) (time-to-seconds (encode-time 0 0 0 23 9 2018))))
    (defun google-translate--get-b-d1 ()
      ;; TKK='427110.1469889687'
      (list 427110 1469889687)))
  )


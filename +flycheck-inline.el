;;<!-- 0200: Flycheck -->
;; LSP 모드로 동작할 때는 eslint_d를 사용할 수 없고 VScode 패키지에 포함된 Language Server를 통해서 작동해야한다
;;(setq flycheck-javascript-eslint-executable "eslint_d")

(use-package! flycheck-inline
  :demand t
  :config
  (dolist (hook (list
                 'emacs-lisp-mode-hook
                 'lisp-interaction-mode-hook
                 'lisp-mode-hook
                 'java-mode-hook
                 'sh-mode-hook
                 'js2-mode-hook
                 'js-mode-hook
                 'html-mode-hook
                 'css-mode-hook
                 'go-mode-hook
                 'slime-repl-mode-hook
                 'cmake-mode-hook
                 'web-mode-hook
                 'typescript-mode-hook
                 'rjsx-mode-hook
                 ))
    (add-hook hook (lambda () (flycheck-inline-mode 1))))
  )


;;<!-- 0200: Flycheck -->
;;
;; (after! flycheck-inline-mode
;;   (global-flycheck-mode)
;;   (flycheck-inline-mode)
;;   (flycheck-display-errors-delay 1)
;;   (add-hook 'js2-mode-hook #'flycheck-inline-mode)
;;   (add-hook 'rjsx-mode-hook #'flycheck-inline-mode)
;;   (add-hook 'typescript-mode-hook #'flycheck-inline-mode)
;;   ;; ⬇ Workaround for eslint loading slow. see https://github.com/flycheck/flycheck/issues/1129#issuecomment-319600923
;;   ;;(advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
;;   ;; LSP 모드로 동작할 때는 eslint_d를 사용할 수 없고 VScode 패키지에 포함된 Language Server를 통해서 작동해야한다
;;   ;;(setq flycheck-javascript-eslint-executable "eslint_d")
;;   )
;; ⬇ LSP 모드로 동작할 때는 사용하지 말것
;; (defun my/use-eslint-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint
;;           (and root
;;                (expand-file-name "node_modules/.bin/eslint"
;;                                  root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))

;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)


(use-package! flycheck-inline
  :defer t
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


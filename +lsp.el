;;<!-- 0220 LSP-MODE -->
;;https://emacs-lsp.github.io/lsp-mode/page/configuration/
;;https://emacs-lsp.github.io/lsp-mode/page/settings/
;;SPC c j -- Jump to symbol in current workspace
;;SPC c J -- Jump to symbol in any workspace
;; (after! lsp-mode
(setq lsp-diagnostics-modeline-scope :project) ;;To see all error statistics in the modeline
(setq lsp-auto-guess-root nil) ;; ls-mode의 내부를 잘 알경우에만 사용
(setq lsp-enable-on-type-formatting nil)
(setq lsp-signature-auto-activate nil)
(setq lsp-enable-folding nil)
(setq lsp-enable-snippet nil)
(setq lsp-enable-completion-at-point t)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.5)
(setq lsp-prefer-capf t)
;;(add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascriptreact"))
(setq lsp-eslint-server-command
      '("node"
        "/Users/andrwj/.vscode-insiders/extensions/dbaeumer.vscode-eslint-2.1.5/server/out/eslintServer.js"
        "--stdio"))
;; ) ;; lsp-mode


;;<!-- 0220 LSP-MODE -->
;;https://emacs-lsp.github.io/lsp-mode/page/configuration/
;;https://emacs-lsp.github.io/lsp-mode/page/settings/
;;SPC c j -- Jump to symbol in current workspace
;;SPC c J -- Jump to symbol in any workspace
(after! lsp-mode
  (setq lsp-diagnostics-modeline-scope   :project ;;To see all error statistics in the modeline
        lsp-auto-guess-root              nil ;; ls-mode의 내부를 잘 알경우에만 사용
        lsp-signature-auto-activate      nil
        lsp-enable-folding               nil
        lsp-enable-completion-at-point   t
        read-process-output-max          (* 1024 1024) ;; 1mb
        lsp-idle-delay                   0.3
        lsp-prefer-capf                  nil
        lsp-log-io                       t
        lsp-print-performance            t
        lsp-report-if-no-buffer          t
        lsp-keep-workspace-alive         t
        lsp-enable-snippet               t
        lsp-restart                      'interactive
        lsp-auto-configure               t
        lsp-document-sync-method         nil
        lsp-auto-execute-action          nil
        lsp-eldoc-render-all             t
        lsp-enable-xref                  t
        lsp-enable-indentation           t
        lsp-enable-on-type-formatting    t
        lsp-signature-auto-activate      t
        lsp-eslint-server-command '("node"
                                    "/Users/andrwj/.vscode-insiders/extensions/dbaeumer.vscode-eslint-2.1.5/server/out/eslintServer.js"
                                    "--stdio")

        ;; lsp-mode사용시, 열려있지 않으면 프로젝트내에 모든 파일을 읽지 못하게 한다
        lsp-intelephense-multi-root nil
        lsp-prefer-flymake nil
        )
  (add-hook 'lsp-managed-mode-hook 'lsp-diagnostics-modeline-mode)
  )


(defun lsp-describe-thing-at-point ()
  "Display the full documentation of the thing at point."
  (interactive)
  (let ((contents (->> (lsp--text-document-position-params)
                       (lsp--make-request "textDocument/hover")
                       (lsp--send-request)
                       (gethash "contents"))))
    (pop-to-buffer
     (with-current-buffer (get-buffer-create "*lsp-help*")
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert (lsp--render-on-hover-content contents t))
         (goto-char (point-min))
         (view-mode t)
         (current-buffer))))))


(use-package! lsp-ui
   :demand
   :config
   (setq lsp-ui-doc-enable                nil
      lsp-ui-doc-position                 'at-point
      ;; lsp-ui-doc-header                   t
      ;; lsp-ui-doc-border                   "#FF0000"
      ;; lsp-ui-doc-background               "#516B8F"
      lsp-ui-doc-include-signature        t
      lsp-ui-sideline-enable              nil
      ;; lsp-ui-sideline-show-symbol         t
      ;; lsp-ui-sideline-show-diagnostics    t
      ;; lsp-ui-sideline-update-mode         'point
      ;; lsp-ui-sideline-delay               0.3
      ;; lsp-ui-sideline-ignore-duplicate    t
      ;; lsp-ui-sideline-show-code-actions   t
      lsp-ui-peek-always-show             nil

      lsp-ui-doc-use-childframe t
      lsp-ui-doc-position                 'bottom
      lsp-ui-doc-include-signature        t
      lsp-ui-sideline-enable              nil
      lsp-ui-flycheck-enable               t
      lsp-ui-flycheck-list-position       'right
      lsp-ui-flycheck-live-reporting       t
      lsp-ui-peek-enable t
      lsp-ui-peek-list-width               80
      lsp-ui-peek-peek-height              25
   )
   (add-hook 'lsp-mode-hook 'lsp-ui-mode)
)


(use-package! company-lsp
  :demand t
  :config
  (setq company-lsp-async               t
        company-lsp-enable-recompletion t
        company-lsp-enable-snippet      t
        company-lsp-cache-candidates    'auto
        company-lsp-filter-candidates   t
        )
  (push 'company-lsp company-backends)
  )


;;<!-- 0230 Company -->
(after! company
  (setq company-selection-wrap-around t
        ;; do or don't automatically start completion after <idle time>
        company-idle-delay 0.3
        ;; at least 1 letters need to be there though
        company-minimum-prefix-length 2
        ;; show completion numbers for hotkeys
        company-show-numbers t
        ;; align annotations to the right
        company-tooltip-align-annotations t
        company-search-regexp-function #'company-search-flex-regexp
        company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend)

        company-tooltip-limit 7
        )
)


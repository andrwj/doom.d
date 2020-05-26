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
        lsp-idle-delay                   1.0
        lsp-prefer-capf                  nil
        lsp-log-io                       t
        lsp-print-performance            t
        lsp-inhibit-message              t
        lsp-report-if-no-buffer          t
        lsp-keep-workspace-alive         t
        lsp-enable-snippet               t
        lsp-restart                      'interactive
        lsp-auto-configure               t
        lsp-document-sync-method         nil
        lsp-auto-execute-action          nil
        lsp-eldoc-render-all             t
        lsp-enable-completion-at-point   nil
        lsp-enable-xref                  t
        lsp-enable-indentation           t
        lsp-enable-on-type-formatting    t
        lsp-signature-auto-activate      t
        lsp-enable-semantic-highlighting t
        lsp-eslint-server-command '("node"
                                    "/Users/andrwj/.vscode-insiders/extensions/dbaeumer.vscode-eslint-2.1.5/server/out/eslintServer.js"
                                    "--stdio")

        )

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


(after! lsp-ui
  (setq lsp-ui-doc-enable                   nil
        lsp-ui-doc-position                 'at-point
        lsp-ui-doc-header                   t
        lsp-ui-doc-border                   "red"
        lsp-ui-doc-background               "516B8F"
        lsp-ui-doc-include-signature        t
        lsp-ui-sideline-enable              nil
        sp-ui-sideline-show-symbol          nil
        lsp-ui-sideline-show-diagnostics    nil
        lsp-ui-sideline-update-mode         'point
        lsp-ui-sideline-delay               0.5
        lsp-ui-sideline-ignore-duplicate    t
        lsp-ui-sideline-show-code-actions   t
        lsp-ui-peek-always-show             t
        lsp-ui-flycheck-enable              t
        lsp-ui-doc-max-height               80
        lsp-ui-doc-max-width                200
        lsp-ui-doc-include-signature        t

        )
)


; (after! lsp-ui-peek
;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
; )


(after! company-lsp
  (setq company-lsp-async               t
        company-lsp-enable-recompletion t
        company-lsp-enable-snippet      t
        company-lsp-cache-candidates    'auto
        ))


;;<!-- 0230 Company -->
(after! company
  (setq company-selection-wrap-around t
        ;; do or don't automatically start completion after <idle time>
        company-idle-delay 1
        ;; at least 1 letters need to be there though
        company-minimum-prefix-length 2
        ;; show completion numbers for hotkeys
        company-show-numbers t
        ;; align annotations to the right
        company-tooltip-align-annotations t
        company-search-regexp-function #'company-search-flex-regexp
        company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend)
        ))

;;<!-- 0039 -- Centaur-tabs -->
;;https://github.com/ema2159/centaur-tabs#installation
(use-package! centaur-tabs
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("s-<right>" . centaur-tabs-backward)
  ("s-<left>" . centaur-tabs-forward)
  ("C-c t s" . centaur-tabs-counsel-switch-group)
  ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ("C-c t g" . centaur-tabs-group-buffer-groups)
  (:map evil-normal-state-map
	 ("g n" . centaur-tabs-forward)
	 ("g p" . centaur-tabs-backward))
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 45
        centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        ;; 'under' | 'over' 값은 탭 높이에 따라 표시여부가 달라진다
        centaur-tabs-set-bar 'left
        ;;centaur-tabs-active-bar-face "#48A6EE"
        x-underline-at-descent-line t
        centaur-tabs-set-close-button nil
        centaur-tabs-close-button "x"
        centaur-tabs-set-modified-marker t
        ;;(setq centaur-tabs-modified-marker "*"
        niquify-separator "/"
        uniquify-buffer-name-style 'forward
        centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts "Avenir Next Condensed" 220)

  (if (display-graphic-p)
      (progn
        ;; GUI형 Emacs 사용시, 탭으로 이동
        (define-key evil-normal-state-map (kbd "s-{") 'centaur-tabs-backward)
        (define-key evil-normal-state-map (kbd "s-}") 'centaur-tabs-forward))
    (progn
      ;; Terminal Emacs 사용시, 탭으로 이동
      (define-key evil-normal-state-map (kbd "C-c <left>") 'centaur-tabs-backward)
      (define-key evil-normal-state-map (kbd "C-c <right>") 'centaur-tabs-forward))
    )

  ;; 버터 타입에 따라 탭 그룹에 속할 여부 결정
  (defun centaur-tabs-hide-tab (x)
    (let ((name (format "%s" x)))
      (or
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))
       )))

  ;; 버퍼 그룹
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
	   (cond
	    ((or (string-equal "*" (substring (buffer-name) 0 1))
	         (memq major-mode '(magit-process-mode
				                      magit-status-mode
				                      magit-diff-mode
				                      magit-log-mode
				                      magit-file-mode
				                      magit-blob-mode
				                      magit-blame-mode
				                      )))
	     "Emacs")
	    ((derived-mode-p 'prog-mode)
	     "Editing")
	    ((derived-mode-p 'dired-mode)
	     "Dired")
	    ((memq major-mode '(helpful-mode
			                    help-mode))
	     "Help")
	    ((memq major-mode '(org-mode
			                    org-agenda-clockreport-mode
			                    org-src-mode
			                    org-agenda-mode
			                    org-beamer-mode
			                    org-indent-mode
			                    org-bullets-mode
			                    org-cdlatex-mode
			                    org-agenda-log-mode
			                    diary-mode))
	     "OrgMode")
	    (t
	     (centaur-tabs-get-group-name (current-buffer))))))
  ) ;; (use-package! centaur-tabs)


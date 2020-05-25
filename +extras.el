;;<!-- 0105: w3m 웨브라우저 -->
;; https://github.com/emacs-w3m/emacs-w3m
;; brew install w3m
(after! w3m
  (message "0105 -- w3m")
  )


;;<!-- 0250 lisp-mode -->
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)


;;<!-- 0270 Python - PyEnv -->
;; (use-package pyvenv
;;   :config
;;   (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
;; (add-hook 'pyvenv-post-activate-hooks
;;           #'(lambda ()
;;               (call-interactively #'lsp-workspace-restart)))
;; (pyvenv-mode +1)

;;<!-- 0280 Rust Development Env -->
;; 1) curl https://sh.rustup.rs -sSf | sh
;; 2) export PATH="$HOME/.cargo/bin:$PATH"
;; 3) cargo install rustfmt
;;;   rustup component add rustfmt --toolchain stable-x86_64-unknown-linux-gnu
;; 4) Racer is a code completion and source code navigation tool for Rust.
;;;   cargo install racer
;; 5) Rust source code is needed for auto-completion so clone it somewhere:
;;;   git clone git@github.com:rust-lang/rust.git
;; 6) package-install flycheck-rust

;; Racer 설치관련
;; https://github.com/racer-rust/racer

(after! rust-mode
  (setq racer-cmd "~/.cargo/bin/racer")
  ;; 디버깅을 위한 소스 위치
  (setq racer-rust-src-path "~/Develops/Rust/9999-rust/src")
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  ;; 사용자 요청에 의한 포맷팅
  (add-hook 'rust-mode-hook
            (lambda () (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
  ;; 저장하기전 포맷팅
  (add-hook 'before-save-hook
            (lambda () (when (eq 'rust-mode major-mode)
                         (lsp-format-buffer))))
  ;; 탭 3칸
  (add-hook 'rust-mode-hook (lambda () (setq-local tab-width 3)))
  )


;;<!-- undo-fu -->
;; https://gitlab.com/ideasman42/emacs-undo-fu
(after! undo-fu
  (global-undo-tree-mode -1)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)
  (setq evil-want-fine-undo t)
  )

;;<!-- elfeed -->
(after! elfeed
  (global-set-key (kbd "C-x w") 'elfeed)
  (global-set-key (kbd "C-x W") 'elfeed-update)
  (setq elfeed-feeds
        '("https://www.reddit.com/r/emacs.rss")
        '("https://www.reddit.com/r/WebAssembly.rss")
        )
  )

;;<!-- twittering-mode -->
;; brew install gnupg
(after! twittering-mode
  (setq twittering-use-master-password t)
  )


;; <!-------------------------------- ⬇️  정리해야함 ------------------------ !>

;; dired
(after! dired
  (setq dired-listing-switches "-aBhl  --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies (quote always)
        dired-recursive-deletes (quote top)))

;; dired-narrow
(use-package! dired-narrow
  :commands (dired-narrow-fuzzy)
  :init
  (map! :map dired-mode-map
        :desc "narrow" "/" #'dired-narrow-fuzzy))


;; easy-kill
;; (use-package! easy-kill
;;   :bind*
;;   (([remap kill-ring-save] . easy-kill))
;;   )

;; Word-Wrap Mode
(add-hook! 'markdown-mode-hook #'+word-wrap-mode)
(add-hook! 'text-mode-hook #'+word-wrap-mode)
(add-hook! 'javascript-mode-hook #'+word-wrap-mode)
(add-hook! 'web-mode-hook #'+word-wrap-mode)


;; (use-package! anki-editor
;;   :after org-noter
;;   :config
;;   (setq anki-editor-create-decks 't))


;; ;; Kotlin Lnaguage; 코틀린
;; (use-package! kotlin-mode
;;   :mode "\\.kt\\'")


;; ;; Groovy; 그루비
;; (use-package! groovy-mode
;;   :mode "\\.groovy\\'")


;; ;; Dart; 다트
;; (use-package! dart-mode
;;   :mode "\\.dart\\'")


;; Docker Mode
;; (use-package! dockerfile-mode
;;   :mode "Dockerfile\\'"
;;   :config
;;   (put 'dockerfile-image-name 'safe-local-variable #'stringp)
;;   )


;; This supresses the output window. Useful for when I do async exports
;; https://stackoverflow.com/questions/13901955/how-to-avoid-pop-up-of-async-shell-command-buffer-in-emacs

(defun async-shell-command-no-window
    (command)
  (interactive)
  (let
      ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*.*"
          (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command)))


;;;;;;;;;;;;;;;; Emacs confugraiton


;; ;; Prevents some cases of Emacs flickering
;; (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


;; ;; Switch to the new window after splitting
;; (setq evil-split-window-below t
;;       evil-vsplit-window-right t)

;; ;; Silence all that useless output
;; (setq direnv-always-show-summary nil)

;; ;; do not auto-complete until my sign
;; (setq company-idle-delay nil)

;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
;; disable it by default.
;; (setq lsp-ui-sideline-enable nil)
;; (setq lsp-enable-symbol-highlighting nil)


;;;;;;;;;;;;;;;;;;;;;;;  OrgMode ;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-directory "~/.doom.d/org")

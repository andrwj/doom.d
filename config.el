;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;<!-- doom specific settings -->
(setq user-full-name "A.J"
      user-mail-address "andrwj@gmail.com")

;; Noto Sans Font Download:
;; https://www.google.com/get/noto/#/family/noto-sans-kore
(setq doom-font (font-spec :family "Noto Sans Mono CJK KR" :size 18 )
      doom-variable-pitch-font (font-spec :family "Noto Sans Light" :size 18)
      doom-unicode-font (font-spec :name "Noto Sans Mono CJK KR" :size 15)
      ;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for presentations or streaming.
      doom-big-font (font-spec :name "Noto Sans Black" :size 35)
      doom-theme 'doom-one
      )

;;<!-- 0001 환경 변수 -->
;;
;; regular diminished font size, even for the childframes.
(setq +helm-posframe-text-scale 0)

;; 줄간격
(setq-default line-spacing 4)

;; 리더키는 스페이스
(defconst my-leader "SPC")

;; 라인번호 표시
(setq display-line-numbers-type t)


;; <!-- 0002 전역 키 설정 -->
(map!
 (:leader
  (:prefix "b"
   :desc "Kill buffer" "d" #'kill-this-buffer
   ) ;; "b"

  ;; https://github.com/emacsorphanage/helm-ag
  (:prefix ("/" . "search")
   :desc "current buffer only" "b" #'helm-ag-this-file
   :desc "all buffers" "B" #'helm-do-ag-buffers
   :desc "files" "f" #'helm-do-ag
   :desc "in project" "p" #'helm-do-ag-project-root
   ) ;; "/"

  ;; https://github.com/Alexander-Miller/treemacs
  (:prefix ("f" . "file")
   :desc "Toggle show" "t" #'treemacs
   :desc "Add project & open" "a" #'treemacs-add-project-to-workspace
   :desc "Add project exclusively" "A" #'treemacs-display-current-project-exclusively
   (:prefix ("L" . "Create & Collapse")
    :desc "Create Folder" "d" 'treemacs-create-dir
    :desc "Create File" "f" 'treemacs-create-file
    :desc "Callapse projects" "c" #'treemacs-collapse-project
    :desc "Callapse all projects" "C" #'treemacs-collapse-all-projects
    )
   :desc "Remove project" "r" #'treemacs-remove-project
   ) ;; "t"

  (:prefix ("k" . "kill")
   :desc "Save and kill" "e" 'save-buffers-kill-terminal
   :desc "Kill buffer" "b" 'my-kill-this-buffer
   :desc "Delete frame" "f" 'delete-frame
   (:prefix ("o" . "Other")
    :desc "Frames" "f" 'delete-other-frames
    :desc "Windows" "w" 'delete-other-windows
    )
   ) ;; "k"

  (:prefix ("r" . "remote")
   :desc "Ranger" "r" 'ranger
   )
  (:prefix ("t" . "toggle")
   :desc "whitespace-mode" "s" 'global-whitespace-mode
   ;; "rgb" 모듈이 아닌 이 설정파일에 포함된 기능
   :desc "rainbow-delimiters-mode" "R" 'rainbow-delimiters-mode
   )
  ))

;; ;; ** Global Keybindings
;; ;; Normal mode?
;; (nmap
;;  :prefix my-leader
;;  "b d" #'kill-this-buffer
;;  ;; kill things
;;  "k" '(:ignore t :which-key "kill")
;;  "k e" 'save-buffers-kill-terminal
;;  "k b" 'my-kill-this-buffer
;;  "k f" 'delete-frame
;;  "k o f" 'delete-other-frames
;;  "k o w" 'delete-other-windows
;;  "a" 'helm-mini)
;; ;; (my-leader-def 'normal 'override
;; ;;   "a" 'org-agenda)



;;<!--  0003 모든 인덴트 설정 -->
(defun andrwj/setup-indent-env (n)
  (interactive)
  (setq-default c-basic-offset n) ; java/c/c++
  (setq-default javascript-indent-level n) ; javascript-mode
  (setq-default js-indent-level n) ; js-mode
  (setq-default js2-basiceoffset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-default web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-default web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-default web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-default css-indent-offset n) ; css-mode
  (setq-default typescript-indent-level n) ; TypeScript
  (message (format "Set indenation to %d spaces" n))
  )
(andrwj/setup-indent-env 3)

;; 0004 aggressive-indent
(use-package! aggressive-indent
  :hook
  (css-mode . aggressive-indent-mode)
  (emacs-lisp-mode . aggressive-indent-mode)
  (js-mode . aggressive-indent-mode)
  :config
  (setq aggressive-indent-comments-too t)
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-protected-commands 'comment-dwim)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  ;; 들여쓰기가 출렁이는 것을 막음
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'web-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line)))))
  )

;;<!-- 0007 OSX Clipboard -->
;; `gnu'         compiled for a GNU Hurd system.
;; `gnu/linux'   compiled for a GNU/Linux system.
;; `darwin'      compiled for Darwin (GNU-Darwin, Mac OS X, ...).
;; `ms-dos'      compiled as an MS-DOS application.
;; `windows-nt'  compiled as a native W32 application.
;; `cygwin'      compiled using the Cygwin library.
(if (eq system-type 'darwin)
    (progn
      (osx-clipboard-mode +1)
      (setq-default alert-default-style 'osx-notifier)
      ))

(setq evil-want-fine-undo t)

;;<!-- 0008 상태에 따른 커서 모양/색상 지정 (주의: iTerm2에서는 아래설정과 상관없이 해당 어플의 설정이 사용된다) -->
(setq evil-default-cursor (quote (t "#750000"))
      evil-visual-state-cursor '("#880000" box)
      evil-normal-state-cursor '("#750000" box)
      evil-insert-state-cursor '("#e2e222" bar))


;;<!-- 0009 버퍼내/모든 파일에서 문자열 찾기 -->
;; 함수 이름 'helm-*-do-' 인 형태가 실시간 검색용

;; 특정 위치내 모든 파일에 대해 바꾸기
;; helm-ag로 찾은 내용(버퍼) 안에서 바꾸기를 하면, 버퍼내 변경사항을 helm-ag가 모든 파일에 대해 적용되는 것임!
;;	1) helm-do-ag 실행
;;	2) 검색 범위 선정
;;	3) 패턴 지정 (이때 절대 리턴 누르지 말고 C-c C-e 눌러 iedit-mode로 진입!!!)
;;	4) helm-ag 가 표시하는 목록에서 바꾸고자 하는 부분으로 가서 블록을 잡거나 위치 시킴
;;	5) SPC s e 를 눌러 문자열 치환
;;	6) C-c C-c 눌러 적용

(custom-set-variables
 ;;성능문제 때문에 follow-mode를 쓰면 Emacs가 응답하지 못하는 경우가 생긴다.
 ;;대안: treemacs 패널을 enable 해두면 그나마 낫다. .agignore 파일에 검색 제외 대상을 추가한다
 '(helm-follow-mode-persistent t)
 '(helm-ag-use-agignore t)
 '(helm-ag-fuzzy-match t)
 '(helm-ag-use-grep-ignore-list t)
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-command-option " -U --all-text")
 '(helm-ag-insert-at-point 'word)
 '(helm-ag-edit-save t)
 '(helm-ag-use-temp-buffer t)
 '(helm-ag-ignore-buffer-patterns '("\\.DS_Store\\'" "\\.gitignore\\'"))
 )

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows t

      ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
      helm-swoop-split-direction 'split-window-vertically

      ;; If nil, you can slightly boost invoke speed in exchange for text color
      helm-swoop-speed-or-color nil

      ;; Go to the opposite side of line from the end or beginning of line
      elm-swoop-move-to-line-cycle t

      ;; Optional face for line numbers. Face name is `helm-swoop-line-number-face`
      helm-swoop-use-line-number-face t

      ;; If you prefer fuzzy matching
      helm-swoop-use-fuzzy-match t

      ;; If a symbol or phrase is selected, use it as the initial query.
      helm-swoop-pre-input-function (lambda ()
                                      (if mark-active
                                          (buffer-substring-no-properties (mark) (point))
                                        ""))
      )




;;<!-- 0011 언어설정 -->
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(defun andrwj/set-korean-input-method ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (global-unset-key (kbd "S-SPC"))
        (global-unset-key (kbd "C-SPC"))
        (define-key evil-insert-state-map (kbd "C-SPC") 'toggle-korean-input-method)
        (define-key evil-replace-state-map (kbd "C-SPC") 'toggle-korean-input-method)
        (message "<GUI> INPUT method switching: Control-Space")
        )
    (progn
      (global-unset-key (kbd "C-@"))
      (define-key evil-insert-state-map (kbd "C-@") 'toggle-korean-input-method)
      (define-key evil-replace-state-map (kbd "C-@") 'toggle-korean-input-method)
      (message "<TUI> INPUT method switching: Control-Space")
      )
    )
  )
(andrwj/set-korean-input-method)


;;<!-- 0012 evil-multiedit (multiple cursor)-->
;; https://github.com/hlissner/evil-multiedit
;;
;;         패턴잡고 "R"   -- 모든 패턴에 대해 색상강조
;;                alt-d   -- 추가 (forward selection)
;;                alt-D   -- 추가 (backward selection)
;;                ENTER   -- 토글 selection
;;                  C-g   -- 취소
;; 패턴잡은상태에서 C-n   --  다음번 패턴으로 이동
;; 패턴잡은상태에서 C-p   --  이전 패턴으로 이동

;; <!-- 0030 윈도우간 이동 -->
;; In normal state
(define-key evil-normal-state-map (kbd "C-c <up>") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-c <down>") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-c <left>") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-c <right>") 'evil-window-right)

;; In terminal
(evil-define-key 'insert term-raw-map (kbd "C-c <up>") 'evil-window-up)
(evil-define-key 'insert term-raw-map (kbd "C-c <right>") 'evil-window-right)
(evil-define-key 'insert term-raw-map (kbd "C-c <left>") 'evil-window-left)

;;<!-- 0038 ranger -->
(after! ranger
  (ranger-override-dired-mode t)
  (setq ranger-show-hidden t) ;;show dot files
  (setq ranger-dont-show-binary t) ;; don't show binary
  (setq helm-descbinds-window-style 'same-window) ;; helm-descbinds 패키지와 같이 씀에 따라 생기는 문제 해결
  (setq ranger-hide-cursor nil)
  (setq ranger-width-parents 0.2)
  (setq ranger-width-preview 0.55)
  (setq ranger-excluded-extensions '("mkv" "iso" "mp4" "DS_Store" "zip" "tgz" "tar" "gz"))
  )

;;<!-- 0060  Whitespace -->
(use-package whitespace
  :config
  (progn
    ;; Make whitespace-mode with very basic background coloring for whitespaces.
    ;; http://ergoemacs.org/emacs/whitespace-mode.html
    (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))

    ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
    (setq whitespace-display-mappings
          ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
          '(
            (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
            (newline-mark 10 [182 10]) ; LINE FEED,
            (tab-mark 9 [9655 9] [92 9]) ; tab
            )))
  ;; only full-width space
  ;; (setq whitespace-space-regexp "\\(\u3000+\\)")
  (defvar my/bg-color "#232323")
  (set-face-attribute 'whitespace-trailing nil
                      :background my/bg-color
                      :foreground "gray25"
                      :underline t)
  (set-face-attribute 'whitespace-tab nil
                      :background my/bg-color
                      :foreground "gray25"
                      :underline nil)
  (set-face-attribute 'whitespace-space nil
                      :background my/bg-color
                      :foreground "gray25"
                      :weight 'bold)
  (set-face-attribute 'whitespace-empty nil
                      :background my/bg-color))



;;<!-- 0101  magit -->
(setq-default magit-repository-directories '("~/.doom.d/github")) ;; 다른 곳으로 심볼릭 링크를 걸 것
(global-git-commit-mode t)
(setq-default git-magit-status-fullscreen t)
(setq-default vc-handled-backends nil) ;; turn off emacs default git handler for it makes system slow

;;<!-- 0105 w3m 웨브라우저 -->
;; https://github.com/emacs-w3m/emacs-w3m
;; brew install w3m


;;<!-- google translation --> default UI
(require 'google-translate-default-ui)
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


;; <!-- ------------------------:START 0130  Colorize Identifiers --------------------------->
;; ;; https://github.com/mariusk/emacs-color

;; Takes a color string like #ffe0e0 and returns a light
;; or dark foreground color to make sure text is readable.
(defun fg-from-bg (bg)
  (let* ((avg (/ (+ (string-to-number (substring bg 1 3) 16)
                    (string-to-number (substring bg 3 5) 16)
                    (string-to-number (substring bg 5 7) 16)
                    ) 3)))
    (if (> avg 128) "#000000" "#ffffff")
    ))

;; Improved from http://ergoemacs.org/emacs/emacs_CSS_colors.html
;; * Avoid mixing up #abc and #abcabc regexps
;; * Make sure dark background have light foregrounds and vice versa
(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer.
URL `https://github.com/mariusk/emacs-color'
Version 2016-08-09"
  (interactive)
  (font-lock-add-keywords
   nil
   '(
     ("#[ABCDEFabcdef[:digit:]]\\{6\\}"
      (0 (progn (let* ((bgstr (match-string-no-properties 0))
                       (fgstr (fg-from-bg bgstr)))
                  (put-text-property
                   (match-beginning 0)
                   (match-end 0)
                   'face (list :background bgstr :foreground fgstr))))))
     ("#[ABCDEFabcdef[:digit:]]\\{3\\}[^ABCDEFabcdef[:digit:]]"
      (0 (progn (let* (
                       (ms (match-string-no-properties 0))
                       (r (substring ms 1 2))
                       (g (substring ms 2 3))
                       (b (substring ms 3 4))
                       (bgstr (concat "#" r r g g b b))
                       (fgstr (fg-from-bg bgstr)))
                  (put-text-property
                   (match-beginning 0)
                   (- (match-end 0) 1)
                   'face (list :background bgstr :foreground fgstr)
                   )))))
     ))
  (font-lock-fontify-buffer))
;; Generates a list of random color values using the
;; Golden Ratio method described here:
;;   http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/
;; The list will be length long. Example:
;;
;; (gen-col-list 3 0.5 0.65)
;; => ("#be79d2" "#79d2a4" "#d28a79")
(require 'color)
(defun gen-col-list (length s v &optional hval)
  (cl-flet ( (random-float () (/ (random 10000000000) 10000000000.0))
             (mod-float (f) (- f (ffloor f))) )
    (unless hval
      (setq hval (random-float)))
    (let ((golden-ratio-conjugate (/ (- (sqrt 5) 1) 2))
          (h hval)
          (current length)
          (ret-list '()))
      (while (> current 0)
        (setq ret-list
              (append ret-list
                      (list (apply 'color-rgb-to-hex (color-hsl-to-rgb h s v)))))
        (setq h (mod-float (+ h golden-ratio-conjugate)))
        (setq current (- current 1)))
      ret-list)))

(defun set-random-rainbow-colors (s l &optional h)
  ;; Output into message buffer in case you get a scheme you REALLY like.
  ;; (message "set-random-rainbow-colors %s" (list s l h))

  (rainbow-delimiters-mode t)

  ;; I also want css style colors in my code.
  (xah-syntax-color-hex)

  ;; Show mismatched braces in bright red.
  (set-face-background 'rainbow-delimiters-unmatched-face "red")

  ;; Rainbow delimiters based on golden ratio
  (let ( (colors (gen-col-list 9 s l h))
         (i 1) )
    (let ( (length (length colors)) )
      ;;(message (concat "i " (number-to-string i) " length " (number-to-string length)))
      (while (<= i length)
        (let ( (rainbow-var-name (concat "rainbow-delimiters-depth-" (number-to-string i) "-face"))
               (col (nth i colors)) )
          ;; (message (concat rainbow-var-name " => " col))
          (set-face-foreground (intern rainbow-var-name) col))
        (setq i (+ i 1))))))

(add-hook 'js-mode-hook '(lambda () (set-random-rainbow-colors 0.5 0.49)))
(add-hook 'emacs-lisp-mode-hook '(lambda () (set-random-rainbow-colors 0.5 0.49)))
(add-hook 'lisp-mode-hook '(lambda () (set-random-rainbow-colors 0.5 0.49)))

;; <!-- ------------------------:END 0130  Colorize Identifiers --------------------------->


;; <!-- 0200 Flycheck -->

(after! 'flycheck
  (setq flycheck-javascript-eslint-executable "eslint_d")
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))
  (global-flycheck-mode)
  (flycheck-inline-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-display-errors-delay 1)
  ;; Workaround for eslint loading slow
  ;; https://github.com/flycheck/flycheck/issues/1129#issuecomment-319600923
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
  )
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint
          (and root
               (expand-file-name "node_modules/.bin/eslint"
                                 root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(after! 'js2-mode
	'(add-hook 'js2-mode-hook 'flycheck-inline-mode))


;;<!-- 0250 lisp-mode -->
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)


;;<!-- 0280 Rust Development Env -->
;; official document: http://develop.spacemacs.org/layers/+lang/rust/README.html
;; 0) add 'rust' layer into dotspacemacs-configuration-layers
;;;   (rust :variables
;;;       rust-backend 'racer ;; or 'lsp
;;;       )
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

(defun andrwj/setup-rust-env ()
  "Rust 개발환경 셋업"
  (interactive)
  ;; (add-hook 'rust-mode-hook
  ;;           (lambda () (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
  (setq racer-cmd "~/.cargo/bin/racer")
  (setq racer-rust-src-path "~/Develops/Rust/9999-rust/src")
  ;; (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  )

(add-hook 'rust-mode-hook (lambda () (setq-local tab-width 3)))


;; <!-------------------------------- 정리해야함 ------------------------ !>
;; awesome-tab
(if (display-graphic-p)
    (progn
      ;; GUI형 Emacs 사용시, 탭으로 이동
      (define-key evil-normal-state-map (kbd "s-{") 'previous-buffer)
      (define-key evil-normal-state-map (kbd "s-}") 'next-buffer))

  (progn
    ;; Terminal Emacs 사용시, 탭으로 이동
    (define-key evil-normal-state-map (kbd "C-c [") 'prevous-buffer)
    (define-key evil-normal-state-map (kbd "C-c ]") 'next-buffer)
    ))

;;; I-search
(setq search-highlight t
      search-whitespace-regexp ".*?"
      isearch-lax-whitespace t
      isearch-regexp-lax-whitespace nil
      isearch-lazy-highlight t
      isearch-lazy-count t
      lazy-count-prefix-format " (%s/%s) "
      lazy-count-suffix-format nil
      isearch-yank-on-move 'shift
      isearch-allow-scroll 'unlimited)

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
(use-package! easy-kill
  :bind*
  (([remap kill-ring-save] . easy-kill))
  )

;; Word-Wrap Mode
(add-hook! 'markdown-mode-hook #'+word-wrap-mode)
(add-hook! 'text-mode-hook #'+word-wrap-mode)
(add-hook! 'javascript-mode-hook #'+word-wrap-mode)
(add-hook! 'web-mode-hook #'+word-wrap-mode)


;; pdf-view 설정
(after! pdf-view
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-width)
  (add-hook! 'pdf-view-mode-hook (evil-colemak-basics-mode -1))
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t
        pdf-view-resize-factor 1.1)
  ;; faster motion
  (map!
   :map pdf-view-mode-map
   :n "g g"          #'pdf-view-first-page
   :n "G"            #'pdf-view-last-page
   :n "N"            #'pdf-view-next-page-command
   :n "E"            #'pdf-view-previous-page-command
   :n "e"            #'evil-collection-pdf-view-previous-line-or-previous-page
   :n "n"            #'evil-collection-pdf-view-next-line-or-next-page
   :localleader
   (:prefix "o"
    (:prefix "n"
     :desc "Insert" "i" 'org-noter-insert-note
     ))
   ))


(use-package! anki-editor
  :after org-noter
  :config
  (setq anki-editor-create-decks 't))


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


;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Silence all that useless output
(setq direnv-always-show-summary nil)

;; do not auto-complete until my sign
(setq company-idle-delay nil)

;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
;; disable it by default.
(setq lsp-ui-sideline-enable nil)
(setq lsp-enable-symbol-highlighting nil)


;;;;;;;;;;;;;;;;;;;;;;;  OrgMode ;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-directory "~/Develops/Emacs/org-documents")

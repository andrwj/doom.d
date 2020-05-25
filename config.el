;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; 참조: https://github.com/jwiegley/use-package
;;
;;<!-- 0000: doom specific settings -->
(setq user-full-name "A.J"
      user-mail-address "andrwj@gmail.com")

;; Noto Sans Font Download: https://www.google.com/get/noto/#/family/noto-sans-kore
(if (display-graphic-p)
    (progn
      (setq doom-font (font-spec :family "Noto Sans Mono CJK KR" :size 18 )
            doom-variable-pitch-font (font-spec :family "Noto Sans Light" :size 18)
            doom-unicode-font (font-spec :name "Noto Sans Mono CJK KR" :size 15)
            doom-big-font (font-spec :name "Noto Sans Black" :size 35) ;; use this for presentations or streaming.
            doom-theme 'doom-one
            )
      ))

;;<!-- 0001: 환경 변수 -->
(setq +helm-posframe-text-scale 0) ;; regular diminished font size, even for the childframes.
(setq-default line-spacing 2) ;; 줄간격 (line-height)
(defconst my-leader "SPC") ;; 리더키는 스페이스
(setq display-line-numbers-type t) ;; 라인번호 표시


;; <!-- 0002: 전역 키 설정 -->
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


;;<!--  0003: 모든 인덴트 설정 -->
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

;;<!-- 0004: No aggressive-indent, No Emacs Life! -->
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

;;<!-- 0007: OSX Clipboard -->
(if (eq system-type 'darwin)
    (progn
      (osx-clipboard-mode +1)
      (setq-default alert-default-style 'osx-notifier)
      ))
;; 참고
;; `gnu'         compiled for a GNU Hurd system.
;; `gnu/linux'   compiled for a GNU/Linux system.
;; `darwin'      compiled for Darwin (GNU-Darwin, Mac OS X, ...).
;; `ms-dos'      compiled as an MS-DOS application.
;; `windows-nt'  compiled as a native W32 application.
;; `cygwin'      compiled using the Cygwin library.


;;<!-- 0008: 상태에 따른 커서 모양/색상 지정 (주의: iTerm2에서는 아래설정과 상관없이 해당 어플의 설정이 사용된다) -->
(setq evil-default-cursor (quote (t "#750000"))
      evil-visual-state-cursor '("#880000" box)
      evil-normal-state-cursor '("#750000" box)
      evil-insert-state-cursor '("#e2e222" bar)
      )


;;<!-- 0009: 버퍼내/모든 파일에서 문자열 찾기 -->
;; 함수 이름 'helm-*-do-' 인 형태가 실시간 검색용

;; 특정 위치내 모든 파일에 대해 바꾸기
;; helm-ag로 찾은 내용(버퍼) 안에서 바꾸기를 하면, 버퍼내 변경사항을 helm-ag가 모든 파일에 대해 적용되는 것임!
;; 1) helm-do-ag 실행
;; 2) 검색 범위 선정
;; 3) 패턴 지정 (이때 절대 리턴 누르지 말고 C-c C-e 눌러 iedit-mode로 진입!!!)
;; 4) helm-ag 가 표시하는 목록에서 바꾸고자 하는 부분으로 가서 블록을 잡거나 위치 시킴
;; 5) SPC s e 를 눌러 문자열 치환
;; 6) C-c C-c 눌러 적용

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




;;<!-- 0011: 언어설정 -->
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


;;<!-- 0012: evil-multiedit (multiple cursor)-->
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

;;<!-- 0038: ranger -->
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

;;<!-- 0060:  Whitespace -->
(use-package! whitespace
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

(load! "+flycheck-inline")
(load! "+lsp")


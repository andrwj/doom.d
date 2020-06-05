(defun andrwj/add-keybindings-4clojure()
  (interactive)
   (define-key evil-normal-state-map "gN" '4clojure-next-question)
   (define-key evil-normal-state-map "gP" '4clojure-previous-question)
   (define-key evil-normal-state-map "gC" '4clojure-check-answers)
   (define-key evil-normal-state-map "gO" '4clojure-open-question)
  )

(defun andrwj/enable-4clojure-env()
  (interactive)
   (require 'cider)
   (require '4clojure)
   ;; origin: https://github.com/losingkeys/4clojure.el
   ;; evil-mode key bindings ==> 082-add-4clojure-keybindings-in-evil-mode.el
   ;; 현재행 평가: SPC m e f
   ;; 마지막 sexp 평가: SPC m e e
   (defadvice 4clojure-open-question (around 4clojure-open-question-around)
      "Start a cider/nREPL connection if one hasn't already been started when opening 4clojure questions"
      ad-do-it
      (unless cider-current-clojure-buffer
         (cider-jack-in)))

  (andrwj/add-keybindings-4clojure)

  )

;; 참조사이트 http://sachachua.com/blog/2014/05/playing-around-clojure-cider-4clojure/
(defun andrwj/enable-4clojure-in-orgmode ()
  (interactive)
  (require '4clojure)
   (add-to-list 'org-babel-load-languages '(emacs-lisp . t))
   (add-to-list 'org-babel-load-languages '(clojure . t))
   (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
   (setq nrepl-hide-special-buffers t
            cider-repl-pop-to-buffer-on-connect nil
            cider-popup-stacktraces nil
            cider-repl-popup-stacktraces t)
   (cider-jack-in)
  )

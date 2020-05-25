;; <!-- ------------------------:START 0130  Colorize Identifiers --------------------------->
;; https://github.com/mariusk/emacs-color

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

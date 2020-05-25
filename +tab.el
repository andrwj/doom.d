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


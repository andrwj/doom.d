;;<!-- 0101:  magit -->
(after! magit
  (setq-default magit-repository-directories '("~/.doom.d/github")) ;; 다른 곳으로 심볼릭 링크를 걸 것
  (global-git-commit-mode t)
  (setq-default git-magit-status-fullscreen t)
  (setq-default vc-handled-backends nil) ;; turn off emacs default git handler for it makes system slow
  )


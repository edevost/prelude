;;; package --- Summary
;;; Commentary:
;;; Code:

;;; Color theme
(load-theme 'misterioso t)

;;; Set yaml mode with files ending in sls, for salt stack
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . yaml-mode))

;;; Add neo-tree function

(add-to-list 'load-path "~/.emacs.d/personal/preload/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(setq projectile-switch-project-action 'neotree-projectile-action)

(setq magit-git-executable "/usr/local/git/bin/git")

(provide 'prefs.el)
;;; prefs.el ends here

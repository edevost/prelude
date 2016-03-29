;;; package --- Summary
;;; Commentary:
;;; Code:

;;; Color theme
(color-theme-initialize)
(color-theme-dark-laptop)

;;; Set yaml mode with files ending in sls, for salt stack
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

(provide 'prefs.el)
;;; prefs.el ends here

;;; package --- Summary
;;; Commentary:
;;; Code:

;;; Color theme
;;(load-theme 'tango-dark t)

;;; Set yaml mode with files ending in sls, for salt stack
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

(require 'ess-site)

(provide 'prefs.el)
;;; prefs.el ends here

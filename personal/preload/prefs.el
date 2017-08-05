;;; package --- Summary
;;; Commentary:
;;; Code:

;;; Color theme
;;(load-theme 'tango-dark t)

;;; Set yaml mode with files ending in sls, for salt stack
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . yaml-mode))

;;(require 'ess-site)


;;; Add neo-tree function

(add-to-list 'load-path "~/.emacs.d/personal/preload/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(setq projectile-switch-project-action 'neotree-projectile-action)

(setq magit-git-executable "/usr/local/git/bin/git")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set emacs web search ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq search-engines
      '(
        (("google" "g") "https://google.com/search?q=%s")
        (("duckduckgo" "d" "ddg") "https://duckduckgo.com/?q=%s")
        (("rfc" "r") "https://www.rfc-editor.org/rfc/rfc%s.txt")
        (("rfc-kw" "rk") "https://www.rfc-editor.org/search/rfc_search_detail.php?title=%s")
        ))
(setq search-engine-default "google")

(defun search-get-engine (engine-name engine-list)
  (cond
   ((null engine-list) nil)
   ((member engine-name (caar engine-list)) (cadar engine-list))
   (t (search-get-engine engine-name (cdr engine-list)))))

(defun search-engine (engine-name term)
  "Search for a term using an engine."
  (interactive "MEngine: \nMTerm: ")
  (let* ((url (search-get-engine engine-name search-engines)))
    (if (equal url nil)
        (message "Error: search engine \"%s\" unknown." engine-name)
      (eww (format url (url-hexify-string term))))))

(defun search-web (term)
  "Search the web using google or a specified engine."
  (interactive "MQuery: ")
  (let ((idx (position ?: term)))
    (if (equal idx nil)
        (search-engine search-engine-default term)
      (search-engine (subseq term 0 idx)
                     (subseq term (+ 1 idx))))))

(global-set-key (kbd "C-c w") 'search-web)
(provide 'prefs.el)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set export for rst in org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/personal/preload/ox-rst-master")
(require 'ox-rst)

;;; prefs.el ends here

(require 'ido)
(ido-mode t)

;; Initialise elpy

(package-initialize)
(elpy-enable)
;; Initialse org and setup capture
(require 'org)
(setq org-directory "~/GTD/")

(setq org-refile-use-outline-path 'file)
(setq org-default-notes-file (concat org-directory "intray.org"))
(setq org-agenda-files '("~/GTD"))

(define-key global-map "\C-cc" 'org-capture)
(setq org-refile-targets '((org-agenda-files :maxlevel . 1)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

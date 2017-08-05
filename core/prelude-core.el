;;; prelude-core.el --- Emacs Prelude: Core Prelude functions.
;;
;; Copyright © 2011-2017 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Here are the definitions of most of the functions added by Prelude.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'thingatpt)
(require 'dash)
(require 'ov)

(defun prelude-open-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (eq major-mode 'dired-mode)
              (dired-get-file-for-visit)
             buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file witemacs word movement minibufferh: ")
                    open)))
    (start-process "prelude-open-with-process" nil program current-file-name)))

(defun prelude-buffer-mode (buffer-or-name)
  "Retrieve the `major-mode' of BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    major-mode))

(defun prelude-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro prelude-install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "prelude-%s" search-engine-name)) ()
       ,(format "Search %s with a query or region if any." search-engine-name)
       (interactive)
       (prelude-search ,search-engine-url ,search-engine-prompt)))

(prelude-install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")
(prelude-install-search-engine "youtube"    "http://www.youtube.com/results?search_query=" "Search YouTube: ")
(prelude-install-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")
(prelude-install-search-engine "duckduckgo" "https://duckduckgo.com/?t=lm&q="              "Search DuckDuckGo: ")

(defun prelude-todo-ov-evaporate (_ov _after _beg _end &optional _length)
  (let ((inhibit-modification-hooks t))
    (if _after (ov-reset _ov))))

(defun prelude-annotate-todo ()
  "Put fringe marker on TODO: lines in the curent buffer."
  (interactive)
  (ov-set (format "[[:space:]]*%s+[[:space:]]*TODO:" comment-start)
          'before-string
          (propertize (format "A")
                      'display '(left-fringe right-triangle))
          'modification-hooks '(prelude-todo-ov-evaporate)))

(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line
or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun prelude-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (prelude-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun prelude-duplicate-and-comment-current-line-or-region (arg)
  "Duplicates and comments the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (prelude-get-positions-of-line-or-region))
                (region (buffer-substring-no-properties beg end)))
    (comment-or-uncomment-region beg end)
    (setq end (line-end-position))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun prelude-rename-buffer-and-file ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun prelude-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun prelude-view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point-min) (point))
    (delete-blank-lines)
    (set-auto-mode)))

(defun prelude-cleanup-buffer-or-region ()
  "Cleanup a region if selected, otherwise the whole buffer."
  (interactive)
  (call-interactively 'untabify)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (call-interactively 'indent-region))
  (whitespace-cleanup))

(defun prelude-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%s" value))))

(defun prelude-recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory prelude-dir 0))

(defvar prelude-tips
  '("Press <C-c o> to open a file with external program."
    "Press <C-c p f> to navigate a project's files with ido."
    "Press <s-r> to open a recently visited file."
    "Press <C-c p s g> to run grep on a project."
    "Press <C-c p p> to switch between projects."
    "Press <C-=> to expand the selected region."
    "Press <C-c g> to search in Google."
    "Press <C-c G> to search in GitHub."
    "Press <C-c y> to search in YouTube."
    "Press <C-c U> to search in DuckDuckGo."
    "Press <C-c r> to rename the current buffer and the file it's visiting if any."
    "Press <C-c t> to open a terminal in Emacs."
    "Press <C-c k> to kill all the buffers, but the active one."
    "Press <C-x g> to run magit-status."
    "Press <C-c D> to delete the current file and buffer."
    "Press <C-c s> to swap two windows."
    "Press <S-RET> or <M-o> to open a line beneath the current one."
    "Press <s-o> to open a line above the current one."
    "Press <C-c C-z> in a Elisp buffer to launch an interactive Elisp shell."
    "Press <C-Backspace> to kill a line backwards."
    "Press <C-S-Backspace> or <s-k> to kill the whole line."
    "Press <s-j> or <C-^> to join lines."
    "Press <s-.> or <C-c j> to jump to the start of a word in any visible window."
    "Press <f11> to toggle fullscreen mode."
    "Press <f12> to toggle the menu bar."
    "Explore the Tools->Prelude menu to find out about some of Prelude extensions to Emacs."
    "Access the official Emacs manual by pressing <C-h r>."
    "Visit the EmacsWiki at http://emacswiki.org to find out even more about Emacs."))

(defun prelude-tip-of-the-day ()
  "Display a random entry from `prelude-tips'."
  (interactive)
  (when (and prelude-tips (not (window-minibuffer-p)))
    ;; pick a new random seed
    (random t)
    (message
     (concat "Prelude tip: " (nth (random (length prelude-tips)) prelude-tips)))))

(defun prelude-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(require 'epl)

(defun prelude-update ()
  "Update Prelude to its latest version."
  (interactive)
  (when (y-or-n-p "Do you want to update Prelude? ")
    (message "Updating installed packages...")
    (epl-upgrade)
    (message "Updating Prelude...")
    (cd prelude-dir)
    (shell-command "git pull")
    (prelude-recompile-init)
    (message "Update finished. Restart Emacs to complete the process.")))

(defun prelude-update-packages (&optional arg)
  "Update Prelude's packages.
This includes package installed via `prelude-require-package'.

With a prefix ARG updates all installed packages."
  (interactive "P")
  (when (y-or-n-p "Do you want to update Prelude's packages? ")
    (if arg
        (epl-upgrade)
      (epl-upgrade (-filter (lambda (p) (memq (epl-package-name p) prelude-packages))
                            (epl-installed-packages))))
    (message "Update finished. Restart Emacs to complete the process.")))

;;; Emacs in OSX already has fullscreen support
;;; Emacs has a similar built-in command in 24.4
(defun prelude-fullscreen ()
  "Make Emacs window fullscreen.

This follows freedesktop standards, should work in X servers."
  (interactive)
  (if (eq window-system 'x)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "Only X server is supported")))

(defun prelude-wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(provide 'prelude-core)
;;; prelude-core.el ends here

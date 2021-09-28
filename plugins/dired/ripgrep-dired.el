;;; ripgrep-dired.el --- ripgrep dired tool. -*- lexical-binding: t -*-

;; Author: Jian Wang <leuven65@gmail.com>
;; URL: https://github.com/leuven65/ripgrep-dired
;; Version: 0.1.0
;; Keywords: fd, dired

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'pcre2el)

(defvar ripgrep-dired-rg-basic-args
  (if (eq system-type 'windows-nt)
      "-nH --no-heading --smart-case --path-separator '//' --glob-case-insensitive"
    ;; for linux
    "-nH --no-heading --smart-case"))

(defvar ripgrep-dired-rg-dired-args
  (if (eq system-type 'windows-nt)
      "rg --files-with-matches --color never %s -0 %s . | xargs -0 -n 1 -P 8 ls -ld &"
    ;; for linux
    ;; "-b": escape the control character or space
    "rg --files-with-matches --color never %s -0 %s . | xargs -0 -n 1 -P 8 ls -ldb &"
    ))

(defvar ripgrep-dired-rg-args-history nil)

(defvar ripgrep-dired-rx-grep-match-ansi-seq
  (rxt-pcre-to-elisp (concat "(?:"
                             "(?:\033\\[0m)?\033\\[1m\033\\[31m" ; rg ansi seq
                             "|"
                             "\033\\[0?1;31m" ; grep ansi seq
                             ")"
                             "(.*?)"    ; matched string
                             "\033\\[[0-9]*m"))
  )

(defun ripgrep-dired-rg-verison ()
  (string-trim (shell-command-to-string "rg -V") "ripgrep "))

;; Handle match highlighting escape sequences inserted by the rg process.
;; This funciton is copie from builtin, I just replace the regexp.
(defun ripgrep-dired-rg-filter ()
  (save-excursion
    (forward-line 0)  ; go to beging of the line
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; Highlight grep matches and delete marking sequences.
        (while (re-search-forward ripgrep-dired-rx-grep-match-ansi-seq end 1)
          (replace-match (propertize (match-string 1)
                                     'face nil 'font-lock-face grep-match-face)
                         t t)
          (cl-incf grep-num-matches-found))
        ;; Delete all remaining escape sequences
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[mK]" end 1)
          (replace-match "" t t)))))
  )

(with-eval-after-load 'grep
  (add-hook 'grep-mode-hook
            (lambda ()
              (add-hook 'compilation-filter-hook #'ripgrep-dired-rg-filter nil t)))
  ;; (customize-set-variable 'grep-highlight-matches 'always)
  )

(defun ripgrep-dired-read-rg-args ()
  (read-string "Run rg (with args): "
               (let ((ss (thing-at-point 'sexp)))
                 (when ss (format "%s" ss)))
               'ripgrep-dired-rg-args-history))

;; just a hack on the original function `find-dired'
;; it is not good idea to rewrite the function, so hacking is good idea here
;; Maybe I am the first person to hack this function for `fd' ^-^.
(defun ripgrep-dired-hacked-find-dired (dir my-cmd-args)
  (let ((default-directory dir))
    ;; define advice on the shell-command to replace the command
    (define-advice shell-command (:around (orig-fun find-cmd-args &rest r) my-cmd)
      (advice-remove 'shell-command #'shell-command@my-cmd)
      (message "shell-command@my-cmd: %s" my-cmd-args)
      (cl-assert (string-match-p "^find" find-cmd-args))
      ;; call origin function with my command
      (apply orig-fun my-cmd-args r))

    ;; call function
    ;; the above advice will be called
    (find-dired dir my-cmd-args)

    ;; replace the 2nd line by my command
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (let ((buffer-read-only nil))
        (let ((beg (line-beginning-position))
              (end (line-end-position)))
          (delete-region beg end)
          (insert "  " my-cmd-args)
          (cl-assert (> end (point)))
          ;; insert space to keep this line length is same as before
          ;; as find-dired passed the position to process-filter
          (insert (make-string (- end (point)) ?\s))
          )
        )
      )

    ;; set refresh function
    (when (local-variable-p 'revert-buffer-function)
      (setq revert-buffer-function
            `(lambda (ignore-auto noconfirm)
               (ripgrep-dired-hacked-find-dired ,dir ,my-cmd-args)))
      )
    ))

(defun ripgrep-dired-find-grep-dired (dir args)
  (interactive (list (read-directory-name "Run rg in directory: " nil "" t)
                     (ripgrep-dired-read-rg-args)))
  (let ((rg-cmd-args (format ripgrep-dired-rg-dired-args
                             ripgrep-dired-rg-basic-args
                             args)))
    (ripgrep-dired-hacked-find-dired dir rg-cmd-args)
    ))

;;;###autoload
(defalias 'rg-dired 'ripgrep-dired-find-grep-dired)

;; add it to action of helm
(with-eval-after-load "helm-files"
  (defun my-rg-find-grep-dired-helm-find-file-action (file-path)
    (ripgrep-dired-find-grep-dired (if (file-directory-p file-path)
                                       file-path
                                     (file-name-directory file-path))
                                   (ripgrep-dired-read-rg-args)))
  (add-to-list 'helm-find-files-actions
               '("Find grep dired by rg" . my-rg-find-grep-dired-helm-find-file-action)
               t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               rg-grep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rg-grep (args)
  "Run rg command"
  (interactive (list (ripgrep-dired-read-rg-args)))
  (let ((grep-use-null-device nil) ;avoid append NUL by `grep'
        (rg-cmd-args (format "rg --color always %s %s" ripgrep-dired-rg-basic-args args)))
    (grep rg-cmd-args)))

;; (defalias 'rg 'rg-grep)

;; add it to action of helm
(with-eval-after-load "helm-files"
  (defun ripgrep-dired-rg-grep-helm-find-file-action (file-path)
    (let ((default-directory helm-ff-default-directory))
      (rg-grep (format "%s '%s'"
                       (ripgrep-dired-read-rg-args)
                       (file-relative-name file-path helm-ff-default-directory)))))
  (add-to-list 'helm-find-files-actions
               '("Grep by rg" . ripgrep-dired-rg-grep-helm-find-file-action)
               t)
  )

;; support rg in eshell
(with-eval-after-load 'esh-mode
  (defun eshell/rg (&rest args)
    "Use rg in eshell"
    (let ((args (seq-concatenate 'list '("-H" "--smart-case" "--path-separator" "//" "--color" "always") args)))
      ;; (message "rg: %s" args)
      (eshell-printn (format "rg -n %s" (mapconcat #'identity args " ")))
      (eshell-grep "rg" args t))
    )
  )

(provide 'ripgrep-dired)

;;; ripgrep-dired.el ends here

;; -*- lexical-binding: t; -*-

;;; company-files.el --- company-mode completion backend for file names  -*- lexical-binding: t -*-

;; Copyright (C) 2009-2011, 2013-2024  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'cl-lib)

(defgroup company-files nil
  "Completion backend for file names."
  :group 'company)

(defcustom company-files-exclusions nil
  "A list of file name extensions and directory names to ignore.
The values should use the same format as `completion-ignored-extensions'."
  :type '(repeat (string :tag "File extension or directory name"))
  :package-version '(company . "0.9.1"))

(defun company-files--directory-files (dir prefix)
  ;; Don't use directory-files. It produces directories without trailing /.
  (condition-case _err
      (let ((comp (sort (file-name-all-completions prefix dir)
                        (lambda (s1 s2) (string-lessp (downcase s1) (downcase s2))))))
        (when company-files-exclusions
          (setq comp (company-files--exclusions-filtered comp)))
        (if (string-empty-p prefix)
            (delete "../" (delete "./" comp))
          comp))
    (file-error nil)))

(defun company-files--exclusions-filtered (completions)
  (let* ((dir-exclusions (cl-remove-if-not #'company-files--trailing-slash-p
                                           company-files-exclusions))
         (file-exclusions (cl-set-difference company-files-exclusions
                                             dir-exclusions)))
    (cl-loop for c in completions
             unless (if (company-files--trailing-slash-p c)
                        (member c dir-exclusions)
                      (cl-find-if (lambda (exclusion)
                                    (string-suffix-p exclusion c))
                                  file-exclusions))
             collect c)))

(defvar company-files--regexps
  (let* ((root (if (eq system-type 'windows-nt)
                   "[a-zA-Z]:/"
                 "/"))
         (begin (concat "\\(?:\\.\\{1,2\\}/\\|~/\\|" root "\\)")))
    (list (concat "\"\\(" begin "[^\"\n]*\\)")
          (concat "\'\\(" begin "[^\'\n]*\\)")
          (concat "\\(?:[ \t=\[]\\|^\\)\\(" begin "[^ \t\n]*\\)"))))

(defun company-files--grab-existing-name ()
  ;; Grab the file name.
  ;; When surrounded with quotes, it can include spaces.
  (let (file dir)
    (and (cl-dolist (regexp company-files--regexps)
           (when (setq file (company-grab-line regexp 1))
             (cl-return file)))
         (company-files--connected-p file)
         (setq dir (file-name-directory file))
         (not (string-match "//" dir))
         (file-exists-p dir)
         file)))

(defun company-files--connected-p (file)
  (or (not (file-remote-p file))
      (file-remote-p file nil t)))

(defun company-files--trailing-slash-p (file)
  ;; `file-directory-p' is very expensive on remotes. We are relying on
  ;; `file-name-all-completions' returning directories with trailing / instead.
  (let ((len (length file)))
    (and (> len 0) (eq (aref file (1- len)) ?/))))

(defvar company-files--completion-cache nil)

(defun company-files--complete (_prefix)
  (let* ((full-prefix (company-files--grab-existing-name))
         (dir (file-name-directory full-prefix))
         (file (file-name-nondirectory full-prefix))
         (key (list file
                    (expand-file-name dir)
                    (nth 5 (file-attributes dir))))
         (completion-ignore-case read-file-name-completion-ignore-case))
    (unless (or (company-file--keys-match-p key (car company-files--completion-cache))
                (not (company-files--connected-p dir)))
      (let* ((candidates (company-files--directory-files dir file))
             (directories (unless (file-remote-p dir)
                            (cl-remove-if-not (lambda (f)
                                                (company-files--trailing-slash-p f))
                                              candidates)))
             (children (and directories
                            (cl-mapcan (lambda (d)
                                         (company-files--directory-files d ""))
                                       directories))))
        (setq company-files--completion-cache
              (cons key (append candidates children)))))
    (all-completions file (cdr company-files--completion-cache))))

(defun company-files--prefix ()
  (let ((existing (company-files--grab-existing-name)))
    (when existing
      (list existing (company-grab-suffix "[^ '\"\t\n\r/]*/?")))))

(defun company-file--keys-match-p (new old)
  (and (equal (cdr old) (cdr new))
       (string-prefix-p (car old) (car new))))

(defun company-files--adjust-boundaries (_file prefix suffix)
  (cons
   (file-name-nondirectory prefix)
   suffix))

;;;###autoload
(defun company-files (command &optional arg &rest rest)
  "`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-files))
    (prefix (company-files--prefix))
    (candidates
     (company-files--complete arg))
    (adjust-boundaries
     (company-files--adjust-boundaries arg (nth 0 rest) (nth 1 rest)))
    (location (cons (dired-noselect
                     (file-name-directory (directory-file-name arg))) 1))
    (kind (if (string-suffix-p "/" arg) 'folder 'file))
    (sorted t)
    (no-cache t)))

(provide 'company-files)
;;; company-files.el ends here

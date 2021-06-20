;;; counsel-mu4e.el --- Search emails in Mu4e asynchronously with Ivy  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Sean Farley

;; Author: Sean Farley <sean@farley.io>
;; URL: https://github.com/seanfarley/counsel-mu4e
;; Keywords: mail
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (ivy "0.10.0") (mu4e "0.21"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides `counsel-mu4e' function which is inspired by
;; `counsel-notmuch'.
;; Simply call `counsel-mu4e' function and input your mu4e query.

;;; Code:
;;; License: GPLv3
;;
;;
;;; Code:
;;

(require 'counsel)
(require 'mu4e)
(require 'subr-x)

(defgroup counsel-mu4e nil
  "Options for counsel-mu4e."
  :group 'mu4e)

(defcustom counsel-mu4e-path "mu"
  "Path to mu4e executable."
  :type 'string
  :group 'counsel-mu4e)

(defcustom counsel-mu4e-flags "-n 500 --skip-dups --sortfield=date"
  "Path to mu4e executable."
  :type 'string
  :group 'counsel-mu4e)

(defface counsel-mu4e-date-face
  '((t :inherit mu4e-header-value-face :background nil))
  "Default face used in tree mode face for matching messages"
  :group 'counsel-mu4e)

(defface counsel-mu4e-count-face
  '((t :inherit mu4e-header-highlight-face :background nil))
  "Default face used in tree mode face for matching messages"
  :group 'counsel-mu4e)

(defface counsel-mu4e-people-face
  '((t :inherit mu4e-contact-face :background nil))
  "Default face used in tree mode face for matching messages"
  :group 'counsel-mu4e)

(defface counsel-mu4e-subject-face
  '((t :inherit mu4e-header-face :background nil))
  "Default face used in tree mode face for matching messages"
  :group 'counsel-mu4e)

(defvar counsel-mu4e-history nil
  "History for `counsel-mu4e'.")

;; the delimiter used in the --fields string only looks like two spaces; it
;; is actually U+205F (MEDIUM MATHEMATICAL SPACE) followed by U+2006
;; (SIX-PER-EM SPACE) in the hope that no one has those two characters in a
;; row in their subject
(defvar counsel-mu4e-delimiter "  "
  "Delimiter for fields in mu output.")

(defun counsel-mu4e-cmd (input)
  "Form mu4e query command using INPUT."
  (counsel-require-program counsel-mu4e-path)
  (format "%s find %s --fields 'i%sd%sf%ss' --nocolor %s"
          counsel-mu4e-path
          counsel-mu4e-flags
          counsel-mu4e-delimiter
          counsel-mu4e-delimiter
          counsel-mu4e-delimiter
          (shell-quote-argument input)))

(defun counsel-mu4e-function (input)
  "Get mail from mu4e using INPUT."
  (if (< (length input) 3)
      (ivy-more-chars)
    (counsel--async-command
     (counsel-mu4e-cmd input))
    '("" "working...")))

(defun counsel-mu4e-action-show (sexp)
  "Open resulting SEXP in ‘mu4e-show’ view."
  (mu4e-view-message-with-message-id
   (car (split-string sexp counsel-mu4e-delimiter))))

(defun counsel-mu4e--get-match-face (needle haystack)
  "Return the nth match face if NEEDLE is in HAYSTACK.
Otherwise return default face."
  (let ((tail (member needle haystack)))
    (if (not tail)
        'counsel-mu4e-subject-face
      (nth (1+ (mod (- (length haystack) (length tail))
                    (1- (length ivy-minibuffer-faces))))
           ivy-minibuffer-faces))))

(defun counsel-mu4e-transformer (str)
  "Transform STR to mu4e display style."
  (let ((fields (split-string str counsel-mu4e-delimiter)))
    (when (> (length fields) 1)
      (let* ((keys (mapcar (lambda (i) (downcase i)) (split-string ivy-text)))
             (subject (string-join (mapcar (lambda (i)
                                             (propertize i 'face
                                                         (counsel-mu4e--get-match-face
                                                          (downcase i) keys)))
                                           (split-string (nth 3 fields)))
                                   " "))
             (date (propertize (nth 1 fields) 'face 'counsel-mu4e-date-face))
             (people (propertize (nth 2 fields) 'face 'counsel-mu4e-people-face)))
        (format "%s\t%s\t%s" date people subject)))))

;;;###autoload
(defun counsel-mu4e (&optional initial-input)
  "Search for your email in mu4e with INITIAL-INPUT."
  (interactive)
  (ivy-read "mu4e search: " 'counsel-mu4e-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-mu4e-history
            :action #'counsel-mu4e-action-show
            :unwind #'counsel-delete-process
            :caller #'counsel-mu4e))

(ivy-set-display-transformer 'counsel-mu4e 'counsel-mu4e-transformer)

(provide 'counsel-mu4e)

;;; counsel-mu4e.el ends here

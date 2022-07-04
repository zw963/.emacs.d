;;; helm-all-the-icons.el --- Browse icons from all-the-icons package. -*- lexical-binding: t -*-

;; Author:      Thierry Volpiatto <thievol@posteo.net>
;; Copyright (C) 2021 Thierry Volpiatto <thievol@posteo.net>

;; Version: 1.0
;; URL: https://github.com/emacs-helm-helm-all-the-icons

;; Compatibility: GNU Emacs 24.3+
;; Package-Requires: ((emacs "24.3"))

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

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'all-the-icons)

(defvar helm-all-the-icons-alist '((all-the-icons-data/alltheicons-alist . all-the-icons-alltheicon)
                                   (all-the-icons-data/fa-icon-alist . all-the-icons-faicon)
                                   (all-the-icons-data/file-icon-alist . all-the-icons-fileicon)
                                   (all-the-icons-data/octicons-alist . all-the-icons-octicon)
                                   (all-the-icons-data/material-icons-alist . all-the-icons-material)
                                   (all-the-icons-data/weather-icons-alist . all-the-icons-wicon)))

(defun helm-all-the-icons-build-source (data fn)
  (let ((max-len (cl-loop for (s . _i) in (symbol-value data)
                          maximize (length s))))
    (helm-build-sync-source (replace-regexp-in-string
                             "-alist\\'" ""
                             (cadr (split-string (symbol-name data) "/")))
      :candidates (lambda ()
                    (cl-loop for (name . icon) in (symbol-value data)
                             for fmt-icon = (funcall fn name)
                             collect (cons (concat name
                                                   (make-string
                                                    (1+ (- max-len (length name))) ? )
                                                   (format "%s" fmt-icon))
                                           (cons name icon))))
      :action `(("insert icon" . (lambda (candidate)
                                   (let ((fmt-icon (funcall ',fn (car candidate))))
                                     (insert (format "%s" fmt-icon)))))
                ("insert code for icon" . (lambda (candidate)
                                            (insert (format "(%s \"%s\")" ',fn (car candidate)))))
                ("insert name" . (lambda (candidate)
                                   (insert (car candidate))))
                ("insert raw icon" . (lambda (candidate)
                                       (insert (cdr candidate))))
                ;; FIXME: yank is inserting the raw icon, not the display.
                ("kill icon" . (lambda (candidate)
                                 (let ((fmt-icon (funcall ',fn (car candidate))))
                                   (kill-new (format "%s" fmt-icon)))))))))

(defun helm-all-the-icons-sources ()
  (cl-loop for (db . fn) in helm-all-the-icons-alist
           collect (helm-all-the-icons-build-source db fn)))

;;;###autoload
(defun helm-all-the-icons ()
  (interactive)
  (require 'all-the-icons)
  (helm :sources (helm-all-the-icons-sources)
        :buffer "*helm all the icons*"
        :candidate-number-limit nil))

(provide 'helm-all-the-icons)

;;; helm-all-the-icons ends here

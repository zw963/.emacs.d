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

(defvar helm-all-the-icons-alist (mapcar (lambda (family)
                                           `(,family
                                             ,(intern-soft (format "all-the-icons-%s-data" family))
                                             ,(intern-soft (format "all-the-icons-%s" family))))
                                         all-the-icons-font-families))

(defvar helm-all-the-icons--cache (make-hash-table))
(defun helm-all-the-icons-build-source (family dfn ifn &optional reporter)
  "Build source for FAMILY using data fn DFN and insert fn IFN.
DFN is all-the-icons-<FAMILY>-data and IFN is all-the-icons-<FAMILY>
function."
  (let* ((data    (funcall dfn))
         (max-len (cl-loop for (s . _i) in data
                           maximize (length s))))
    (helm-build-sync-source (symbol-name family)
      :init (lambda ()
              (unless (gethash family helm-all-the-icons--cache)
                (puthash family
                         (cl-loop for (name . icon) in data
                                  for fmt-icon = (funcall ifn name)
                                  when reporter do (progress-reporter-update reporter)
                                  collect (cons (concat (substring-no-properties name)
                                                        (make-string
                                                         (1+ (- max-len (length name))) ? )
                                                        (format "%s" fmt-icon))
                                                (cons name icon)))
                         helm-all-the-icons--cache)))
      :candidates (lambda () (gethash family helm-all-the-icons--cache))
      :action `(("insert icon" . (lambda (candidate)
                                   (let ((fmt-icon (funcall ',ifn (car candidate))))
                                     (insert (format "%s" fmt-icon)))))
                ("insert code for icon" . (lambda (candidate)
                                            (insert (format "(%s \"%s\")" ',ifn (car candidate)))))
                ("insert name" . (lambda (candidate)
                                   (insert (car candidate))))
                ("insert raw icon" . (lambda (candidate)
                                       (insert (cdr candidate))))
                ;; FIXME: yank is inserting the raw icon, not the display.
                ("kill icon" . (lambda (candidate)
                                 (let ((fmt-icon (funcall ',ifn (car candidate))))
                                   (kill-new (format "%s" fmt-icon)))))))))

(defmacro helm-all-the-icons-with-progress (&rest body)
  `(let ((reporter (make-progress-reporter "Updating icons cache...")))
     (progn
       ,@body)))

(defun helm-all-the-icons-sources ()
  (cl-declare (special reporter))
  (helm-all-the-icons-with-progress
   (cl-loop for (family dfn fn) in helm-all-the-icons-alist
            collect (helm-all-the-icons-build-source family dfn fn reporter))))

;;;###autoload
(defun helm-all-the-icons (&optional refresh)
  (interactive "P")
  (require 'all-the-icons)
  (when refresh (clrhash helm-all-the-icons--cache))
  (helm :sources (helm-all-the-icons-sources)
        :buffer "*helm all the icons*"))

(provide 'helm-all-the-icons)

;;; helm-all-the-icons.el ends here

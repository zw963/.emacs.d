;; -*- lexical-binding: t; -*-

;;; iedit-rect.el --- visible rectangle editing support based on Iedit.

;; Copyright (C) 2010 - 2019, 2020 Victor Ren

;; Time-stamp: <2022-01-14 12:33:45 Victor Ren>
;; Author: Victor Ren <victorhge@gmail.com>
;; Keywords: occurrence region simultaneous rectangle refactoring
;; Version: 0.9.9.9.9
;; X-URL: https://github.com/victorhge/iedit
;;        https://www.emacswiki.org/emacs/Iedit
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x

;; This file is not part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package also provides rectangle support with *visible rectangle*
;; highlighting, which is similar with cua-mode rectangle support. But it is
;; lighter weight and uses iedit mechanisms.

;; The code was developed and fully tested on Gnu Emacs 24.0.93, partially
;; tested on Gnu Emacs 22. If you have any compatible problem, please let me
;; know.

;;; todo:
;; - Add restrict function back

;;; Code:

;; (eval-when-compile (require 'cl-lib))
(require 'rect) ;; kill-rectangle
(require 'iedit-lib)

(defvar-local iedit-rectangle-mode nil) ;; Name of the minor mode

(or (assq 'iedit-rectangle-mode minor-mode-alist)
    (nconc minor-mode-alist
           (list '(iedit-rectangle-mode iedit-rectangle-mode))))


;;; Default key bindings:
(when (null (where-is-internal 'iedit-rectangle-mode))
  (let ((key-def (lookup-key ctl-x-r-map  ";")))
    (if key-def
        (display-warning 'iedit (format "Iedit rect default key %S is occupied by %s."
                                        (key-description [C-x r RET])
                                        key-def)
                         :warning)
	  (define-key ctl-x-r-map  "\r" 'iedit-rectangle-mode)
      (define-key ctl-x-r-map  ";" 'iedit-rectangle-mode)
	  (define-key rectangle-mark-mode-map  ";" 'iedit-rectangle-mode)
      (message "Iedit-rect default key binding is %s" (key-description [C-x r \;])))))

(defvar-local iedit-rectangle nil
  "This buffer local variable which is the rectangle geometry if
current mode is iedit-rect. Otherwise it is nil.
\(car iedit-rectangle) is the top-left corner and
\(cadr iedit-rectangle) is the bottom-right corner" )

;;; Define Iedit rect mode map
(defvar iedit-rect-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map iedit-occurrence-keymap-default)
    (define-key map (kbd "M-K") 'iedit-kill-rectangle)
	(define-key map (kbd "C-;") 'iedit-rectangle-mode) ; to exit iedit-rect mode
    map)
  "Keymap used within overlays in Iedit-rect mode.")

(or (assq 'iedit-rectangle-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'iedit-rectangle-mode iedit-lib-keymap) minor-mode-map-alist)))


;; Avoid to restore Iedit-rect mode when restoring desktop
(add-to-list 'desktop-minor-mode-handlers
             '(iedit-rectangle-mode . nil))

;;;###autoload
(defun iedit-rectangle-mode (&optional beg end)
  "Toggle Iedit-rect mode.

When Iedit-rect mode is on, a rectangle is started with visible
rectangle highlighting.  Rectangle editing support is based on
Iedit mechanism.

Commands:
\\{iedit-rect-keymap}"
  (interactive (when (iedit-region-active)
                 (list (region-beginning)
                       (region-end))))
  ;; enforce skip modification once, errors may happen to cause this to be
  ;; unset.
  (setq iedit-skip-modification-once t)
  (if iedit-rectangle-mode
      (iedit-rectangle-done)
    (iedit-barf-if-lib-active)
    (if (and beg end)
        (progn
          (iedit-rectangle-start beg end))
      (error "no region available."))))

(defun iedit-rectangle-line (startcol endcol)
  (push (iedit-make-occurrence-overlay
         (progn
           (move-to-column startcol t)
           (point))
         (progn
           (move-to-column endcol t)
           (point)))
        iedit-occurrences-overlays))

(defun iedit-rectangle-start (beg end)
  "Start Iedit mode for the region as a rectangle."
  (barf-if-buffer-read-only)
  (setq iedit-rectangle (list (copy-marker beg) (copy-marker end t)))
  (setq iedit-occurrences-overlays nil)
  (setq iedit-occurrence-keymap iedit-rect-keymap)
  (goto-char
   (apply-on-rectangle 'iedit-rectangle-line beg end))
  (setq mark-active nil)
  (run-hooks 'deactivate-mark-hook)
  (setq iedit-rectangle-mode
        (propertize
         (concat " Iedit-rect:"
                 (number-to-string (length iedit-occurrences-overlays)))
         'face
         'font-lock-warning-face))
  (iedit-lib-start 'iedit-rectangle-done)
  (force-mode-line-update))

(defun iedit-rectangle-done ()
  "Exit Iedit mode.
Save the current occurrence string locally and globally.  Save
the initial string globally."
  (iedit-lib-cleanup)
  (setq iedit-rectangle-mode nil)
  (force-mode-line-update))

(defun iedit-kill-rectangle(&optional fill)
  "Kill the rectangle.
The behavior is the same as `kill-rectangle' in rect mode."
  (interactive "*P")
  (or (and iedit-rectangle (iedit-same-column))
      (error "Not a rectangle"))
  (let ((inhibit-modification-hooks t))
    (kill-rectangle (marker-position (car iedit-rectangle))
                    (marker-position (cadr iedit-rectangle)) fill)))

(provide 'iedit-rect)

;;; iedit-rect.el ends here

;;  LocalWords:  iedit el MERCHANTABILITY kbd isearch todo ert Lindberg Tassilo
;;  LocalWords:  eval rect defgroup defcustom boolean defvar assq alist nconc
;;  LocalWords:  substring cadr keymap defconst purecopy bkm defun princ prev
;;  LocalWords:  iso lefttab backtab upcase downcase concat setq autoload arg
;;  LocalWords:  refactoring propertize cond goto nreverse progn rotatef eq elp
;;  LocalWords:  dolist pos unmatch args ov sReplace iedit's cdr quote'ed Ren
;;  LocalWords:  cua ctl RET

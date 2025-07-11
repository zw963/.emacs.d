;; -*- lexical-binding: t; -*-

;;; company-box-doc.el --- Company front-end  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/company-box

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Display candidate's documentation in another frame

;;; Code:

(require 'dash)
(require 'company)
(require 'frame-local)
(require 'cl-macs)
(require 'subr-x)

(defgroup company-box-doc nil
  "Display documentation popups alongside company-box"
  :group 'company)

(defcustom company-box-doc-enable t
  "Enable company-box-doc by default."
  :type 'boolean
  :safe #'booleanp
  :group 'company-box-doc)

(defcustom company-box-doc-delay 0.5
  "The number of seconds to wait before displaying the popup."
  :type 'number
  :group 'company-box-doc)

(defcustom company-box-doc-no-wrap nil
  "Specify whether or not to wrap the documentation box at the edge of
 the Emacs frame."
  :type 'boolean
  :group 'company-box-doc)

(defvar company-box-doc-frame-parameters
  '((internal-border-width . 10))
  "Frame parameters to use on the doc frame.
`company-box-frame-parameters' is then append to this variable.")

(declare-function company-box--get-frame 'company-box)
(declare-function company-box--set-frame 'company-box)
(declare-function company-box--get-buffer 'company-box)
(declare-function company-box--make-frame 'company-box)

(defvar company-box-frame-parameters)
(defvar company-box--bottom)
(defvar company-box-scrollbar)

(defvar-local company-box-doc--timer nil)

(defun company-box-doc--fetch-doc-buffer (candidate)
  (let ((inhibit-message t) (message-log-max nil))
    (--> (while-no-input
           (-some-> (company-call-backend 'doc-buffer candidate)
             (get-buffer)))
         (if (eq t it) nil it))))

(defun company-box-doc--set-frame-position (frame)
  (-let* ((box-position (frame-position (company-box--get-frame)))
          (box-width (frame-pixel-width (company-box--get-frame)))
          (window (frame-root-window frame))
          (frame-resize-pixelwise t)
          ((width . height)
           (if company-box-doc-no-wrap
               (window-text-pixel-size window nil nil 10000 10000)
             (window-text-pixel-size
              window nil nil
              ;; Use the widest space available (left or right of the box frame)
              (let ((space-right (- (frame-native-width) (+ 40 (car box-position) box-width)))
                    (space-left (- (car box-position) 40)))
                (if (< space-right space-left) space-left space-right))
              (- (frame-native-height) 40))))
          (bottom (+ company-box--bottom (window-pixel-top) (frame-border-width)))
          (x (+ (car box-position) box-width (/ (frame-char-width) 2)))
          (y (cdr box-position))
          (y (if (> (+ y height 20) bottom)
                 (- y (- (+ y height) bottom) 20)
               y))
          (space-right (- (frame-pixel-width) x))
          (space-left (car box-position))
          (x (or (let ((border (* (or (alist-get 'internal-border-width company-box-doc-frame-parameters) 0)
                                  2)))
                   (and (> width space-right)
                        (> space-left (+ width border (/ (frame-char-width) 2)))
                        (- (car box-position) width border (/ (frame-char-width) 2))))
                 x)))
    (set-frame-position frame (max x 0) (max y 10))
    (set-frame-size frame width height t)))

(defun company-box-doc--make-buffer (object)
  (let* ((buffer-list-update-hook nil)
         (inhibit-modification-hooks t)
         (string (cond ((stringp object) object)
                       ((bufferp object) (with-current-buffer object (buffer-string))))))
    (when (and string (> (length (string-trim string)) 0))
      (with-current-buffer (company-box--get-buffer "doc")
        (erase-buffer)
        (insert string)
        (setq mode-line-format nil
              display-line-numbers nil
              header-line-format nil
              show-trailing-whitespace nil
              truncate-lines nil
              cursor-in-non-selected-windows nil)
        (current-buffer)))))

(defun company-box-doc--make-frame (buffer)
  (let* ((company-box-frame-parameters
          (append company-box-doc-frame-parameters
                  company-box-frame-parameters))
         (frame (company-box--make-frame buffer)))
    ;; (set-face-background 'internal-border "white" frame)
    (set-frame-parameter frame 'name "")
    frame))

(defun company-box-doc--show (selection frame)
  (cl-letf (((symbol-function 'completing-read) #'company-box-completing-read)
            (window-configuration-change-hook nil)
            (inhibit-redisplay t)
            (display-buffer-alist nil)
            (buffer-list-update-hook nil))
    (-when-let* ((valid-state (and (eq (selected-frame) frame)
                                   company-box--bottom
                                   company-selection
                                   (company-box--get-frame)
                                   (frame-visible-p (company-box--get-frame))))
                 (candidate (nth selection company-candidates))
                 (doc (or (company-call-backend 'quickhelp-string candidate)
                          (company-box-doc--fetch-doc-buffer candidate)))
                 (doc (company-box-doc--make-buffer doc)))
      (unless (frame-live-p (frame-local-getq company-box-doc-frame))
        (frame-local-setq company-box-doc-frame (company-box-doc--make-frame doc)))
      (company-box-doc--set-frame-position (frame-local-getq company-box-doc-frame))
      (unless (frame-visible-p (frame-local-getq company-box-doc-frame))
        (make-frame-visible (frame-local-getq company-box-doc-frame))))))

(defun company-box-completing-read (_prompt candidates &rest _)
  "`cider', and probably other libraries, prompt the user to
resolve ambiguous documentation requests.  Instead of failing we
just grab the first candidate and press forward."
  (car candidates))

(defun company-box-doc (selection frame)
  (when company-box-doc-enable
    (company-box-doc--hide frame)
    (when (timerp company-box-doc--timer)
      (cancel-timer company-box-doc--timer))
    (setq company-box-doc--timer
          (run-with-timer
           company-box-doc-delay nil
           (lambda nil
             (company-box-doc--show selection frame)
             (company-ensure-emulation-alist))))))

(defun company-box-doc--hide (frame)
  (let ((frame (frame-local-getq company-box-doc-frame frame)))
    (and (frame-live-p frame)
         (make-frame-invisible frame))))

(defun company-box-doc--delete-frame ()
  "Delete the child frame if it exists."
  (-when-let (frame (frame-local-getq company-box-doc-frame))
    (and (frame-live-p frame)
         (delete-frame frame))
    (frame-local-setq company-box-doc-frame nil)))

(defun company-box-doc-manually ()
  (interactive)
  (company-box-doc--show company-selection (or (frame-parent) (selected-frame))))

(define-key company-active-map [remap company-show-doc-buffer] 'company-box-doc-manually)

(provide 'company-box-doc)
;;; company-box-doc.el ends here

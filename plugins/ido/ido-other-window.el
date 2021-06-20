;;; ido-other-window.el --- Invoke any command using ido in other window

;; Author: Zak B. Elep
;; Version: 0.1.0
;; Created: 2016-03-16
;; Keywords: convenience, completion, ido, window
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This little extension to `ido-mode' makes window commands such as
;; `ido-find-file-other-window', `ido-switch-buffer-other-window', and
;; others obsolete.  It follows behavior similar to `helm-mode' window
;; actions upon selection.

;; This is adapted from the original work by Tim Harper
;; (https://gist.github.com/timcharper/493269) to turn it into a
;; shippable Emacs package, as well as make it a bit more smarter when
;; doing sensible window splits...

;;; Code:

(require 'ido)

(defun split-window-sensibly-and-switch (window)
  "Call `split-window-sensibly' and switch to other WINDOW.
If `split-window-preferred-function' is set, try to use that first."
  (interactive)
  (condition-case nil
      (funcall split-window-preferred-function window)
    (split-window-sensibly window))
  (other-window 1))

(defun split-window-vertically-and-switch ()
  "Call `split-window-vertically' and switch to other window."
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun split-window-horizontally-and-switch ()
  "Call `split-window-horizontally' and switch to other window."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defvar ido-exit-minibuffer-target-window nil
  "Target window to jump to after Ido does its thing.")

;;;###autoload
(defun ido-invoke-in-other-window ()
  "Signal Ido to switch to (or create) another window after exiting."
  (interactive)
  (setq ido-exit-minibuffer-target-window 'other)
  (ido-exit-minibuffer))

;;;###autoload
(defun ido-invoke-in-horizontal-split ()
  "Signal Ido to split horizontally and switch after exiting."
  (interactive)
  (setq ido-exit-minibuffer-target-window 'horizontal)
  (ido-exit-minibuffer))

;;;###autoload
(defun ido-invoke-in-vertical-split ()
  "Signal Ido to split vertically and switch after exiting."
  (interactive)
  (setq ido-exit-minibuffer-target-window 'vertical)
  (ido-exit-minibuffer))

;;;###autoload
(defun ido-invoke-in-new-frame ()
  "Signal Ido to create a new frame after exiting."
  (interactive)
  (setq ido-exit-minibuffer-target-window 'frame)
  (ido-exit-minibuffer))

(defadvice ido-read-internal
    (around ido-read-internal-with-minibuffer-other-window activate)
  "Advice `ido-read-internal' on what target window to switch to."
  (let* (ido-exit-minibuffer-target-window
	 (this-buffer (current-buffer))
	 (result ad-do-it))
    (cond
     ((equal ido-exit-minibuffer-target-window 'other)
      (if (= 1 (count-windows))
          (split-window-sensibly-and-switch (selected-window))
        (other-window 1)))
     ((equal ido-exit-minibuffer-target-window 'horizontal)
      (split-window-horizontally-and-switch))
     ((equal ido-exit-minibuffer-target-window 'vertical)
      (split-window-vertically-and-switch))
     ((equal ido-exit-minibuffer-target-window 'frame)
      (select-frame (make-frame))))

    ;; why? Some ido commands, such as textmate.el's
    ;; textmate-goto-symbol don't switch the current buffer
    (switch-to-buffer this-buffer)
    result))

(defadvice ido-setup-completion-map
    (after ido-setup-completion-map-with-other-window-keys activate)
  "Advice `ido-setup-completion-map' on keys to open in other windows."
  (mapc (lambda (map)
          (define-key map (kbd "C-o") 'ido-invoke-in-other-window)
          (define-key map (kbd "C-2") 'ido-invoke-in-vertical-split)
          (define-key map (kbd "C-3") 'ido-invoke-in-horizontal-split)
          (define-key map (kbd "C-4") 'ido-invoke-in-other-window)
          (define-key map (kbd "C-5") 'ido-invoke-in-new-frame))
        (list ido-buffer-completion-map
              ido-common-completion-map
              ido-file-completion-map
              ido-file-dir-completion-map)))

(provide 'ido-other-window)

;;; ido-other-window.el ends here

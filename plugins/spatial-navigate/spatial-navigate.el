;; -*- lexical-binding: t; -*-

;;; spatial-navigate.el --- Directional navigation between white-space blocks -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2020  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-spatial-navigate
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Support jumping horizontally & vertically
;; across blocks of white-space or non-white-space.

;;; Usage


;; ;; This shows how using Alt-Arrow Keys can be used to set directional navigation.
;; (global-set-key (kbd "<M-up>") 'spatial-navigate-backward-vertical-bar)
;; (global-set-key (kbd "<M-down>") 'spatial-navigate-forward-vertical-bar)
;; (global-set-key (kbd "<M-left>") 'spatial-navigate-backward-horizontal-bar)
;; (global-set-key (kbd "<M-right>") 'spatial-navigate-forward-horizontal-bar)

;; ;; If you use evil-mode, the 'box' navigation functions make sense in normal mode,
;; ;; the 'bar' functions make most sense in insert mode.
;;
;; (define-key evil-normal-state-map (kbd "M-k") 'spatial-navigate-backward-vertical-box)
;; (define-key evil-normal-state-map (kbd "M-j") 'spatial-navigate-forward-vertical-box)
;; (define-key evil-normal-state-map (kbd "M-h") 'spatial-navigate-backward-horizontal-box)
;; (define-key evil-normal-state-map (kbd "M-l") 'spatial-navigate-forward-horizontal-box)
;; (define-key evil-insert-state-map (kbd "M-k") 'spatial-navigate-backward-vertical-bar)
;; (define-key evil-insert-state-map (kbd "M-j") 'spatial-navigate-forward-vertical-bar)
;; (define-key evil-insert-state-map (kbd "M-h") 'spatial-navigate-backward-horizontal-bar)
;; (define-key evil-insert-state-map (kbd "M-l") 'spatial-navigate-forward-horizontal-bar)

;;; Code:


;; ---------------------------------------------------------------------------
;; Custom Variables

(defcustom spatial-navigate-wrap-horizontal-motion nil
  "Skip blank lines when horizontal motion reaches line bounds."
  :group 'spatial-navigate
  :type 'boolean)


;; ---------------------------------------------------------------------------
;; Generic Functions

(defun spatial-navigate--evil-visual-mode-workaround (state)
  "Workaround for evil-visual line mode, STATE must be \\'pre or \\'post."
  (declare (important-return-value nil))
  (when (and (fboundp 'evil-visual-state-p)
             (funcall #'evil-visual-state-p)
             (fboundp 'evil-visual-type)
             (eq (funcall #'evil-visual-type) 'line)
             (boundp 'evil-visual-point))
    (let ((mark (symbol-value 'evil-visual-point)))
      (when (markerp mark)
        (cond
         ;; Without this, `point' will be at the beginning of the line
         ;; (from the pre command hook).
         ((eq state 'pre)
          (goto-char (marker-position mark)))
         ;; Without this, the `point' wont move.
         ;; See: https://github.com/emacs-evil/evil/issues/1708
         ((eq state 'post)
          (set-marker mark (point)))
         (t
          (error "Invalid input, internal error")))))))


;; ---------------------------------------------------------------------------
;; Private Functions

(defun spatial-navigate--vertical-calc (dir is-block-cursor)
  "Calculate the next/previous vertical position based on DIR (-1 or 1).

Argument IS-BLOCK-CURSOR causes the cursor to detect white-space using
characters before and after the current cursor, this behaves in a way that
is logical for a block cursor)."
  (declare (important-return-value t))
  (spatial-navigate--evil-visual-mode-workaround 'pre)

  (let ((result nil)
        (result-fallback (cons 0 (point)))
        (lines 0)
        (lines-prev 0)
        (is-first t)
        (is-empty-state nil)
        (pos-prev (point))
        (col-init (current-column))
        (is-fill-fn
         (lambda (pos beg end default)
           (cond
            ((and (>= pos beg) (< pos end))
             (let ((ch (char-after pos)))
               (not (memq ch (list ?\s ?\t)))))
            (t
             default)))))
    (while (null result)

      ;; Forward line and move to column.
      (forward-line dir)
      (setq lines (+ dir lines))

      (let* ((col (move-to-column col-init))
             (is-empty
              (or (< col col-init)
                  ;; End of the line is also considered empty.
                  (and (or (zerop col-init) is-block-cursor) (eolp))

                  ;; Do this so we don't delimit on spaces between words.
                  ;; Surrounded by spaces before and after.
                  (let* ((pos-eol (pos-eol))
                         (pos-bol (pos-bol))

                         (is-fill-curr (funcall is-fill-fn (point) pos-bol pos-eol nil))
                         (is-fill-prev
                          (funcall is-fill-fn (+ (point) 1) pos-bol pos-eol is-fill-curr))
                         (is-fill-next
                          (funcall is-fill-fn (- (point) 1) pos-bol pos-eol is-fill-curr)))

                    (cond
                     ;; Check 3 characters, current char, before & after.
                     ;; If there are two blanks before or after, this is considered not filled.
                     (is-block-cursor
                      (not (or is-fill-curr (and is-fill-prev is-fill-next))))

                     ;; Check only 2 characters.
                     (t
                      (not (or is-fill-curr is-fill-prev))))))))

        ;; Keep searching for whatever we encounter first.
        (when is-first
          (setq is-empty-state is-empty)
          (setq is-first nil))

        ;; Either set the result, or continue looping.
        (cond
         ((not (eq is-empty is-empty-state))
          ;; We have hit a different state, stop!
          (setq result
                (cond
                 (is-empty-state
                  (cons lines (point)))
                 (t
                  (cons lines-prev pos-prev)))))
         ((eq pos-prev (point))
          ;; Beginning or end, don't hang!
          ;; Use the last valid state.
          (setq result result-fallback))
         (t ; Keep looping.
          ;; If we reach the beginning or end of the document,
          ;; use the last time we reached a valid column.
          (when (eq col col-init)
            (setq result-fallback (cons lines (point))))
          (setq lines-prev lines)
          (setq pos-prev (point))))))
    result))


(defun spatial-navigate--horizontal-calc (dir is-block-cursor)
  "Calculate the next/previous vertical position based on DIR (-1 or 1).

Argument IS-BLOCK-CURSOR causes the cursor to detect white-space using
characters before and after the current cursor, this behaves in a way that
is logical for a block cursor)."
  (declare (important-return-value t))
  (spatial-navigate--evil-visual-mode-workaround 'pre)

  (let ((result nil)
        (is-first t)
        (is-empty-state nil)
        (pos-prev (point))

        (pos-eol (pos-eol))
        (pos-bol (pos-bol))

        (is-fill-fn
         (lambda (pos beg end default)
           (cond
            ((and (>= pos beg) (< pos end))
             (let ((ch (char-after pos)))
               (not (memq ch (list ?\s ?\t)))))
            (t
             default)))))

    ;; This is needed once at the start, unlike line stepping.
    (when (cond
           ((< dir 0)
            (> pos-prev pos-bol))
           (t
            (<= pos-prev pos-eol)))
      (forward-char dir))

    (while (null result)
      (let ((is-empty
             ;; Do this so we don't delimit on spaces between words.
             ;; Surrounded by spaces before and after.
             (let* ((is-fill-curr (funcall is-fill-fn (point) pos-bol pos-eol nil))
                    (is-fill-prev (funcall is-fill-fn (+ (point) 1) pos-bol pos-eol is-fill-curr))
                    (is-fill-next (funcall is-fill-fn (- (point) 1) pos-bol pos-eol is-fill-curr)))
               (not (or is-fill-curr (and is-fill-prev is-fill-next))))))

        ;; Keep searching for whatever we encounter first.
        (when is-first
          (setq is-empty-state is-empty)
          (setq is-first nil))

        ;; Either set the result, or continue looping.
        (cond
         ((not (eq is-empty is-empty-state))
          ;; We have hit a different state, stop!
          (setq result
                (cond
                 (is-block-cursor
                  (cond
                   (is-empty-state
                    (point))
                   (t
                    pos-prev)))
                 (t
                  (cond
                   ((> dir 0)
                    (point))
                   (t
                    pos-prev))))))
         ((eq pos-prev (point))
          ;; Beginning or end, don't hang!
          ;; Use the last valid state.
          (setq result (point)))
         ;; If we get out of range, use last usable point.
         ((cond
           ((< dir 0)
            (< (point) pos-bol))
           (t
            (>= (point) pos-eol)))
          ;; Beginning or end, don't hang!
          ;; Use the last valid state.
          (setq result pos-prev))
         (t ; Keep looping.
          ;; If we reach the beginning or end of the document, we may need to use this.
          (setq pos-prev (point))
          (when (cond
                 ((< dir 0)
                  (> pos-prev pos-bol))
                 (t
                  (<= pos-prev pos-eol)))
            ;; Forward character.
            (forward-char dir))))))
    result))


;; ---------------------------------------------------------------------------
;; Wrapper Functions

(defun spatial-navigate--vertical (dir is-block-cursor)
  "See `spatial-navigate--vertical-calc' for docs on DIR and IS-BLOCK-CURSOR."
  (declare (important-return-value nil))
  (pcase-let ((`(,lines . ,pos-next)
               (save-excursion (spatial-navigate--vertical-calc dir is-block-cursor))))
    (when (zerop lines)
      (user-error "Spatial-navigate: no lines to jump to!"))

    (goto-char pos-next)

    (spatial-navigate--evil-visual-mode-workaround 'post)))

(defun spatial-navigate--horizontal (dir is-block-cursor)
  "See `spatial-navigate--horizontal-calc' for docs on DIR and IS-BLOCK-CURSOR."
  (declare (important-return-value nil))
  (let ((pos-next (save-excursion (spatial-navigate--horizontal-calc dir is-block-cursor))))

    ;; Optionally skip over blank lines.
    (when spatial-navigate-wrap-horizontal-motion
      (when (zerop (- pos-next (point)))
        (save-excursion
          (when (zerop (forward-line dir))
            ;; Skip blank lines.
            (while (and (looking-at-p "[[:blank:]]*$") (zerop (forward-line dir))))
            (setq pos-next
                  (cond
                   ((< dir 0)
                    (pos-eol))
                   (t
                    (pos-bol))))))))

    (when (zerop (- pos-next (point)))
      (user-error "Spatial-navigate: line limit reached!"))

    (goto-char pos-next)

    (spatial-navigate--evil-visual-mode-workaround 'post)))


;; ---------------------------------------------------------------------------
;; Public Functions

;; Vertical motion.

;;;###autoload
(defun spatial-navigate-forward-vertical-box ()
  "Jump forward vertically across white-space and non-white-space.

Use for a box cursor."
  (declare (important-return-value nil))
  (interactive)
  (spatial-navigate--vertical 1 t))

;;;###autoload
(defun spatial-navigate-backward-vertical-box ()
  "Jump backward vertically across white-space and non-white-space.

Use for a box cursor."
  (declare (important-return-value nil))
  (interactive)
  (spatial-navigate--vertical -1 t))


;;;###autoload
(defun spatial-navigate-forward-vertical-bar ()
  "Jump forward vertically across white-space and non-white-space.

Use for a bar cursor."
  (declare (important-return-value nil))
  (interactive)
  (spatial-navigate--vertical 1 nil))

;;;###autoload
(defun spatial-navigate-backward-vertical-bar ()
  "Jump backward vertically across white-space and non-white-space.

Use for a bar cursor."
  (declare (important-return-value nil))
  (interactive)
  (spatial-navigate--vertical -1 nil))

;; Horizontal motion.

;;;###autoload
(defun spatial-navigate-forward-horizontal-box ()
  "Jump forward horizontal across white-space and non-white-space.

Use for a box cursor."
  (declare (important-return-value nil))
  (interactive)
  (spatial-navigate--horizontal 1 t))

;;;###autoload
(defun spatial-navigate-backward-horizontal-box ()
  "Jump backward horizontal across white-space and non-white-space.

Use for a box cursor."
  (declare (important-return-value nil))
  (interactive)
  (spatial-navigate--horizontal -1 t))

;;;###autoload
(defun spatial-navigate-forward-horizontal-bar ()
  "Jump forward horizontal across white-space and non-white-space.

Use for a bar cursor."
  (declare (important-return-value nil))
  (interactive)
  (spatial-navigate--horizontal 1 nil))

;;;###autoload
(defun spatial-navigate-backward-horizontal-bar ()
  "Jump backward horizontal across white-space and non-white-space.

Use for a bar cursor."
  (declare (important-return-value nil))
  (interactive)
  (spatial-navigate--horizontal -1 nil))

(provide 'spatial-navigate)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; spatial-navigate.el ends here

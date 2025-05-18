;; -*- lexical-binding: t; -*-

(defun go-to-line-and-column-cond (lc-cond)
  "Allow a specification of LINE:COLUMN or LINE,COLUMN instead of just COLUMN.
Just :COLUMN or ,COLUMN moves to the specified column on the current line.
LINE alone still moves to the beginning of the specified line (like LINE:0 or LINE,0).
By Default I'm bind it to M-g M-l.
The default value of the COLUMN is decrement by -1
because all compilers consider the number of COLUMN from 1 (just for copy-past)"
  (interactive "sLine:Column:: ")
  (let (line delim column max-lines)
    (setq max-lines (count-lines (point-min) (point-max)))
    (save-match-data
      (string-match "^\\([0-9]*\\)\\([,:]?\\)\\([0-9]*\\)$" lc-cond)
      (setq line (string-to-number (match-string 1 lc-cond)))
      (setq delim (match-string 2 lc-cond))
      (setq column (string-to-number (match-string 3 lc-cond)))
      (if (= 0 line) (setq line (line-number-at-pos)))
      (if (> line max-lines) (setq line max-lines))
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column column)
      (message "Marker set to line %d column %s" (line-number-at-pos) (current-column))
      )))

(defun avy-goto-line-fixed (&optional arg)
  "Jump to a line start in current buffer.

When ARG is 1, jump to lines currently visible, with the option
to cancel to `goto-line' by entering a number.

When ARG is 4, negate the window scope determined by
`avy-all-windows'.

Otherwise, forward to `goto-line' with ARG."
  (interactive "p")
  (setq arg (or arg 1))
  (if (not (memq arg '(1 4)))
      (progn
        (goto-char (point-min))
        (forward-line (1- arg)))
    (avy-with avy-goto-line
      (let* ((avy-handler-old avy-handler-function)
             (avy-handler-function
              (lambda (char)
                (if (or (< char ?0)
                        (> char ?9))
                    (funcall avy-handler-old char)
                  (let ((line (read-from-minibuffer
                               "Goto line: " (string char))))
                    (when line
                      (avy-push-mark)
                      (save-restriction
                        (widen)
                        ;; (goto-char (point-min))
                        ;; (forward-line (1- (string-to-number line)))
                        (go-to-line-and-column-cond line)
                        )
                      (throw 'done 'exit))))))
             (r (avy--line (eq arg 4))))
        (when (and (not (memq r '(t nil))) (eq avy-action #'identity))
          (avy-action-goto r))))))

;; (global-set-key [remap goto-line] 'avy-goto-line-fixed)

(require 'goto-line-preview)
(global-set-key [remap goto-line] 'goto-line-preview)


(provide 'goto-line_init)

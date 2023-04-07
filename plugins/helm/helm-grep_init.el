(require 'helm-grep)

(defun helm-quit-and-do-git-grep-on-project ()
  "Drop into `helm-grep-do-git-grep' on entire project from `helm'."
  (interactive)
  (with-helm-alive-p
    (helm-run-after-exit #'helm-grep-do-git-grep-hacked t)))

(defun helm-git-grep-get-input-symbol ()
  "Get input symbol."
  (if (not mark-active)
      (thing-at-point 'symbol)
    (when (use-region-p)
      (buffer-substring (region-beginning) (region-end)))))

(defun helm-grep-do-git-grep-hacked (arg)
  "Preconfigured `helm' for git-grepping `default-directory'.
With a prefix arg ARG git-grep the whole repository."
  (interactive "P")
  (require 'helm-files)
  (helm-grep-git-1 default-directory arg nil (helm-git-grep-get-input-symbol)))

(global-set-key (kbd "M-r") 'helm-grep-do-git-grep-hacked)
(define-key helm-grep-map (kbd "M-r") 'helm-quit-and-do-git-grep-on-project)
(define-key helm-find-files-map (kbd "M-r") 'helm-ff-run-git-grep)

;; If you want grep base on multi-files, use M-SPC mark files, then C-s
(define-key helm-map (kbd "M-SPC") 'helm-toggle-visible-mark-forward)
(define-key helm-find-files-map (kbd "C-s") 'helm-ff-run-grep-ag)

(add-hook 'helm-grep-mode-hook 'wgrep-change-to-wgrep-mode 100)

;; hack for helm-grep support wgrep
;; (defun helm-grep-save-results-1 ()
;;   "Save Helm grep result in a `helm-grep-mode' buffer."
;;   (let* ((buf "*hgrep*")
;;          new-buf
;;          (pattern (with-helm-buffer helm-input-local))
;;          (src (helm-get-current-source))
;;          (src-name (assoc-default 'name src)))
;;     (with-current-buffer (get-buffer-create buf)
;;       (setq default-directory (or helm-ff-default-directory
;;                                   (helm-default-directory)
;;                                   default-directory))
;;       (setq-local helm-grep-mode-use-pcre (helm-get-attr 'pcre src))
;;       (setq buffer-read-only t)
;;       (let ((inhibit-read-only t)
;;             (map (make-sparse-keymap)))
;;         (erase-buffer)
;;         (insert "-*- mode: helm-grep -*-\n\n"
;;                 (format "%s Results for `%s':\n\n" src-name pattern))
;;         (save-excursion
;;           (insert (with-current-buffer helm-buffer
;;                     (goto-char (point-min)) (forward-line 1)
;;                     (buffer-substring (point) (point-max)))))
;;         (save-excursion
;;           (while (not (eobp))
;;             (add-text-properties (point-at-bol) (point-at-eol)
;;                                  `(keymap ,map
;;                                           help-echo ,(concat
;;                                                       (get-text-property
;;                                                        (point) 'helm-grep-fname)
;;                                                       "\nmouse-1: set point\nmouse-2: jump to selection")
;;                                           mouse-face highlight))
;;             (define-key map [mouse-1] 'mouse-set-point)
;;             (define-key map [mouse-2] 'helm-grep-mode-mouse-jump)
;;             (define-key map [mouse-3] 'ignore)
;;             (forward-line 1))))
;;       (helm-grep-mode)
;;       (if (fboundp 'wgrep-change-to-wgrep-mode)
;;           (wgrep-change-to-wgrep-mode)))
;;     (pop-to-buffer buf)
;;     (message "Helm %s Results saved in `%s' buffer" src-name buf)))

(provide 'helm-grep_init)

;;; helm-grep_init.el ends here

(require 'web-mode)
;; (require 'auto-rename-tag)
(require 'instant-rename-tag)

(require 'lsp-mode_init)

(defun zw/web-mode-common-hooks ()
  (local-set-key [(control c) (return)] 'save-buffer-and-browse-url)
  (local-set-key [(control c) (?\r)] 'save-buffer-and-browse-url)
  (local-set-key [(control c)(control c)] 'web-mode-buffer-indent)
  (local-set-key [(control meta f)] 'rhtml-mode-forward-sexp)
  (local-set-key [(control meta b)] 'rhtml-mode-backward-sexp)
  (local-set-key [(control meta ?\s)] 'rhtml-mark-sexp-tag)
  (local-set-key [(control tab)] 'web-mode-element-children-fold-or-unfold)
  (local-set-key [(meta return)] 'html-mode-newline-and-indent)
  (local-set-key [(control \;)] 'instant-rename-tag)
  (auto-rename-tag-mode)
  (setq-local fill-column 120)
  (setq-local company-minimum-prefix-length 1)
  (lsp-deferred)
  )

(setq
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-style-padding 2
 web-mode-script-padding 2
 web-mode-enable-block-face t
 web-mode-enable-part-face t

 web-mode-enable-current-element-highlight t
 web-mode-enable-current-column-highlight t

 web-mode-ac-sources-alist '(("css" . (ac-source-css-property))
                             ("html" . (ac-source-words-in-buffer ac-source-abbrev)))
 )

(setq web-mode-syntax-table
      (let ((table (make-syntax-table)))
        (modify-syntax-entry ?_ "_" table)
        (modify-syntax-entry ?= "." table)
        (modify-syntax-entry ?< "." table)
        (modify-syntax-entry ?> "." table)
        (modify-syntax-entry ?& "." table)
        (modify-syntax-entry ?/ "." table)
        (modify-syntax-entry ?% "." table)
        (modify-syntax-entry ?' "\"" table)
        table)
      )
(set-syntax-table web-mode-syntax-table)

(add-hook 'web-mode-hook 'zw/web-mode-common-hooks)

(defun save-buffer-and-browse-url ()
  "Save buffer and browse url."
  (interactive)
  (basic-save-buffer-1)
  (browse-url-of-buffer)
  )

(defun web-mode-buffer-indent ()
  "Indent all buffer."
  (interactive)
  (let ((debug t) (ts (current-time)) (sub nil))
    (indent-region (point-min) (point-max))
    (when debug
      (setq sub (time-subtract (current-time) ts))
      (message "buffer-indent: time elapsed = %Ss %9Sµs" (nth 1 sub) (nth 2 sub)))
    (delete-trailing-whitespace)))

(defun forward-erb-tag ()
  (interactive)
  (let ((count (current-column))
        (times (if (and (= (point) (point-min)) (looking-at "<%[^=]")) 2 1)))
    (search-forward-regexp (concat "^" (make-string count ? ) "<%[^=]") nil t times))
  (search-forward "%>" nil t)
  )

(defun backward-erb-tag ()
  (interactive)
  (unless (looking-at "<%") (search-backward "<%" nil t))
  (let ((count (current-column)))
    (search-backward-regexp (concat "^" (make-string count ? ) "<%") nil t 1))
  )

(defun rhtml-mode-forward-sexp ()
  (interactive)
  (cond
   ((looking-at "<%=?.*%>") (forward-erb-tag))
   (t (call-interactively 'web-mode-forward-sexp))))

(defun rhtml-mode-backward-sexp ()
  (interactive)
  (cond
   ((or (looking-back "<%.*" (point-min)) (looking-at "<%")) (backward-erb-tag))
   (t (call-interactively 'web-mode-backward-sexp))))

(defun rhtml-mark-sexp-tag ()
  (interactive)
  (cond
   ((looking-at "<%=?.*%>")
    (progn
      (push-mark nil t t)
      (forward-erb-tag)))
   ;; ((looking-at "<[^%/].*[^%/]>")
   ;;  (progn
   ;;    (forward-word)
   ;;    (web-mode-mark-and-expand)))
   (t (web-mode-mark (point))))
  )

(defun html-mode-newline-and-indent ()
  (interactive)
  (if (or
       (and (looking-back "<[^/]*.*[^%]*>" (point-min)) (looking-at " *</"))
       (and (looking-back "<.*" (point-min)) (looking-at ">")))
      (save-excursion (newline-and-indent)))
  (newline-and-indent))

(add-to-list 'web-mode-commands-like-expand-region 'rhtml-mark-sexp-tag)

(provide 'web-mode_init)
;;; web-mode_init.el ends here

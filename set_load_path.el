;; -*-Emacs-Lisp-*-

;;;; ------------------------------ settings load path ------------------------------
;; (setq default-directory '(concat (file-name-directory (or load-file-name buffer-file-name)) "plugins"))
;; (setq config (expand-file-name "my-customized-settings")) ;

(setq plugins (expand-file-name "plugins"))
(let ((default-directory (concat (file-name-directory (or load-file-name buffer-file-name)) "plugins")))
  (normal-top-level-add-subdirs-to-load-path)
  (add-to-list 'load-path default-directory)
  )


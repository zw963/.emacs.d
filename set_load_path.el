;; -*-Emacs-Lisp-*-

;;;; ------------------------------ settings load path ------------------------------
(setq default-directory (file-name-directory (or load-file-name buffer-file-name)))
;; (setq config (expand-file-name "my-customized-settings")) ;
(setq plugins (expand-file-name "plugins"))
(normal-top-level-add-subdirs-to-load-path)

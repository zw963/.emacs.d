;; -*-Emacs-Lisp-*-

;;;; ------------------------------ settings load path ------------------------------
(setq default-directory (file-name-directory (or load-file-name buffer-file-name)))
(normal-top-level-add-to-load-path '("autoload_plugins/"))
(let ((default-directory (concat default-directory "plugins")))
  (normal-top-level-add-subdirs-to-load-path)
  (add-to-list 'load-path default-directory))

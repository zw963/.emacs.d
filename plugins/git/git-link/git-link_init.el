;; -*- lexical-binding: t; -*-

(require 'git-link-transient)

(setq git-link-default-remote "origin")

(global-set-key [(control x) (v) (c)] 'git-link-dispatch)

(provide 'git-link_init)

;;; git-link_init.el ends here

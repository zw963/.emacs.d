;; -*- lexical-binding: t; -*-

(require 'async-bytecomp)
(require 'dired-async)
(require 'auth-source)
;; (setq auth-sources '("~/.netrc")

(dired-async-mode 1)

(async-bytecomp-package-mode 1)

(require 'smtpmail-async)

(provide 'async_init)

;;; async_init.el ends here

(require 'async-bytecomp)
(require 'dired-async)
(require 'auth-source)
;; (setq auth-sources '("~/.netrc")

(dired-async-mode 1)

(async-bytecomp-package-mode 1)

(require 'smtpmail-async)

(custom-set-variables
 '(send-mail-function 'async-smtpmail-send-it)
 '(message-send-mail-function 'async-smtpmail-send-it)     ; show completion list when ambiguous
 )

(provide 'async_init)

;;; async_init.el ends here

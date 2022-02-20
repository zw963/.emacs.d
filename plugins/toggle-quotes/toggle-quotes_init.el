(require 'toggle-quotes)
(require 'ruby-hash-syntax)

(defun swap-quote-or-ruby-hash ()
  "Convert the double quote to single quote, per contra, ruby hash too."
  (interactive)
  (if (and (use-region-p)
           (member major-mode '(ruby-mode enh-ruby-mode))
           (fboundp 'ruby-hash-syntax-toggle))
      (call-interactively 'ruby-hash-syntax-toggle)
    (toggle-quotes)))

(global-set-key [(control \')] 'swap-quote-or-ruby-hash)

(provide 'toggle-quotes_init)
;;; toggle-quotes_init ends here

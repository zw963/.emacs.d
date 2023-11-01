(setq yard-tags
      '("abstract" "api" "attr" "attr_reader" "attr_writer"
        "author" "deprecated" "example" "note" "option" "overload"
        "param" "private" "raise" "return" "see" "since" "todo" "version"
        "yield" "yieldparam" "yieldreturn" "hidden")
      )

(require 'yard-mode)
;; (add-to-list 'yard-tags "hidden")
;; (add-to-list 'yard-tag-docstrings "@hidden")
(run-ruby-mode-hook '(yard-mode))
(run-ruby-mode-hook '(eldoc-mode))

(provide 'yard-mode_init)

;;; yard-mode_init.el ends here

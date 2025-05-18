;; -*- lexical-binding: t; -*-

(require 'hotfuzz)

(setq completion-styles '(hotfuzz))
(setq helm-completion-style 'emacs)

(setq completion-category-defaults nil)
(completion-category-overrides '((buffer (display-sort-function . identity))
                                 (eglot-capf (styles hotfuzz))))

(provide 'hotfuzz_init)

;;; hotfuzz_init.el ends here

;; -*- lexical-binding: t; -*-

(require 'hotfuzz)

(with-eval-after-load 'helm
  (setq helm-completion-style 'emacs)
  )

(setq
 completion-styles '(hotfuzz)
 completion-category-defaults nil
 completion-category-overrides '((buffer (display-sort-function . identity))
                                 (eglot-capf (styles hotfuzz)))
 )

(provide 'hotfuzz_init)

;;; hotfuzz_init.el ends here

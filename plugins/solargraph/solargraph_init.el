(require 'solargraph)
(setq solargraph:port 7658)

(require 'ac-solargraph)
(ac-solargraph:setup)

(with-eval-after-load 'enh-ruby-mode
  (define-key enh-ruby-mode-map (kbd "C-j") 'solargraph:complete))

(provide 'solargraph_init)

;;; solargraph_init.el ends here

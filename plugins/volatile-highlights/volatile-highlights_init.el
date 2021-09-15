(require 'volatile-highlights)

(volatile-highlights-mode t)

(with-eval-after-load 'undo-tree
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  )

(provide 'volatile-highlights_init)

;;; volatile-highlights_init.el ends here

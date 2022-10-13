(autoload 'gitattributes-mode "gitattributes-mode" nil t)

(dolist (pattern '("/\\.gitattributes\\'"
                   "/info/attributes\\'"
                   "/git/attributes\\'"))
  (add-to-list 'auto-mode-alist (cons pattern #'gitattributes-mode)))

(autoload 'gitconfig-mode "gitconfig-mode" nil t)

(dolist (pattern '("/\\.gitconfig\\'"      "/\\.git/config\\'"
                   "/modules/.*/config\\'" "/git/config\\'"
                   "/\\.gitmodules\\'"     "/etc/gitconfig\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'gitconfig-mode)))

(autoload 'gitignore-mode "gitignore-mode" nil t)

(dolist (pattern (list "/\\.gitignore\\'"
                       "/info/exclude\\'"
                       "/git/ignore\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'gitignore-mode)))

(provide 'git-modes_init)

;;; git-modes_init.el ends here

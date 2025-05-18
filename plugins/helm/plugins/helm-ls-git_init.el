;; -*- lexical-binding: t; -*-

(require 'helm-ls-git)
(setq helm-ls-git-fuzzy-match t)
(global-set-key [(control x) (control d)] 'helm-browse-project)

(setq helm-ls-git-default-sources
      (delete 'helm-ls-git-stashes-source
              (delete 'helm-ls-git-create-branch-source
                      (delete 'helm-ls-git-branches-source
                              helm-ls-git-default-sources))))

(provide 'helm-ls-git_init)

;;; helm-ls-git_init.el ends here

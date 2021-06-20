;; pacman -S hunspell hunspell-en_US
(require 'ispell)
(setq
 ispell-dictionary "en_US"
 ispell-program-name "hunspell"
 ispell-personal-dictionary (expand-file-name "hunspell/hunspell_dict.txt" default-directory)
 )

(add-hook 'gfm-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
;; (add-hook 'org-mode-hook 'flyspell-mode)

(require 'flyspell-correct-avy-menu)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)

(provide 'flyspell_init)

;;; flyspell_init.el ends here

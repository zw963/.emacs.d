(require 'cnfonts)
(require 'cnfonts-ui)

(setq cnfonts-use-face-font-rescale t)

(setq cnfonts-directory (expand-file-name "configs" (file-name-directory (or load-file-name buffer-file-name))))

(expand-file-name "configs" (file-name-directory (or load-file-name buffer-file-name)))

(cnfonts-enable)

(provide 'cnfonts_init)

;;; cnfonts_init.el ends here

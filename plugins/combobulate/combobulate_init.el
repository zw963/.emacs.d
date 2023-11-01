(require 'combobulate)

(add-hook 'python-ts-mode-hook 'combobulate-mode)
(add-hook 'js-ts-mode-hook 'combobulate-mode)
(add-hook 'json-ts-mode-hook 'combobulate-mode)
(add-hook 'css-ts-mode-hook 'combobulate-mode)
(add-hook 'yaml-ts-mode-hook 'combobulate-mode)
(add-hook 'tsx-ts-mode 'combobulate-mode)

(provide 'combobulate_init)

;;; combobulate_init.el ends here

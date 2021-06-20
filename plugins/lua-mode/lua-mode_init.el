(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(setq lua-indent-level 2)

(provide 'lua-mode_init)

;;; lua-mode_init.el ends here

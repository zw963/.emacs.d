;;------------------------------ 显示分页符。 ------------------------------

(require 'page-break-lines)

(with-eval-after-load 'browse-kill-ring+
  (add-to-list 'page-break-lines-modes 'browse-kill-ring-mode)
  )

(global-page-break-lines-mode 1)


(provide 'page-break-lines_init)

;;; page-break-lines_init.el ends here

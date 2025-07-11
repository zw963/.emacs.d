;; -*- lexical-binding: t; -*-

;; 这个会造成 treemacs-follow-mode 当滚动时，也频繁的刷新文件列表，造成页面被 pull back.
(require 'wakatime-mode)

(setq wakatime-disable-on-error t)
(setq wakatime-cli-path (expand-file-name "~/utils/modern-unix-commands/bin/wakatime"))

(global-wakatime-mode)

(provide 'wakatime-mode_init)

;;; wakatime-mode_init.el ends here

(require 'wakatime-mode)

(setq wakatime-disable-on-error t)
(setq wakatime-cli-path (expand-file-name "~/utils/bin/wakatime-cli-linux-amd64"))


(global-wakatime-mode)

(provide 'wakatime-mode_init)

;;; wakatime-mode_init.el ends here

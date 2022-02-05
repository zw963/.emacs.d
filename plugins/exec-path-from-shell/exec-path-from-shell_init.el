(require 'exec-path-from-shell)

(dolist (var
         '(
           "SSH_AUTH_SOCK"
           "SSH_AGENT_PID"
           "http_proxy"
           "https_proxy"
           )
         )
  (add-to-list 'exec-path-from-shell-variables var))

(exec-path-from-shell-initialize)

(provide 'exec-path-from-shell_init)

;;; exec-path-from-shell_init.el ends here

(require 'exec-path-from-shell)

(dolist (var
         '(
           "SSH_AUTH_SOCK"
           "SSH_AGENT_PID"
           "XXX"
           ;; "GPG_AGENT_INFO"
           ;; "LANG"
           ;; "LC_CTYPE"
           ;; "NIX_SSL_CERT_FILE"
           ;; "NIX_PATH"
           )
         )
  (add-to-list 'exec-path-from-shell-variables var))

(exec-path-from-shell-initialize)

(provide 'exec-path-from-shell_init)

;;; exec-path-from-shell_init.el ends here

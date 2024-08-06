(require 'easy-kill)

(defadvice easy-kill (around rect-mark activate)
  "Let 'kill-ring-save support rect-mark."
  (if rectangle-mark-mode
      (call-interactively 'copy-rectangle-as-kill)
    ad-do-it
    ))

(global-set-key [(meta w)] 'easy-kill)
(global-set-key [remap mark-sexp] #'easy-mark)

(dolist (hook '(
                sh-mode-hook
                bash-ts-mode-hook
                ))
  (add-hook hook (lambda ()
                   (global-unset-key [remap mark-sexp])
                   )))

(provide 'easy-kill_init)
;;; easy-kill_init.el ends here

(require 'spaceline-config)

(spaceline-spacemacs-theme)
;; (spaceline-emacs-theme)

(with-eval-after-load 'winum
  (setq winum-auto-setup-mode-line nil)
  )

(with-eval-after-load 'anzu
  (setq anzu-cons-mode-line-p nil)
  )

(spaceline-compile
  '(((persp-name
      workspace-number
      window-number)
     :fallback evil-state
     :face highlight-face
     :priority 100)
    (anzu :priority 95)
    auto-compile
    ((buffer-modified buffer-size buffer-id remote-host)
     :priority 98)
    (major-mode :priority 79)
    (process :when active)
    ((flycheck-error flycheck-warning flycheck-info)
     :when active
     :priority 89)
    (minor-modes :when active
                 :priority 9)
    (mu4e-alert-segment :when active)
    (erc-track :when active)
    (version-control :when active
                     :priority 78)
    (org-pomodoro :when active)
    (org-clock :when active)
    nyan-cat)
                                        ; right side
  '(which-function
    (python-pyvenv :fallback python-pyenv)
    (purpose :priority 94)
    (battery :when active)
    (selection-info :priority 95)
    input-method
    ((buffer-encoding-abbrev
      point-position
      line-column)
     :separator " | "
     :priority 96)
    (global :when active)
    (buffer-position :priority 99)
    (hud :priority 99)))

;; (setq spaceline-buffer-encoding-abbrev-p nil)
;; (setq spaceline-buffer-encoding-p t)


;; (setq spaceline-persp-name-p t)
;; (setq spaceline-workspace-number-p t)
;; (setq spaceline-window-number-p t)
;; (setq spaceline-evil-state-p t)
;; (setq spaceline-anzu-p t)
;; (setq spaceline-auto-compile-p t)
;; (setq spaceline-buffer-modified-p t)
;; (setq spaceline-buffer-size-p t)
;; (setq spaceline-buffer-id-p t)
;; (setq spaceline-remote-host-p t)
;; (setq spaceline-major-mode-p t)


;; (setq spaceline-flycheck-error-p t)
;; (setq spaceline-flycheck-warning-p t)
;; (setq spaceline-flycheck-info-p t)
;; (setq spaceline-minor-modes-p t)
;; (setq spaceline-process-p t)
;; (setq spaceline-erc-track-p t)
;; (setq spaceline-version-control-p t)
;; (setq spaceline-org-pomodoro-p t)
;; (setq spaceline-org-clock-p t)
;; (setq spaceline-nyan-cat-p t)
;; (setq spaceline-battery-p t)
;; (setq spaceline-which-function-p t)
;; (setq spaceline-python-pyvenv-p t)
;; (setq spaceline-python-pyenv-p t)
;; (setq spaceline-paradox-menu-p t)
;; (setq spaceline-selection-info-p t)
;; (setq spaceline-input-method-p t)
;; (setq spaceline-buffer-encoding-abbrev-p t)
;; (setq spaceline-point-position-p t)
;; (setq spaceline-line-column-p t)
;; (setq spaceline-global-p t)
;; (setq spaceline-buffer-position-p t)
;; (setq spaceline-hud-p t)


;; (setq spaceline-helm-number-p t)
;; (setq spaceline-helm-buffer-id-p t)
;; (setq spaceline-helm-help-p t)
;; (setq spaceline-helm-prefix-argument-p t)
;; (setq spaceline-helm-follow-p t)

(spaceline-helm-mode 1)

(provide 'spaceline_init)

;;; spaceline_init.el ends here

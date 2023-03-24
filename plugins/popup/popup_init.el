(require 'popup)
(load "popup_doom_hack.el")
(load "settings.el")

(set-popup-rule! "\\*LSP Dart tests\\*" :height 0.3)
(set-popup-rule! "\\*Hover\\*" :quit nil)
(set-popup-rule! "\\*dap-ui-locals\\*" :side 'right :width 0.3)
(set-popup-rule! "\\*dap-ui-sessions\\*" :side 'right :width 0.3)
(set-popup-rule! "\\*midje-test-report\\*" :side 'right :width 0.5)
(set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
(set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil)
(set-popup-rule! "\\*Crystal-Context\\*" :side 'right :width 0.3)

(provide 'popup_init)

;;; popup_init.el ends here

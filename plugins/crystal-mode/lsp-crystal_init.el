;; 注意，这里不要首先 require lsp-crystal, 否则将用默认的 scry 作为 server.
;; (setq lsp-clients-crystal-executable '("crystalline" "--stdio"))

;; (with-eval-after-load 'lsp-mode
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("crystalline"))
;;                     :activation-fn (lsp-activate-on "crystal")
;;                     :priority '1
;;                     :server-id 'crystalline)))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(crystal-mode . "crystal"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("crystalline"))
                    :activation-fn (lsp-activate-on "crystal")
                    :priority '1
                    :server-id 'crystalline)))

(require 'lsp-mode_init)
(add-hook 'crystal-mode-hook 'lsp-mode-common-hooks)

(require 'dap-mode_init)
;; 运行 dap-codelldb-setup 来安装扩展
(require 'dap-codelldb)

(dap-register-debug-template "Crystal LLDB"
                             (list :type "lldb"
                                   :request "launch"
                                   :name "crystal: debug current file"
                                   :preLaunchTask "crystal: build current file (debug)"
                                   :mode "auto"
                                   :program "${workspaceFolder}/bin/${fileBasenameNoExtension}"
                                   :cwd "${workspaceFolder}"
                                   :buildFlags nil
                                   :initCommands: [
                                                   "command script import /home/zw963/Crystal/crystal-lang/crystal/etc/lldb/crystal_formatters.py"
                                                   ]
                                   :args nil
                                   :env nil))

(provide 'lsp-crystal_init)

;;; lsp-crystal_init.el ends here

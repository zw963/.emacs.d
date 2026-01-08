;; -*- lexical-binding: t; -*-

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

(provide 'dap-crystal_init)

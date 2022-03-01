(require 'feature-mode)

(with-eval-after-load 'feature-mode
  (setenv "RUBYLIB" (concat (getenv "RUBYLIB") ":" "~/utils/ruby_tools/app/lib/cucumber")))

(setq feature-default-i18n-file (expand-file-name "~/Dropbox/common/.emacs.d/plugins/feature-mode/i18n.yml"))
(setq feature-use-rvm t)
(setq feature-ruby-command "/home/zw963/utils/ruby_tools/bin/ruby-portable")

;; | feature          | "功能"                   |
;; | background       | "背景"                   |
;; | scenario         | "场景", "剧本"             |
;; | scenario_outline | "场景大纲", "剧本大纲"         |
;; | examples         | "例子"                   |
;; | given            | "* ", "假如", "假设", "假定" |
;; | when             | "* ", "当"              |
;; | then             | "* ", "那么"             |
;; | and              | "* ", "而且", "并且", "同时" |
;; | but              | "* ", "但是"             |

(defcustom cucumber-user-defined-keywords
  '("功能" "背景" "场景" "剧本" "场景大纲" "剧本大纲" "例子"
    "假如" "假设" "假定" "当" "那么" "而且" "并且" "同时" "但是")
  "List of keywords to highlight for spec"
  :group 'rinari
  :type '(repeat string)
  )

(defun cucumber-apply-keywords-locally ()
  (self-defined-highlight-keywords cucumber-user-defined-keywords 'font-lock-keyword-face) ; 暗金色
  )
(add-hook 'feature-mode-hook 'cucumber-apply-keywords-locally)

(provide 'feature-mode_init)
;;; feature-mode_init.el ends here

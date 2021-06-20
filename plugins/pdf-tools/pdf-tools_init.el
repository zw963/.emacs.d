(setq pdf-info-epdfinfo-program (expand-file-name "pdf-tools/epdfinfo/bin/epdfinfo" plugins))

(require 'pdf-tools)
(require 'pdf-loader)
(require 'pdf-history)
(require 'pdf-links)
(require 'pdf-outline)
(require 'pdf-annot)
(require 'pdf-sync)
(require 'pdf-occur)
(require 'pdf-virtual)

;; 激活 pdf-tools
(pdf-tools-install)

;; 让 gnus 支持使用 emacs 打开
;; 修改: .mailcap, , 添加:
;; application/pdf; emacsclient %s

;; 让 xdg-open 支持:
;; $: xdg-mime default emacsclient.desktop application/pdf

;; debain 安装:
;; apt-get install libpoppler-dev libpoppler-private-dev poppler-glib-dev

;; arch 安装
;; pacman -S poppler

(with-eval-after-load 'org (require 'org-pdfview))
;; 支持新的链接类型: [[pdfview:/path/to/myfile.pdf::42][My file Description]]
;; 快捷键: C-c C-l

(add-to-list 'org-file-apps
             '("\\.pdf\\'" . (lambda (file link)
                               (org-pdfview-open link))))

(with-eval-after-load 'pdf-view-mode
  (setq pdf-view-midnight-colors '("#839496" . "#002b36" ))
  (pdf-view-midnight-minor-mode))

(require 'org-noter)
(setq org-noter-auto-save-last-location t)
(setq org-noter-always-create-frame nil)
(setq org-noter-insert-note-no-questions t)
(setq org-noter-doc-split-fraction '(0.7 . 0.3))

(provide 'pdf-tools_init)
;;; pdf-tools_init.el ends here

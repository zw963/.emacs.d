;; ------------------------------ 模式行美化 ------------------------------
;; 这个包造成 delete-selection-mode 关闭，需要更新.

(require 'smart-mode-line)
(setq sml/theme 'smart-mode-line-powerline)
;; (setq sml/theme 'smart-mode-line-atom-one-dark)

;; 这个可能需要根据屏幕来做调整, 确保没有 mode line 选项跑到屏幕右侧外面.
(setq sml/mode-width 25)
;; (setq sml/vc-mode-show-backend t)

(setq
 sml/no-confirm-load-theme t
 sml/name-width 55
 ;; sml/shorten-directory t
 ;; sml/shorten-modes t

 ;; rm-blacklist '(" pair" " ctagsU" " ARev" " hs"
 ;;                  " Wrap" " Rinari"
 ;;                " Ruby-Test" " Hi")
 ;; rm-blacklist '(" pair" " Anzu" " hl-s"
 ;;                  " =>" " RJ"
 ;;                " Paredit" " Guide" )

 ;; rm-blacklist '(" ARev" " hs"
 ;;                " Wrap"  " Rinari"
 ;;                " Ruby-Test"  " Hi")

 ;; rm-blacklist '(" Anzu" " hl-s"
 ;;                 " =>"  " RJ"  Helm"
 ;;                " Paredit" " Guide" )

 ;; rm-blacklist '(" Helm")

 ;; mode-line-format '("%e" mode-line-front-space mode-line-mule-info mode-line-client
 ;;                    mode-line-modified mode-line-remote mode-line-frame-identification
 ;;                    mode-line-buffer-identification "   " (10 "(%P)")
 ;;                    (vc-mode vc-mode)
 ;;                    "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)
 )

;; (add-list-to-list 'rm-blacklist '
;;                   (
;;                    " Helm" " Undo-Tree" " AC" " yas" " super-save"
;;                    " ElDoc" " AI" " ctagsU" " GitGutter" " md" " PgLn"
;;                    " YARD" " Abbrev" " (*)" " ," " Anzu" " WK" " company" " WS"
;;                    " Helm-Fuz" " ivy-posframe" " ivy" "Lens"
;;                    ))

;; use rich-minority package.
(setq rm-blacklist (mapconcat 'identity '(
                                          " ,"              ;; subward
                                          " Anzu"           ;; Anzu mode
                                          " yas"            ;; yasnippet
                                          " company"        ;; company
                                          " ivy-posframe"   ;; ivy-posframe
                                          " ivy"            ;; ivy
                                          " Helm"           ;; Helm mode
                                          " Undo-Tree"      ;; Undo Tree mode
                                          " AI"             ;; Auto indent mode
                                          " md"             ;; Move-Dup mode
                                          " WK"             ;; Which key mode
                                          "\(\*\)"          ;; Beacon mode
                                          " hs"             ;; hide/show mode
                                          " WS"             ;; White Space mode
                                          " GCMH"           ;; GCMH mode
                                          " \\*"            ;; fancy narraw mode
                                          " YARD"           ;; YARD mode
                                          " ElDoc"          ;; ElDoc mode
                                          " wb"             ;; ws butler mode
                                          " ColorIds"       ;; Color identifiers mode
                                          " SP"             ;; smartparens mode
                                          " PgLn"           ;; page-break-lines
                                          " Ρ"              ;; pangu spacing
                                          ) "\\|"))

(add-to-list 'sml/replacer-regexp-list '("^~/Airhost/airhost_ror" ":AH:"))

;; (require 'nyan-mode)
;; (add-to-list 'mode-line-format (list '(:eval (list (nyan-create)))))

;; (require 'parrot)
;; (add-to-list 'mode-line-format (list '(:eval (list (parrot-create)))))

(sml/setup)

(require 'show-point-mode)
(add-hook 'emacs-lisp-mode-hook 'show-point-mode)

(provide 'smart-mode-line_init)
;;; smart-mode-line_init.el ends here

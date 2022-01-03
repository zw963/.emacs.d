(require 'rich-minority)

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

;; (add-list-to-list 'rm-blacklist '
;;                   (
;;                    " Helm" " Undo-Tree" " AC" " yas" " super-save"
;;                    " ElDoc" " AI" " ctagsU" " GitGutter" " md" " PgLn"
;;                    " YARD" " Abbrev" " (*)" " ," " Anzu" " WK" " company" " WS"
;;                    " Helm-Fuz" " ivy-posframe" " ivy" "Lens"
;;                    ))

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
                                          "VHl"
                                          ) "\\|"))

(provide 'rich-minority_init)

;;; rich-minority_init.el ends here

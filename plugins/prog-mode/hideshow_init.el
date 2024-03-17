(require 'hideshow)

;; 某些模式下, 可能需要定制 hs-hide-all-non-comment-function.
;; 参加下面的例子, the following code shows the next nested level in addition to
;; the top-level:
;;   (defun ttn-hs-hide-level-1 ()
;;     (when (hs-looking-at-block-start-p)
;;       (hs-hide-level 1))
;;     (forward-sexp 1))
;;   (setq hs-hide-all-non-comment-function 'ttn-hs-hide-level-1)

(unless (memq 'hs-headline mode-line-format)
  (setq mode-line-format
        (append '("-" hs-headline) mode-line-format)))

;; 支持 hideshow 时，显示小箭头以及隐藏的行数。
(setq elpy-folding-fringe-indicators t)
(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'elpy-folding-fringe-marker
    (vector #b00000000
            #b00000000
            #b00000000
            #b11000011
            #b11100111
            #b01111110
            #b00111100
            #b00011000))

  (define-fringe-bitmap 'elpy-folding-fringe-foldable-marker
    (vector #b00100000
            #b00010000
            #b00001000
            #b00000100
            #b00000100
            #b00001000
            #b00010000
            #b00100000)))

(defun elpy-folding--display-code-line-counts (ov)
  "Display a folded region indicator with the number of folded lines.

Meant to be used as `hs-set-up-overlay'."
  (let* ((marker-string "*fringe-dummy*")
         (marker-length (length marker-string))
         (close-icon (if (featurep 'all-the-icons) (concat "... " (all-the-icons-material "unfold_more")) "...")))
    ;; (close-icon (if (featurep 'nerd-icons) (concat "... " (nerd-icons-mdicon "nf-md-unfold_more_horizontal")) "...")))
    (cond
     ((eq 'code (overlay-get ov 'hs))
      (let* ((nmb-line (count-lines (overlay-start ov) (overlay-end ov)))
             (display-string (format (concat "(%d)" close-icon) nmb-line)))
        ;; fringe indicator
        (when elpy-folding-fringe-indicators
          (put-text-property 0 marker-length 'display
                             (list 'left-fringe 'elpy-folding-fringe-marker
                                   'elpy-folding-fringe-face)
                             marker-string)
          (overlay-put ov 'before-string marker-string)
          (overlay-put ov 'elpy-hs-fringe t))
        ;; folding indicator
        (put-text-property 0 (length display-string)
                           'face 'elpy-folding-face display-string)
        (put-text-property 0 (length display-string)
                           'mouse-face 'highlight display-string)
        (overlay-put ov 'display display-string)
        (overlay-put ov 'elpy-hs-folded t)))
     ;; for docstring and comments, we don't display the number of line
     ((or (eq 'docstring (overlay-get ov 'hs))
          (eq 'comment (overlay-get ov 'hs)))
      (let ((display-string close-icon))
        (put-text-property 0 (length display-string)
                           'mouse-face 'highlight display-string)
        (overlay-put ov 'display display-string)
        (overlay-put ov 'elpy-hs-folded t))))))

(setq hs-set-up-overlay #'elpy-folding--display-code-line-counts)
(add-hook 'prog-mode-hook 'hs-minor-mode)

(add-hook 'hs-minor-mode-hook (lambda ()
                                (local-set-key [(control c) (/)] 'hs-hide-all)
                                (local-set-key [(control c) (\\)] 'hs-show-all)
                                (local-set-key [(control tab)] 'hs-toggle-hiding)
                                (local-set-key [(mouse-2)] 'hs-toggle-hiding)
                                ))

;; 这里额外启用了 :box t 属性使得提示更加明显, 不知道啥效果，打开试一下
(defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

(add-to-list 'hs-special-modes-alist '(dart-mode "{" "}" "/[*/]" nil nil))
(add-to-list 'hs-special-modes-alist '(go-mode "{" "}" "/[*/]" nil nil))
(add-to-list 'hs-special-modes-alist '(mint-mode "{" "}" "/[*/]" nil nil))

(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(def\\|do\\|{\\)" "\\(end\\|}\\)" "#" (lambda (arg) (ruby-forward-sexp)) nil))

(add-to-list 'hs-special-modes-alist
             '(ruby-ts-mode
               "\\(def\\|do\\|{\\)" "\\(end\\|}\\)" "#" (lambda (arg) (ruby-forward-sexp)) nil))

(add-to-list 'hs-special-modes-alist
             '(enh-ruby-mode
               "\\(def\\|do\\|{\\)" "\\(end\\|}\\)" "#" (lambda (arg) (enh-ruby-forward-sexp)) nil))

(add-to-list 'hs-special-modes-alist
             '(crystal-mode
               "\\(def\\|do\\|{\\)" "\\(end\\|}\\)" "#" (lambda (arg) (crystal-forward-sexp)) nil))

(add-to-list 'hs-special-modes-alist
             `(elixir-ts-mode
               ,(concat (regexp-opt '("def" "defmodule" "defprotocol" "case" "cond" "quote" "receive" "fn" "if" "for")) ".*? do") "end" "#" (lambda (arg) (elixir-ts--forward-sexp)) nil))

(add-to-list 'hs-special-modes-alist
             '(bash-ts-mode
               "{" "}" "#" nil nil))

(provide 'hideshow_init)

;;; name_init.el ends here

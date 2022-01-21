(require 'hydra)

(with-eval-after-load 'git-gutter
  (defhydra hydra-git-gutter (global-map "C-x v")
    "Git gutter"
    ("n" git-gutter:next-hunk "next-hunk")
    ("p" git-gutter:previous-hunk "previous-hunk")))

;; (with-eval-after-load 'zoom-frm
;;   (defhydra hydra-zoom-frm (global-map "C-x -")
;;     "zoom frame"
;;     ("-" zoom-frm-out "zoom frame out")
;;     ("=" zoom-frm-in "zoom frame in"))
;;   )

(with-eval-after-load 'dap-mode
  (require 'dap-hydra)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  )

(with-eval-after-load 'back-button
  (defhydra hydra-back-button
    (
     global-map "C-x ,"
     :pre (setq cursor-type t)
     :post (setq cursor-type 'bar)
     )
    "global mark ring"
    ("j" back-button-global-backward "global previous mark")
    ("l" back-button-global-forward "global next hunk"))
  )


;; (defhydra hydra-block-nav
;;   (
;;    global-map "C-x m"
;;    :pre (setq cursor-type t)
;;    :post (setq cursor-type 'bar)
;;    )
;;   "navigate block"
;;   ("n" block-nav-next-block "next")
;;   ("p" block-nav-previous-block "previous")
;;   ("j" block-nav-previous-indentation-level "up one level")
;;   ("l" block-nav-next-indentation-level "down one level")
;;   )

;; (defhydra hydra-rectangle
;;   (:body-pre (rectangle-mark-mode 1)
;;    :color pink
;;    :post (deactivate-mark)
;;    )
;;   "
;;   ^_k_^     _d_elete    _s_tring     |\\     _,,,--,,_
;; _h_   _l_   _o_k        _y_ank       /,`.-'`'   ._  \-;;,_
;;   ^_j_^     _n_ew-copy  _r_eset     |,4-  ) )_   .;.(  `'-'
;; ^^^^        _e_xchange  _u_ndo     '---''(_/._)-'(_\_)
;; ^^^^        ^ ^         _p_aste
;; "
;;   ("h" backward-char nil)
;;   ("l" forward-char nil)
;;   ("k" previous-line nil)
;;   ("j" next-line nil)
;;   ("e" exchange-point-and-mark nil)
;;   ("n" copy-rectangle-as-kill nil)
;;   ("d" delete-rectangle nil)
;;   ("r" (if (region-active-p)
;;            (deactivate-mark)
;;          (rectangle-mark-mode 1)) nil)
;;   ("y" yank-rectangle nil)
;;   ("u" undo nil)
;;   ("s" string-rectangle nil)
;;   ("p" kill-rectangle nil)
;;   ("o" nil nil))
;; (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

(provide 'hydra_init)

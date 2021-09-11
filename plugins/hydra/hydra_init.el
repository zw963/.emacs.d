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

(with-eval-after-load 'hydra
  (defhydra hydra-block-nav
    (
     global-map "C-x m"
     :pre (setq cursor-type t)
     :post (setq cursor-type 'bar)
     )
    "navigate block"
    ("n" block-nav-next-block "next")
    ("p" block-nav-previous-block "previous")
    ("j" block-nav-previous-indentation-level "up one level")
    ("l" block-nav-next-indentation-level "down one level")
    ))

(with-eval-after-load 'back-button
  (defhydra hydra-back-button
    (
     global-map "C-x ,"
     :pre (setq cursor-type t)
     :post (setq cursor-type 'bar)
     )
    "global mark ring"
    ("n" back-button-global-forward "global next mark")
    ("p" back-button-global-backward "global previous hunk")))

(provide 'hydra_init)

;; -*- lexical-binding: t; -*-

(require 'hydra)

;; (with-eval-after-load 'git-gutter
;;   (defhydra hydra-git-gutter (global-map "C-x v")
;;     "Git gutter"
;;     ("n" git-gutter:next-hunk "next-hunk")
;;     ("p" git-gutter:previous-hunk "previous-hunk")))

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

(with-eval-after-load 'dumb-jump
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))
  )

(defhydra hydra-window ()
  "
Movement^^        ^Split^         ^Switch^              ^Resize^
----------------------------------------------------------------
_h_ ←           _v_ertical      _b_uffer                _q_ X←
_j_ ↓           _x_ horizontal  _f_ind files    _w_ X↓
_k_ ↑           _z_ undo        _a_ce 1         _e_ X↑
_l_ →           _Z_ reset       _s_wap          _r_ X→
_F_ollow                _D_lt Other     _S_ave          max_i_mize
_SPC_ cancel    _o_nly this     _d_elete
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("q" hydra-move-splitter-left)
  ("w" hydra-move-splitter-down)
  ("e" hydra-move-splitter-up)
  ("r" hydra-move-splitter-right)
  ("b" helm-mini)
  ("f" helm-find-files)
  ("F" follow-mode)
  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body))
   )
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right))
   )
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down))
   )
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("S" save-buffer)
  ("d" delete-window)
  ("D" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body))
   )
  ("o" delete-other-windows)
  ("i" ace-maximize-window)
  ("z" (progn
         (winner-undo)
         (setq this-command 'winner-undo))
   )
  ("Z" winner-redo)
  ("SPC" nil)
  )
;; FIXME: 这个和 helm-recentf 冲突。
;; (global-set-key [(control x) (f)] 'hydra-window/body)

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

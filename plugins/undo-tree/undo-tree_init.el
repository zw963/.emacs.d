;; -*- lexical-binding: t; -*-

;; (require 'undo-fu)

;; (with-eval-after-load 'undo-fu
;;   (global-unset-key (kbd "C-z"))
;;   (global-set-key (kbd "C-z")   'undo-fu-only-undo)
;;   (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
;;   )

;; ------------------------------ undo-tree ------------------------------
;; 测试下, 这个包到底有没有问题.
(require 'undo-tree)

(with-eval-after-load 'undo-tree
  (setq
   undo-tree-auto-save-history nil ; 开启性能有问题? 因此关闭。
   ;; undo-tree-visualizer-diff t  ; 这个无需设定, d 快捷键将会显示 diff
   ;; undo-tree-visualizer-timestamps t ; 则个可以通过 t 切换.
   undo-tree-enable-undo-in-region t
   )

  ;; 使用这个目录，可能是造成 undo 性能问题的元凶。
  ;; (add-to-list 'undo-tree-history-directory-alist `(".*" . ,(expand-file-name "~/.undo-historys")))

  ;; (defadvice undo-tree-make-history-save-file-name
  ;;     (after undo-tree activate)
  ;;   (setq ad-return-value (concat ad-return-value ".gz")))

  (global-undo-tree-mode)

  (global-set-key [(control meta /)] 'undo-tree-redo)

  ;; (define-key undo-tree-visualizer-mode-map [(control \8)] 'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-mode-map [(l)] 'undo-tree-visualize-switch-branch-right)
  (define-key undo-tree-visualizer-mode-map [(j)] 'undo-tree-visualize-switch-branch-left)
  )

;; gpt 生成的版本
(defun undo-tree-or-undo-pp (&optional arg)
  (interactive "p")
  ;; 先做便宜判断，避免每次都扫 buffer
  (if (save-excursion
        (let ((p (point)))
          (goto-char (max (point-min) (- p 300)))
          (search-forward "(puts \"\\033\\[1;44m\"; __x = (" (min (point-max) (+ p 300)) t)))
      (let ((begin-pos (save-excursion
                         (search-backward-regexp "(puts \"\\\\033\\[1;44m\"; __x = (" nil t)))
            (end-pos   (save-excursion
                         (search-forward-regexp "); puts File.read_?lines.*__x)" nil t 1))))
        (if (and begin-pos end-pos)
            (save-excursion
              (replace-regexp "(puts.*__x = (\\|); puts File.read_?lines.*__x)" ""
                              nil begin-pos end-pos))
          (undo-tree-undo arg)))
    (undo-tree-undo arg)))

(global-set-key [remap undo-tree-undo] #'undo-tree-or-undo-pp)

(provide 'undo-tree_init)
;;; undo-tree_init.el ends here


;; =============== 不用的代码 ===============

;; ;; 不记得 undo pp 怎么用了，肯定性能不好。
;; (defun undo-tree-or-undo-pp (&optional arg)
;;   (interactive)
;;   (let ((begin-pos (save-excursion (search-backward-regexp "(puts \"\\\\033\\[1;44m\"; __x = (" nil t)))
;;         (end-pos (save-excursion (search-forward-regexp "); puts File.read_?lines.*__x)" nil t 1))))
;;     (cond ((and begin-pos end-pos)
;;            (save-excursion (replace-regexp "(puts.*__x = (\\|); puts File.read_?lines.*__x)" "" nil begin-pos end-pos)))
;;           (t (undo-tree-undo arg)))))

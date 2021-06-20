;; ------------------------------ undo-tree ------------------------------
;; 测试下, 这个包到底有没有问题.
(require 'undo-tree)
(setq
 undo-tree-auto-save-history t
 ;; undo-tree-visualizer-diff t  ; 这个无需设定, d 快捷键将会显示 diff
 ;; undo-tree-visualizer-timestamps t ; 则个可以通过 t 切换.
 undo-tree-enable-undo-in-region t ; 不需要选区内 undo
 )

;; 使用这个目录，可能是造成 undo 性能问题的元凶。
;; (add-to-list 'undo-tree-history-directory-alist `(".*" . ,(expand-file-name "~/.undo-historys")))

;; (defadvice undo-tree-make-history-save-file-name
;;     (after undo-tree activate)
;;   (setq ad-return-value (concat ad-return-value ".gz")))

(global-undo-tree-mode)

(defun undo-tree-or-undo-pp (&optional arg)
  (interactive)
  (cond ((and (save-excursion (setq begin-pos (search-backward-regexp "(puts \"\\\\033\\[0;44m\\\\033\\[1m\";__x=(" nil t) ))
              (save-excursion (setq end-pos (search-forward-regexp ");puts File.readlines.*__x)" nil t 1))))
         (save-excursion (replace-regexp "(puts.*__x=(\\|);puts File.readlines.*__x)" "" nil begin-pos end-pos)))
        (t (call-interactively 'undo-tree-undo))))

(global-set-key [remap undo-tree-undo] 'undo-tree-or-undo-pp)

(global-set-key [(control meta /)] 'undo-tree-redo)

;; (define-key undo-tree-visualizer-mode-map [(control \8)] 'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map [(l)] 'undo-tree-visualize-switch-branch-right)
(define-key undo-tree-visualizer-mode-map [(j)] 'undo-tree-visualize-switch-branch-left)

(provide 'undo-tree_init)
;;; undo-tree_init.el ends here

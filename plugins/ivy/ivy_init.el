(require 'counsel)

(setq counsel-find-file-ignore-regexp (regexp-opt boring-file-regexp-list))

;; ivy-ignore-buffers

;; ;; 使用rg 替代 grep, 使用counsel-grep-or-swiper的时候
;; (setq counsel-grep-base-command
;;       "rg -i -M 120 --no-heading --line-number --color never '%s' %s")

(setq counsel-find-file-ignore-regexp
      (string-join boring-file-regexp-list "\\|")
      ;; (concat
      ;;  ;; filename begins with #
      ;;  "\\(?:\\`[#.]\\)"
      ;;  ;; filename ends with # or ~
      ;;  "\\|\\(?:\\`.+?[#~]\\'\\)"
      ;;  "\\|\\.elc\\'"
      ;;  "\\|\\.pyc\\'"
      ;;  "\\|\\.meta\\'"
      ;;  )
      )

(add-list-to-list 'ivy-ignore-buffers boring-buffer-regexp-list)

(ivy-mode 1)

(setq ivy-use-virtual-buffers t) ;; Enable bookmarks and recentf
;; (setq enable-recursive-minibuffers t)
;; (setq ivy-use-selectable-prompt t)

(global-set-key (kbd "C-x C-r") 'ivy-resume)

;; (counsel-mode 1)

;; 暂时关闭，因为和 ctrlf 冲突
;; (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; 下面是 counsel 下面的一些绑定, 仅供了解
;; (global-set-key [remap switch-to-buffer] 'ivy-switch-buffer)


(global-set-key [remap dired] 'counsel-dired)
(global-set-key [remap set-variable] 'counsel-set-variable)
(global-set-key [remap insert-char] 'counsel-unicode-char)
(global-set-key [remap recentf-open-files] 'counsel-recentf)
(global-set-key [remap command-history] 'counsel-command-history)
(global-set-key [remap apropos-command] 'counsel-apropos)
(global-set-key [remap bookmark-jump] 'counsel-bookmark)
(global-set-key [remap describe-bindings] 'counsel-descbinds)
(global-set-key [remap describe-face] 'counsel-describe-face)
(global-set-key [remap describe-function] 'counsel-describe-function)
(global-set-key [remap describe-symbol] 'counsel-describe-symbol)
(global-set-key [remap describe-variable] 'counsel-describe-variable)
;; (global-set-key [remap execute-extended-command] 'counsel-M-x)
;; (global-set-key [remap find-file] 'counsel-find-file)
(global-set-key [remap find-library] 'counsel-find-library)
(global-set-key [remap geiser-doc-look-up-manual] 'counsel-geiser-doc-look-up-manual)
(global-set-key [remap imenu] 'counsel-imenu)
(global-set-key [remap info-lookup-symbol] 'counsel-info-lookup-symbol)
(global-set-key [remap list-faces-display] 'counsel-faces)
(global-set-key [remap load-library] 'counsel-load-library)
(global-set-key [remap load-theme] 'counsel-load-theme)
(global-set-key [remap pop-to-mark-command] 'counsel-mark-ring)
;; (global-set-key [remap yank-pop] 'counsel-yank-pop)
;; (global-set-key "\C-s" 'swiper)

;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

(with-eval-after-load 'hydra
  (setq ivy-read-action-function #'ivy-hydra-read-action)
  )

(with-eval-after-load 'yasnippet
  (require 'ivy-yasnippet)
  ;; Run ivy-yasnippet directly
  )

(require 'ivy-xref)
(when (>= emacs-major-version 27)
  (setq xref-show-definitions-function #'ivy-xref-show-defs))
(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)

(require 'ivy-emoji)

(provide 'ivy_init)

;;; ivy_init.el ends here

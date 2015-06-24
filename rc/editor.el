(setq enable-local-eval t)

(setq-default indent-tabs-mode nil
              tab-width 4
              case-fold-search t
              default-directory "~"
              fill-column 80)

(delete-selection-mode t)

(require 'use-package)
(use-package diminish)
(use-package bind-key)

(use-package whitespace
  :diminish whitespace-mode
  :config
  (setq whitespace-style '(face trailing lines-tail
                                space-before-tab
                                indentation space-after-tab)
        whitespace-line-column 80))

(setq next-line-add-newlines nil
      require-final-newline t
      kill-whole-line t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-auto-revert-mode t)


(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))


(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (local-file-name "cache/saveplace")))


(use-package savehist
  :config
  (setq savehist-additional-variables
        '(search ring regexp-search-ring)
        savehist-autosave-interval 30
        savehist-file (local-file-name "cache/savehist"))
  (savehist-mode t))


(use-package recentf
  :init
  (setq recentf-save-file (local-file-name "cache/recentf")
        recentf-max-saved-items 50
        recentf-max-menu-items 10)
  :config
  (recentf-mode t))


(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))


(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (show-smartparens-global-mode t)
  (smartparens-global-mode 1))


(use-package volatile-highlights
  :ensure t
  :config (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)


(use-package tramp)


(add-hook 'text-mode-hook 'turn-on-auto-fill)


(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode)
  (define-key company-mode-map (kbd "C-M-i") 'company-complete)
  (setq company-require-match "Off"
        company-clang-arguments '("-std=c++11")))

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line))
(global-set-key (kbd "M-/") 'hippie-expand)


(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config
  (golden-ratio-mode))


(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (require 'helm-config)
  (setq helm-split-window-in-side-p t)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b")   'helm-for-files)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (helm-mode))


(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-keymap-prefix (kbd "C-c p")
        projectile-cache-file (local-file-name "cache/projectile.cache")
        projectile-known-projects-file (local-file-name "store/projectile-bookmarks.eld"))
  (projectile-global-mode))


(use-package helm-projectile
  :ensure t
  :config
  (setq helm-for-files-preferred-list
        '(helm-source-buffers-list
          helm-source-projectile-files-list
          helm-source-recentf
          helm-source-bookmarks
          helm-source-file-cache
          helm-source-files-in-current-dir
          helm-source-locate))
  (helm-projectile-on))


(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-auto-save-history t)
  (defalias 'redo 'undo-tree-redo)

  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz")))

  (custom-set-variables
   '(undo-tree-history-directory-alist
     (quote (("." . "~/.emacs.d/cache/undo/"))))))


(use-package magit
  :ensure t
  :bind ("M-k" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0"))


(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config (global-anzu-mode t))


(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))


(use-package move-text
  :ensure t
  :bind (("C-S-<up>" . move-text-up)
         ("C-S-<down>" . move-text-down)))


(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode t))


(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode t))

(use-package flyspell
  :ensure t
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook '(lambda () (flyspell-mode t))))


(use-package ace-jump-mode
  :ensure t
  :config
  (global-set-key (kbd "C-c SPC") 'ace-jump-mode)
  (setq ace-jump-mode-gray-background nil))

(use-package buffer-move
  :ensure t)

(defun ace-window-golden-rato (arg)
    (interactive "p")
  (ace-window arg)
  (golden-ratio))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window-golden-rato))

(setq ring-bell-function #'ignore
      tab-always-indent 'complete
      scroll-conservatively 0
      scroll-margin 3
      backup-directory-alist `(("." . ,(expand-file-name "backups/" "~/.cache/emacs/")))
      auto-save-file-name-transforms `((".*"  ,(expand-file-name "autosave/" "~/.cache/emacs/") t))
      mac-command-modifier 'meta
      mac-option-modifier 'super
      scroll-error-top-bottom t
      completion-styles '(flex)
      zig-format-on-save t
      require-final-newline t
      kill-do-not-save-duplicates t
      compilation-scroll-output t
      compilation-ask-about-save nil
      compilation-auto-jump-to-first-error nil
      scroll-preserve-screen-position 't)

(setq-default indent-tabs-mode nil
	          tab-width 4
	          fill-column 80)

(add-to-list 'default-frame-alist '(font . "JetBrains Mono-16"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(undecorated . t))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(delete-selection-mode +1)
(electric-pair-mode +1)
(recentf-mode +1)
(global-hl-line-mode +1)
(auto-save-visited-mode +1)
(global-auto-revert-mode t)
(savehist-mode +1)
(column-number-mode +1)
(which-key-mode +1)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'prog-mode-hook #'hs-minor-mode)

(global-set-key (kbd "M-z") #'undo)
(global-set-key (kbd "M-v") #'yank)
(global-set-key (kbd "C-c x") #'execute-extended-command)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(global-set-key (kbd "M-<left>")  #'move-beginning-of-line)
(global-set-key (kbd "M-<right>") #'end-of-line)
(global-set-key (kbd "M-<backspace>") #'kill-whole-line)
(global-set-key (kbd "M-<kp-delete>") #'kill-line)

(global-set-key (kbd "s-<left>")  #'backward-word)
(global-set-key (kbd "s-<right>") #'forward-word)
(global-set-key (kbd "s-<backspace>") #'backward-kill-word)
(global-set-key (kbd "s-<kp-delete>") #'kill-word)

(global-set-key (kbd "M-<up>") #'beginning-of-buffer)
(global-set-key (kbd "M-<down>") #'end-of-buffer)
(global-set-key (kbd "C-k") #'kill-whole-line)

(global-set-key (kbd "C-o") #'consult-buffer)
(global-set-key (kbd "M-/") #'comment-line)
(global-set-key (kbd "s-/") #'hippie-expand)
(global-set-key (kbd "C-x 3")
                (lambda ()
                  (interactive)
                  (split-window-right)
                  (other-window 1)))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun open-line-below ()
  "Open new line below"
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "M-<return>") #'open-line-below)

(defun kill-region-smart ()
  "Cut the active region, or the current line if no region is active."
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-region)
    (kill-whole-line)))
(global-set-key (kbd "M-x") #'kill-region-smart)

(defun kill-ring-save-smart ()
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-ring-save)
    (save-excursion
      (beginning-of-line)
      (copy-region-as-kill (line-beginning-position)
                           (line-beginning-position 2)))))
(global-set-key (kbd "M-c") #'kill-ring-save-smart)

(global-unset-key (kbd "C-w"))

(defun move-beginning-of-line-smart (arg)
  "Move point back to indentation of beginning of line."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key [remap move-beginning-of-line]
                'move-beginning-of-line-smart)

(defun toggle-fold ()
  (interactive)
  (save-excursion
    (end-of-line)
    (hs-toggle-hiding)))
(global-set-key (kbd "C-f") #'toggle-fold)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(use-package diminish
  :ensure t)
(require 'diminish)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)))

(use-package crux
  :ensure t
  :bind
  ("C-j" . #'crux-top-join-line)
  ("C-c k" . #'crux-kill-other-buffers))

(use-package devil
  :ensure t
  :demand t
  :config
  (diminish 'devil-mode)
  (add-to-list 'devil-translations '(", m x" . "C-c x"))
  (add-to-list 'devil-translations '(", ." . "M-."))
  (add-to-list 'devil-translations '(", l" . "M-g i"))
  (add-to-list 'devil-translations '(", >" . "C-x 4 ."))
  (add-to-list 'devil-translations '(", 2" . "C-x P c"))
  (add-to-list 'devil-translations '(", w" . "M-q"))
  (add-to-list 'devil-repeatable-keys '("%k x `"))
  (global-set-key (kbd "C-2") #'recompile)
  (global-devil-mode 1)

  (global-set-key (kbd "C-x P c") #'project-compile))

(use-package hydra
  :ensure t
  :config)

(use-package avy
  :ensure t
  :bind
  ("s-." . avy-goto-word-1))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode +1))

(use-package yasnippet
  :ensure t
  :demand t
  :config
  (yas-global-mode +1)
  (diminish 'yas-minor-mode))

(defun yas-expand-or-corfu-complete ()
  "Prefer Yasnippet expansion over Corfu completion."
  (interactive)
  (if (yas-expand)
      t
    (corfu-complete)))

(with-eval-after-load 'corfu
  (define-key corfu-map (kbd "TAB") #'yas-expand-or-corfu-complete)
  (define-key corfu-map (kbd "<tab>") #'yas-expand-or-corfu-complete))

(use-package consult
  :ensure t
  :bind
  ("C-x b" . consult-buffer)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ;; M-s bindings in `search-map'
  ("M-s d" . consult-find) ;; Alternative: consult-fd
  ("M-s c" . consult-locate)
  ("M-s g" . consult-ripgrep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  :config)

(use-package magit
  :ensure t
  :custom
  (magit-save-repository-buffers 'dontask)
  (magit-diff-fontify-hunk 'all)
  (magit-diff-specify-hunk-foreground nil)
  (magit-diff-use-indicator-faces t)
  :config
  (defun project-switch-project-magit ()
    ""
    (interactive)
    (let ((default-directory (project-prompt-project-dir))
          (display-buffer-overriding-action '((display-buffer-same-window))))
      (magit-project-status)))

  (keymap-set project-prefix-map "p" #'project-switch-project-magit))

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  :config
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package multiple-cursors
  :ensure t
  :demand t
  :bind
  ("M-d" . #'mc/mark-next-like-this-word)
  :config
  (set-face-attribute 'mc/cursor-face nil
                      :background (face-background 'cursor)
                      :foreground (face-foreground 'cursor))

  (defvar my/cursor-before nil)
  (add-hook 'multiple-cursors-mode-enabled-hook
            (lambda ()
              (unless my/cursor-before
                (setq my/cursor-before (face-background 'cursor))
                (set-face-attribute 'cursor nil :background "#DFAF8F"))))

  (add-hook 'multiple-cursors-mode-disabled-hook
            (lambda ()
              (when my/cursor-before
                (set-face-attribute 'cursor nil :background my/cursor-before)
                (setq my/cursor-before nil)))))
(use-package expand-region
  :ensure t
  :bind
  ("s-d" . #'er/expand-region))

(use-package zenburn-theme
  :ensure t)
(use-package super-save
  :ensure t
  :demand t
  :config
  (super-save-mode +1)
  (diminish 'super-save-mode))

(use-package better-jumper
  :ensure t
  :bind
  ("M-[" . #'better-jumper-jump-backward)
  ("M-]" . #'better-jumper-jump-forward)
  :config
  (diminish 'better-jumper-mode)
  (better-jumper-mode 1)

  (with-eval-after-load 'xref
    (advice-add #'xref-push-marker-stack :override
                #'better-jumper-set-jump)))

(use-package breadcrumb
  :vc (:url "https://github.com/joaotavora/breadcrumb.git" :rev :newest)
  :ensure t
  :config
  (setq-default frame-title-format
                '((:eval (breadcrumb--header-line)))))

(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode))

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)
  (global-set-key (kbd "C-p") #'eglot-format-buffer)
  (add-to-list 'eglot-server-programs
               '(zig-mode . ("~/bin/zls-0.14.0"))))

(use-package zig-mode
  :ensure t
  :config
  (add-hook 'zig-mode-hook #'eglot-ensure))

(use-package rust-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-fontify-code-blocks-natively t)
  :config
  (add-to-list 'auto-mode-alist '("\\.djot\\'" . markdown-mode)))

;; (use-package paredit
;; :ensure t
;; :hook (emacs-lisp-mode . paredit-mode))

(defun visit-init-file ()
  "Open the user's Emacs init file."
  (interactive)
  (find-file user-init-file))

(defun switch-to-theme (theme)
  "Disable all enabled themes and load THEME."
  (interactive
   (list
    (intern
     (completing-read
      "Theme: "
      (mapcar #'symbol-name (custom-available-themes))
      nil t))))

  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(switch-to-theme 'zenburn)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("745f8c882e6edae45476e93f7b47c5bd4a4dc98c65494672ddcd291359935a3a" default))
 '(package-selected-packages
   '(avy better-jumper breadcrumb consult corfu crux devil diff-hl diminish
         djot-mode expand-region hydra jinx magit markdown-mode multiple-cursors
         orderless paredit rust-mode super-save vertico yasnippet yasnippet-capf
         zenburn-theme zig-mode))
 '(package-vc-selected-packages
   '((breadcrumb :url "https://github.com/joaotavora/breadcrumb.git")))
 '(safe-local-variable-values '((eglot-server-programs (zig-mode "~/bin/zls-0.14.0")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)

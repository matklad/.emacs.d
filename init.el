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
      compilation-auto-jump-to-first-error t)

(setq-default indent-tabs-mode nil
	          tab-width 4
	          fill-column 80)

(add-to-list 'default-frame-alist '(font . "JetBrains Mono-16"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(global-set-key (kbd "M-z") #'undo)
(global-set-key (kbd "M-c") #'copy-region-as-kill)
(global-set-key (kbd "M-v") #'yank)
(defun cut-line-or-region ()
  "Cut the active region, or the current line if no region is active."
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-region)
    (kill-whole-line)))
(global-set-key (kbd "M-x") #'cut-line-or-region)
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

(global-set-key (kbd "C-o") #'project-find-file)

(defun open-line-below ()
  "Open new line below"
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "M-<return>") #'open-line-below)

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

(use-package devil
  :ensure t
  :demand t
  :config
  (diminish 'devil-mode)
  (add-to-list 'devil-translations '(", m x" . "C-c x"))
  (add-to-list 'devil-translations '(", ." . "M-."))
  (add-to-list 'devil-translations '(", >" . "C-x 4 ."))
  (add-to-list 'devil-repeatable-keys '("%k x `"))
  (global-devil-mode 1))

(use-package hydra
  :ensure t
  :config
  (defhydra project-hydra (:color teal)
    ("f" project-find-file "find file")
    ("g" project-find-regexp "find regexp")
    ("d" project-dired "dired")
    ("b" project-switch-to-buffer "switch buffer")
    ("p" project-switch-project "switch project")
    ("k" project-kill-buffers "kill buffers")
    ("c" project-compile "compile")
    ("e" project-eshell "eshell")
    ("v" project-vc-dir "vc dir")
    ("x" project-shell-command "shell command")
    ("X" project-async-shell-command "async shell command"))

  (global-set-key (kbd "C-x p") #'project-hydra/body))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package magit
  :ensure t
  :custom
  (magit-diff-fontify-hunk 'all)
  (magit-diff-specify-hunk-foreground nil)
  (magit-diff-use-indicator-faces t)))
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  :config
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package multiple-cursors
  :ensure t)
(use-package zenburn-theme
  :ensure t)
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  (diminish 'super-save-mode))

(use-package yasnippet
  :ensure t
  :demand t
  :config
  (yas-global-mode +1))

(require 'eglot)
(require 'zig-mode)

(add-hook 'zig-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs
               '(zig-mode . ("~/bin/zls-0.14.0")))

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

(switch-to-theme 'leuven)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("745f8c882e6edae45476e93f7b47c5bd4a4dc98c65494672ddcd291359935a3a" default))
 '(package-selected-packages
   '(corfu devil diff-hl diminish hydra magit multiple-cursors orderless super-save
           vertico yasnippet zenburn-theme zig-mode))
 '(safe-local-variable-values '((eglot-server-programs (zig-mode "~/bin/zls-0.14.0")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)

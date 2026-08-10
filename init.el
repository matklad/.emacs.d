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
      auto-save-visited-interval 1
      after-focus-change-function (lambda () (save-some-buffers t))
      zig-format-on-save nil
      )

(setq-default indent-tabs-mode nil
	          tab-width 4
	          fill-column 80)

(add-to-list 'default-frame-alist '(font . "JetBrains Mono-16"))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(delete-selection-mode 1)
(electric-pair-mode 1)
(recentf-mode 1)
(global-hl-line-mode 1)
(auto-save-visited-mode 1)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

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
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(global-set-key (kbd "M-<left>")  #'beginning-of-line)
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

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (pkg '(zig-mode magit multiple-cursors devil magit zenburn-theme
                        hydra))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))


(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)))

(require 'devil)
(global-set-key (kbd "C-c x") #'execute-extended-command)

(add-to-list 'devil-translations '(", m x" . "C-c x"))
(add-to-list 'devil-translations '(", ." . "M-."))
(global-devil-mode 1)

(require 'hydra)

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

(global-set-key (kbd "C-x p") #'project-hydra/body)

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

(require 'eglot)
(require 'zig-mode)

(add-hook 'zig-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs
               '(zig-mode . ("~/bin/zls-0.16.0")))

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
   '(corfu devil hydra magit multiple-cursors orderless vertico zenburn-theme
           zig-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

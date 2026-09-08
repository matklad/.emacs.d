;; -*- lexical-binding: t; -*-
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(setopt mac-command-modifier 'meta
        mac-option-modifier 'super
        ring-bell-function #'ignore
        use-short-answers t
        save-interprogram-paste-before-kill t
        kill-do-not-save-duplicates t
        ffap-machine-p-known 'reject
        window-combination-resize t
        sentence-end-double-space nil
        tab-always-indent 'complete
        line-number-mode t
        column-number-mode t
        mode-line-collapse-minor-modes t
        x-underline-at-descent-line nil
        switch-to-buffer-obey-display-actions t
        show-paren-delay 0
        show-paren-mode t
        show-paren-style 'expression
        show-paren-context-when-offscreen 'overlay
        scroll-preserve-screen-position 1
        scroll-conservatively 10
        scroll-margin 15
        scroll-error-top-bottom t
        custom-file (expand-file-name "custom.el" "~/.cache/emacs/")
        backup-directory-alist `(("." . ,(expand-file-name "backups/" "~/.cache/emacs/")))
        auto-save-file-name-transforms `((".*"  ,(expand-file-name "autosave/" "~/.cache/emacs/") t))
        make-backup-files nil
        create-lockfiles nil
        require-final-newline t
        inhibit-startup-screen t
        warning-minimum-level :error
        comment-empty-lines t
        list-matching-lines-default-context-lines 2
        shell-command-prompt-show-cwd t)

(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80
              display-fill-column-indicator-column 100)

(global-auto-revert-mode t)
(savehist-mode +1)
(blink-cursor-mode -1)
(delete-selection-mode +1)
(electric-pair-mode +1)
(recentf-mode +1)
(global-hl-line-mode +1)
(auto-save-visited-mode +1)
(which-key-mode +1)
(global-display-fill-column-indicator-mode +1)
(pixel-scroll-precision-mode +1)

(add-hook 'prog-mode-hook #'hs-minor-mode)

(keymap-set key-translation-map "ESC" "C-g")
(keymap-global-set "M-z" #'undo-only)
(keymap-global-set "M-S-z" #'undo-redo)
(keymap-global-set "M-v" #'yank)
(keymap-global-set "M-a" #'mark-whole-buffer)
(keymap-global-set "C-c x" #'execute-extended-command)
(keymap-global-set "C-x k" #'kill-current-buffer)

(keymap-global-set "M-<left>"  #'move-beginning-of-line)
(keymap-global-set "M-<right>" #'end-of-line)
(keymap-global-set "M-<kp-delete>" #'kill-line)

(keymap-global-set "s-<left>"  #'backward-word)
(keymap-global-set "s-<right>" #'forward-word)
(keymap-global-set "s-<backspace>" #'backward-kill-word)
(keymap-global-set "s-<kp-delete>" #'kill-word)

(keymap-global-set "M-<up>" #'beginning-of-buffer)
(keymap-global-set "M-<down>" #'end-of-buffer)

(keymap-global-set "M-/" #'comment-line)
(keymap-global-set "s-/" #'hippie-expand)
(keymap-global-set "C-<tab>" #'other-window)
(keymap-global-set "s-SPC" #'cycle-spacing)

(keymap-global-unset "C-w")
(keymap-global-unset "C-x m")
(keymap-global-unset "C-e")
(keymap-global-unset "s-t")
(keymap-global-unset "C-z")
(keymap-global-unset "C-x o")

(keymap-global-set "C-x 3"
                (lambda ()
                  (interactive)
                  (split-window-right)
                  (other-window 1)))

(defun kill-region-smart ()
  "Cut the active region, or the current line if no region is active."
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-region)
    (kill-whole-line)))
(keymap-global-set "M-x" #'kill-region-smart)

(defun kill-ring-save-smart ()
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-ring-save)
    (save-excursion
      (beginning-of-line)
      (copy-region-as-kill (line-beginning-position)
                           (line-beginning-position 2)))))
(keymap-global-set "M-c" #'kill-ring-save-smart)

(defun toggle-fold ()
  (interactive)
  (save-excursion
    (end-of-line)
    (hs-toggle-hiding)))
(keymap-global-set "C-f" #'toggle-fold)

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t))

(use-package corfu
  :init
  (global-corfu-mode +1)
  :custom
  (corfu-auto t))

(use-package isearch
  :ensure nil
  :bind
  (:map isearch-mode-map
        ("s-d" . isearch-forward-thing-at-point))
  :custom
  (lazy-count-prefix-format "(%s/%s) ")
  (isearch-lazy-count t)
  (isearch-allow-motion t)
  (isearch-allow-scroll t)
  (isearch-repeat-on-direction-change t)
  (isearch-wrap-pause 'no-ding))

(use-package compile
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-ask-about-save nil)
  (compilation-auto-jump-to-first-error nil)
  :hook (compilation-filter . ansi-color-compilation-filter)
  :config
  (define-key compilation-mode-map (kbd "C-o") nil))

(use-package crux
  :bind
  ("C-j" . #'crux-top-join-line)
  ("C-c k" . #'crux-kill-other-buffers)
  ("M-<return>" . #'crux-smart-open-line)
  ("M-<backspace>" . #'crux-kill-whole-line)
  ("C-k" . #'crux-kill-whole-line)
  ("<remap> <move-beginning-of-line>" . #'crux-move-beginning-of-line))

(use-package zop-to-char
  :ensure t)

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))


(defun my/yank-indent-advice (&rest _args)
  "Indent yanked text if in a programming mode and not too large."
  (when (and (not (member major-mode '(conf-mode coffee-mode haml-mode
                                        python-mode slim-mode yaml-mode
                                        yaml-ts-mode)))
             (derived-mode-p 'prog-mode)
             (<= (- (region-end) (region-beginning)) 1000))
    (let ((transient-mark-mode nil))
      (indent-region (region-beginning) (region-end) nil))))

(advice-add 'yank :after #'my/yank-indent-advice)
(advice-add 'yank-pop :after #'my/yank-indent-advice)

(use-package whitespace-cleanup-mode
  :custom
  (whitespace-cleanup-mode-preserve-point t)
  (whitespace-cleanup-mode-only-if-initially-clean nil)
  :config
  (global-whitespace-cleanup-mode +1))

(use-package dired-subtree
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil)
  (dired-auto-revert-buffer t)
  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle))
  :config)

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map
              ("C-o". nil)
              ("<return>" . dired-find-file-other-window)
              ("S-<return>" . dired-display-file))
  :config
  (defun dired-project ()
    "Open Dired for the current project root in another window."
    (interactive)
    (dired-other-window (project-root (project-current t))))
  (put 'dired-find-alternate-file 'disabled nil))

(use-package devil
  :vc (:url "https://github.com/fbrosda/devil"
            :branch "which-key-support"
            :rev :newest)
  :demand t
  :config
  (add-to-list 'devil-translations '(", m x" . "C-c x"))
  (add-to-list 'devil-translations '(", ." . "M-."))
  (add-to-list 'devil-translations '(", l" . "C-, l"))
  (add-to-list 'devil-translations '(", >" . "C-x 4 ."))
  (add-to-list 'devil-translations '(", w" . "M-q"))
  (add-to-list 'devil-repeatable-keys '("%k x `"))
  (global-devil-mode +1)
  (setq devil-repeatable-keys
        (assoc-delete-all "%k s" devil-repeatable-keys))

  (add-to-list 'devil-translations '(", z" . "C-, z"))
  (keymap-global-set "C-, z" #'zop-to-char)

  (add-to-list 'devil-translations '(", Z" . "C-, Z"))
  (keymap-global-set "C-, Z" #'zop-up-to-char)

  (add-to-list 'devil-translations '(", 1" . "C-, 1"))
  (keymap-global-set "C-, 1" #'dired-project)

  (add-to-list 'devil-translations '(", 2" . "C-, 2"))
  (keymap-global-set "C-, 2" #'project-compile)

  (add-to-list 'devil-translations '(", 3" . "C-x g"))

  (defun visit-init-file ()
    "Open the user's Emacs init file."
    (interactive)
    (find-file user-init-file))
  (add-to-list 'devil-translations '(", 6" . "C-, 6"))
  (global-set-key (kbd "C-, 6") #'visit-init-file))

(use-package hydra
  :config)

(use-package avy
  :bind
  ("s-." . avy-goto-word-1))

(use-package yasnippet
  :demand t
  :config
  (yas-global-mode +1))

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
  :init
  (defun my/consult-to-project-find-file ()
    "Switch from `consult-buffer` to `project-find-file`, keeping the query."
    (interactive)
    (let ((query (minibuffer-contents)))
      (add-hook
       'minibuffer-exit-hook
       (lambda ()
         (run-at-time
          0 nil
          (lambda ()
            (minibuffer-with-setup-hook
                (lambda ()
                  (insert query))
              (project-find-file)))))
       nil t)
      (abort-recursive-edit)))
  :bind
  ("C-o" . consult-buffer)
  ("C-S-o" . find-file)
  ("M-S-v" . consult-yankg-pop)
  ("C-, l" . consult-imenu)
  ("C-s" . consult-line)
  (:map vertico-map
        ("C-o" . my/consult-to-project-find-file)))

(use-package deadgrep
  :bind
  ("C-S-s" . deadgrep))

(use-package magit
  :custom
  (magit-save-repository-buffers 'dontask)
  (magit-diff-fontify-hunk 'all)
  (magit-diff-specify-hunk-foreground nil)
  (magit-diff-use-indicator-faces t)
  (magit-prefer-remote-upstream t)
  (magit-diff-visit-prefer-worktree t)

  :bind
  (:map magit-hunk-section-map
              ("<return>" . (lambda ()
                              (interactive)
                              (magit-diff-visit-file t))))
  (:map magit-diff-section-map
        ("<return>" . (lambda ()
                        (interactive)
                        (magit-diff-visit-file t))))

  :config
  (defun project-switch-project-magit ()
    ""
    (interactive)
    (let ((default-directory (project-prompt-project-dir))
          (display-buffer-overriding-action '((display-buffer-same-window))))
      (magit-project-status)))

  (keymap-set project-prefix-map "p" #'project-switch-project-magit)
  (keymap-unset magit-status-mode-map "C-<tab>")
  (keymap-unset magit-revision-mode-map "C-<tab>")
  (keymap-unset magit-diff-mode-map "C-<tab>")
  (keymap-set magit-status-mode-map "C-c <tab>" #'magit-section-cycle))

(use-package git-link
  :ensure t)

(use-package diff-hl
  :init
  (global-diff-hl-mode)
  :bind
  ("C-e r" . #'diff-hl-revert-hunk)
  :config
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package multiple-cursors
  :demand t
  :bind
  ("M-d" . #'mc/mark-next-like-this-word)
  ("M-D" . #'mc/unmark-next-like-this)
  ("C-e c" . #'mc/edit-lines)
  (:map mc/keymap ("M-v" . nil))


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
  :custom
  (expand-region-contract-fast-key "D")
  :bind
  ("s-d" . #'er/expand-region))

(use-package super-save
  :demand t
  :config
  (super-save-mode +1))

(use-package better-jumper
  :bind
  ("M-[" . #'better-jumper-jump-backward)
  ("M-]" . #'better-jumper-jump-forward)
  :config
  (better-jumper-mode 1)

  (with-eval-after-load 'xref
    (advice-add #'xref-push-marker-stack :override
                #'better-jumper-set-jump)))

(use-package breadcrumb
  :vc (:url "https://github.com/joaotavora/breadcrumb.git" :rev :newest)
  :config
  (setq-default frame-title-format
                '((:eval (breadcrumb--header-line)))))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind
  ("C-e t" . #'jinx-correct)
  :config)

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)
  (add-to-list 'eglot-ignored-server-capabilities :codeActionProvider)
  (global-set-key (kbd "C-p") #'eglot-format-buffer)
  (add-to-list 'eglot-server-programs
               '(zig-ts-mode . ("~/bin/zls-0.14.0"))))


(defun zig-ts--test-node-p (node)
  "Return non-nil if NODE is a Zig function declaration."
  (equal (treesit-node-type node) "test_declaration"))

(defun zig-ts--function-node-p (node)
  "Return non-nil if NODE is a Zig function declaration."
  (equal (treesit-node-type node) "function_declaration"))

(defun zig-ts--contains-function-p (node)
  "Return non-nil if NODE contains a nested function declaration."
  (seq-some (lambda (child)
              (or (zig-ts--function-node-p child)
                  (zig-ts--contains-function-p child)))
            (treesit-node-children node)))

(defun zig-ts--leaf-function-node-p (node)
  (and (zig-ts--function-node-p node)
       (not (zig-ts--contains-function-p node))))

(defun zig-ts--leaf-functions-and-tests (node)
  "Return leaf function declarations below NODE."
  (if (or (zig-ts--test-node-p node)
          (zig-ts--leaf-function-node-p node))
      (list node)

    (apply #'append
           (mapcar #'zig-ts--leaf-functions-and-tests
                   (treesit-node-children node)))))

(defun my/fold-functions ()
  "Fold leaf Zig functions, or unfold everything if anything is folded."
  (interactive)
  (unless (derived-mode-p 'zig-ts-mode)
    (user-error "This command requires zig-ts-mode"))
  (save-excursion
    (let ((overlays (overlays-in (point-min) (point-max)))
          (folded nil))
      (while (and overlays (not folded))
        (when (overlay-get (car overlays) 'hs)
          (setq folded t))
        (setq overlays (cdr overlays)))
      (if folded
          (hs-show-all)
        (dolist (f
                 (zig-ts--leaf-functions-and-tests (treesit-buffer-root-node 'zig)))
          (when-let ((body (treesit-node-child f -1 )))
            (goto-char (treesit-node-start body))
            (hs-hide-block)))))))

(keymap-global-set "C-S-f" #'my/fold-functions)

(use-package zig-ts-mode
  :vc ( :url "https://codeberg.org/meow_king/zig-ts-mode"
        :rev :newest)
  :hook (zig-ts-mode . eglot-ensure)
  :custom
  (zig-format-on-save t)
  :config
  (font-lock-add-keywords 'zig-ts-mode
   '(("\\<assert\\>" . font-lock-function-name-face)
     ("\\<unreachable\\>" . font-lock-function-name-face))))

(use-package rust-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-fontify-code-blocks-natively t)
  :config
  (add-to-list 'auto-mode-alist '("\\.dj\\'" . markdown-mode)))

(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default))

(define-derived-mode peg-mode fundamental-mode "PEG"
  "Major mode for PEG grammar files."
  (setq-local imenu-generic-expression
              '(("->"
                 "^\\([[:alnum:]_]+\\)[ \t\n]*<-[ \t]*"
                 1))))

(add-to-list 'auto-mode-alist '("\\.peg\\'" . peg-mode))

;; (use-package paredit
;; :ensure t
;; :hook (emacs-lisp-mode . paredit-mode))

(use-package ultra-scroll
  :ensure t
  :config
  (ultra-scroll-mode +1))

(defun switch-theme (theme)
  (interactive
   (list (intern (completing-read "Theme: " (mapcar #'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (enable-theme theme)
  (set-frame-parameter nil 'ns-appearance (if (eq theme 'zenburn) 'dark 'light)))

(load-theme 'whiteboard t t)
(custom-theme-set-faces
 'whiteboard
 '(hl-line ((t (:background "gainsboro")))))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t t))

(enable-theme 'zenburn)
(zenburn-with-color-variables
  (custom-theme-set-faces
   'zenburn
   '(region ((t (:background "#3F5F3F"))))

   `(magit-diff-removed ((t (:background ,zenburn-red-6))))
   `(magit-diff-removed-highlight ((t (:background ,zenburn-red-5))))
   `(magit-diff-removed-indicator ((t (:background unspecified))))

   `(magit-diff-added ((t (:background ,zenburn-green-5))))
   `(magit-diff-added-highlight ((t (:background ,zenburn-green-4))))
   `(magit-diff-added-indicator ((t (:background unspecified)))))

   )


(when (file-exists-p custom-file)
  (load custom-file))
(setq gc-cons-threshold 800000)

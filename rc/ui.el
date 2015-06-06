(require 'use-package)

(use-package zenburn-theme
  :ensure t)

(use-package base16-theme
  :ensure t)

(load-theme 'base16-eighties-dark t)


(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(column-number-mode t)
(blink-cursor-mode -1)
(global-hl-line-mode t)

(setq inhibit-startup-screen t)
(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-12"))
(add-to-list 'default-frame-alist '(cursor-color . "#FFFFFF"))

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config (golden-ratio))


(use-package smooth-scrolling
  :ensure t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

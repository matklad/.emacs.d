(require 'use-package)

(use-package solarized-theme
  :ensure t
  :defer t)

(use-package zenburn-theme
  :ensure t
  :defer t)

(use-package base16-theme
  :ensure t)

(defun switch-theme (name)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (customize-save-variable
   `custom-enabled-themes (list name)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(column-number-mode t)
(blink-cursor-mode -1)
(global-hl-line-mode t)

(setq inhibit-startup-screen t)
(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'default-frame-alist '(font . "Hack 10"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-frame-font "Hack 10")


(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config (golden-ratio))


(use-package smooth-scrolling
  :ensure t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

(require 'use-package)

(defun turn-on-whitespace ()
  (whitespace-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(add-hook 'prog-mode-hook 'turn-on-whitespace)


(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)
                flycheck-clang-language-standard "c++11")
  (global-flycheck-mode))


;; Python


(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package company-anaconda
  :ensure t
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package f
  :ensure t)

(defun pyenv-from-file ()
  (let ((current-file (buffer-file-name))
        (file-name ".python-version"))
    (when current-file
      (let* ((conf-dir (locate-dominating-file current-file file-name))
             (conf-file (concat conf-dir file-name)))
        (pyenv-mode-set
         (string-trim (f-read conf-file)))))))

(use-package pyenv-mode
  :ensure t
  :config
  (pyenv-mode)
  (add-hook 'python-mode-hook 'pyenv-from-file))

(defun electric-python-hook ()
  (setq-local electric-layout-rules
              '((?: . (lambda ()
                        (and (zerop (first (syntax-ppss)))
                             (python-info-statement-starts-block-p)
                             'after)))))
  (add-hook 'post-self-insert-hook
            #'electric-layout-post-self-insert-function nil 'local))

(add-hook 'python-mode-hook 'electric-python-hook)


;; Lisp



(use-package paredit
  :diminish paredit-mode
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           'enable-paredit-mode)
  (add-hook 'paredit-mode-hook '(lambda () (setq kill-whole-line nil))))

(use-package rainbow-delimiters
  :ensure t)

(defun my-clojure-hook ()
  (enable-paredit-mode)
  (rainbow-delimiters-mode)
  (setq company-idle-delay 0)
  (clj-refactor-mode t)
  (cljr-add-keybindings-with-prefix "C-c C-m"))


(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'my-clojure-hook))


(use-package clj-refactor
  :ensure t)


(use-package kibit-mode
  :ensure t)

(use-package cider
  :ensure t
  :pin melpa-stable
  :config
  (define-key cider-mode-map (kbd "C-M-i") 'company-complete)
  (setq cider-stacktrace-default-filters '(java repl tooling dup)))


;; C/C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defconst my-cc-style
  '("bsd"
    (c-basic-offset . 4)
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "my-cc-style" my-cc-style)

(setq-default c-default-style "my-cc-style")



;; Haskell

(use-package haskell-mode
  :ensure t)


(use-package hi2
  :ensure t
  :config
  (setq hi2-show-indentations nil)
  (add-hook 'haskell-mode-hook
            '(lambda ()
               (yas-minor-mode -1)
               (turn-on-hi2))))

;; Latex

(defun run-latex ()
  (interactive)
  (let ((TeX-save-query nil)) (TeX-save-document ""))
  (TeX-command-menu "LaTeX"))

(defun toggle-formula ()
  (interactive)
  (if (eq nil (search-forward "$" (+ (point) 4) 't 1))
      (progn
        (insert "$$")
        (backward-char)
        (deactivate-input-method))
    (toggle-input-method)))


(defun latex-hook ()
  (local-set-key (kbd "C-x C-s") 'run-latex)
  (local-set-key (kbd "C-c C-f") 'toggle-formula)
  (flychek-mode nil)
  (ispell-change-dictionary "ru-yo"))

(use-package tex
  :ensure auctex
  :config
  (setq-default TeX-engine 'xetex
                TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook 'latex-hook))

;; Web

(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package emmet-mode
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 2)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")))
  (sp-local-pair 'web-mode "{{" "}}")
  (sp-local-pair 'web-mode "{%" "%}" :insert "C-c b"))

(use-package coffee-mode
  :ensure t
  :config
  (setq-default coffee-tab-width 2))

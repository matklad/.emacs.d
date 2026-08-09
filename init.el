(setq ring-bell-function #'ignore
      tab-always-indent 'complete
      scroll-conservatively 0
      scroll-margin 3
      backup-directory-alist `(("." . ,(expand-file-name "backups/" "~/.cache/emacs/")))
      auto-save-file-name-transforms `((".*"  ,(expand-file-name "autosave/" "~/.cache/emacs/") t))
      mac-command-modifier 'meta
      mac-option-modifier 'super
      )

(setq-default indent-tabs-mode nil
	          tab-width 4
	          fill-column 80)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(delete-selection-mode 1)
(electric-pair-mode 1)
(recentf-mode 1)
(global-hl-line-mode 1)

(load-theme 'leuven t)


(global-set-key (kbd "M-<left>")  #'beginning-of-line)
(global-set-key (kbd "M-<right>") #'end-of-line)
(global-set-key (kbd "M-<backspace>") #'kill-whole-line)
(global-set-key (kbd "M-<kp-delete>") #'kill-line)
undo)

(global-set-key (kbd "s-<left>")  #'backward-word)
(global-set-key (kbd "s-<right>") #'forward-word)
(global-set-key (kbd "s-<backspace>") #'backward-kill-word)
(global-set-key (kbd "s-<kp-delete>") #'kill-word)

(defun visit-init-file ()
  "Open the user's Emacs init file."
  (interactive)
  (find-file user-init-file))

(custom-set-variables
 ; ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(zig-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

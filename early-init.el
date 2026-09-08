;;; -*- lexical-binding: t -*-
;; From bedrock (https://codeberg.org/ashton314/emacs-bedrock), thanks!
(setq gc-cons-threshold 10000000
      byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent
      use-package-always-ensure t
      frame-resize-pixelwise t
      default-frame-alist '((ns-appearance . dark)
                            (ns-transparent-titlebar . t)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (font . "JetBrains Mono-16")
                            (fullscreen . maximized))
      initial-frame-alist '((background-color . "#3F3F3F")
                            (foreground-color . "#DCDCCC")))

(advice-add #'display-startup-echo-area-message :override #'ignore)
(tool-bar-mode -1)

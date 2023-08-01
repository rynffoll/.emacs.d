;;; -*- lexical-binding: t; no-byte-compile: t -*-

(setq default-directory "~/")

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (let ((init-time (float-time (time-subtract after-init-time before-init-time)))
                    (packages  (length package-activated-list))
                    (gc-time   (float-time gc-elapsed))
                    (gc-count  gcs-done))
                (message "Emacs ready (init time = %.2fs, packages = %d, gc time = %.2fs, gc count = %d)."
                         init-time packages gc-time gc-count))))

(setq frame-inhibit-implied-resize t)

(add-to-list 'default-frame-alist '(left . 0.5))
(add-to-list 'default-frame-alist '(top . 0.5))
(add-to-list 'default-frame-alist '(width . 0.75))
(add-to-list 'default-frame-alist '(height . 0.9))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(internal-border-width . 0))
(add-to-list 'default-frame-alist '(tabs
                                    (current-tab
                                     (name . "main")
                                     (explicit-name . t))))
;; (add-to-list 'default-frame-alist '(fullscreen . fullboth))
(add-to-list 'default-frame-alist '(font . "JetBrains Mono 14"))

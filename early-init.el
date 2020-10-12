;;; -*- lexical-binding: t; no-byte-compile: t -*-

(setq default-directory "~/")

(setq package-enable-at-startup nil)

(defun -override-gc-setup ()
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6))

(defun -restore-gc-setup ()
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(-override-gc-setup)

(add-hook 'emacs-startup-hook #'-restore-gc-setup)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "Emacs ready (init time = %s, gc time = %s, gc count = %d)."
                       (format "%.2fs"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       (format "%.2fs" (float-time gc-elapsed))
                       gcs-done)))

(add-hook 'minibuffer-setup-hook #'-override-gc-setup)
(add-hook 'minibuffer-exit-hook #'-restore-gc-setup)

(defvar -file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq file-name-handler-alist -file-name-handler-alist)))

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
(add-to-list 'default-frame-alist '(font . "JetBrains Mono 15"))

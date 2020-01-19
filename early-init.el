;;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1)))

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "Emacs ready (init time = %s, gc time = %s, gc count = %d)."
                       (format "%.2fs"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       (format "%.2fs" (float-time gc-elapsed))
                       gcs-done)))

(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq file-name-handler-alist my--file-name-handler-alist)))

(add-to-list 'default-frame-alist '(left . 0.5))
(add-to-list 'default-frame-alist '(top . 0.5))
(add-to-list 'default-frame-alist '(width . 0.75))
(add-to-list 'default-frame-alist '(height . 0.9))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(font . "Fira Mono Medium 14"))

(setq frame-inhibit-implied-resize t)

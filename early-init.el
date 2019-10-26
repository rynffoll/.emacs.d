;;; -*- lexical-binding: t; -*-

;; Package Manager
(setq package-enable-at-startup nil)

;; GC tweaks
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

;; File handler tweaks
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq file-name-handler-alist my--file-name-handler-alist)))

;; UI
(add-to-list 'default-frame-alist '(left . 0.5))
(add-to-list 'default-frame-alist '(top . 0.5))
(add-to-list 'default-frame-alist '(width . 0.75))
(add-to-list 'default-frame-alist '(height . 0.9))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(font . "Iosevka Medium 14"))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

(load-theme 'tsdh-light)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

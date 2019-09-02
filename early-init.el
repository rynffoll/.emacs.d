;;; -*- lexical-binding: t; -*-

;; Package Manager
(setq package-enable-at-startup nil
      package-quickstart t)

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
(push '(left . 0.5) default-frame-alist)
(push '(top . 0.5) default-frame-alist)
(push '(width . 0.75) default-frame-alist)
(push '(height . 0.9) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

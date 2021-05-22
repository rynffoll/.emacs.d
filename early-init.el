;;; -*- lexical-binding: t; no-byte-compile: t -*-

(let ((arch (shell-command-to-string "arch")))
  (when (equal arch "arm64")
    (setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/11:/opt/homebrew/opt/libgccjit/lib/gcc/11:/opt/homebrew/opt/gcc/lib/gcc/11/gcc/aarch64-apple-darwin20/11.1.0:/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"))
  (when (equal arch "i386")
    (setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin20/11.1.0:/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib")))

(setq default-directory "~/")

(setq package-enable-at-startup nil)

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "Emacs ready (init time = %s, gc time = %s, gc count = %d)."
                       (format "%.2fs"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       (format "%.2fs" (float-time gc-elapsed))
                       gcs-done)))

(defvar -file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq file-name-handler-alist -file-name-handler-alist)))

(setq site-run-file nil)

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

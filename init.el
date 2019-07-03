;;; -*- lexical-binding: t; -*-

(setq debug-on-error t)
(setq debug-on-quit t)

(setq load-prefer-newer t)
(setq message-log-max t) ;; we don't want to lose any startup log info

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1)))

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)))

(require 'package)
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/")))
(setq package-archive-priorities
      '(("melpa-stable" . 5)
        ("gnu"          . 5)
        ("melpa"        . 10)))
(package-initialize)

(setq package-enable-at-startup nil
      package--initialized t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-compute-statistics t)
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.01)
(setq use-package-hook-name-suffix nil)

(use-package quelpa
  :defer t)

(use-package quelpa-use-package
  :custom
  (quelpa-use-package-inhibit-loading-quelpa t "Improve startup performance"))

(use-package use-package-ensure-system-package)

(use-package general
  :config
  (general-create-definer my/leader-def
    :states '(normal visual insert emacs motion)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-m")
  (general-create-definer my/local-leader-def
    :states '(normal visual insert emacs motion)
    :keymaps 'override
    :prefix "SPC m"
    :non-normal-prefix "M-m m")
  (general-define-key
   :states '(normal visual motion)
   :prefix ","
   "" (general-simulate-key "SPC m"))
  (my/leader-def
    "" '(nil :wk "leader")
    "o" '(:ignore t :wk "open")
    "O" '(:ignore t :wk "org")
    "b" '(:ignore t :wk "buffer")
    "c" '(:ignore t :wk "copy")
    "f" '(:ignore t :wk "file")
    "e" '(:ignore t :wk "emacs")
    "g" '(:ignore t :wk "git")
    "/" '(:ignore t :wk "search")
    "j" '(:ignore t :wk "jump")
    "h" '(:ignore t :wk "help")
    "t" '(:ignore t :wk "toggle")
    "i" '(:ignore t :wk "insert")
    "q" '(:ignore t :wk "quit"))
  (my/local-leader-def
    "" '(nil :wk "local leader")))

(use-package evil
  :general
  (evil-insert-state-map
   "C-k" nil)
  :custom
  (evil-want-keybinding nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-emacs-state-cursor 'hbar)
  (evil-mode-line-format nil)
  (evil-symbol-word-search t)
  (evil-move-beyond-eol nil)
  (evil-move-cursor-back t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer nil)
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-magit
  :after magit evil
  :custom
  (evil-magit-want-horizontal-movement t)
  (evil-magit-use-z-for-folds t))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-org
  :after evil org
  :custom
  (evil-org-special-o/O '(item table-row))
  (evil-org-key-theme '(todo textobjects insert navigation heading))
  :hook
  (org-mode-hook . evil-org-mode))

(use-package evil-org-agenda
  :ensure evil-org
  :after evil org-agenda
  :config
  (evil-org-agenda-set-keys))

(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode t))

(use-package emacs
  :ensure nil
  :general
  (my/leader-def
    "qq" 'kill-emacs)
  :custom
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (use-dialog-box nil)
  (enable-recursive-minibuffers t)
  (indent-tabs-mode nil "Don't use tabs")
  (create-lockfiles nil "Stop creating .# files")
  (frame-resize-pixelwise t)
  (window-resize-pixelwise t)
  (inhibit-compacting-font-caches t)
  (scroll-step 1)
  (scroll-preserve-screen-position t)
  (scroll-margin 0)
  (scroll-conservatively 101)
  (ring-bell-function 'ignore)
  (delete-by-moving-to-trash t)
  :hook
  (focus-out-hook . garbage-collect)
  :config
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package files
  :ensure nil
  :custom
  (require-final-newline t)
  (make-backup-files nil "Stop creating backup~ files")
  (auto-save-default nil "Stop creating #autosave# files")
  (enable-local-variables :all)
  (enable-local-eval t))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode))

(use-package savehist
  :ensure nil
  :config
  (savehist-mode))

(use-package saveplace
  :ensure nil
  :config
  (save-place-mode))

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 300)
  :config
  (recentf-mode t))

(use-package iqa
  :defer t
  :general
  (my/leader-def
    "e" '(:ignore t :wk "emacs")
    "ed" 'iqa-find-user-init-directory
    "ee" 'iqa-find-user-init-file
    "er" 'iqa-reload-user-init-file)
  :custom
  (iqa-user-init-file (concat user-emacs-directory "config.org")))

(use-package cus-edit
  :ensure nil
  :general
  (my/leader-def
    "oc" 'customize-group)
  :custom
  (custom-file null-device "Don't store customizations"))

(use-package epa
  :ensure nil
  :defer t
  :custom
  (epa-pinentry-mode 'loopback))

(use-package emacs
  :ensure nil
  :preface
  (defun my/switch-to-scratch () (interactive) (switch-to-buffer "*scratch*"))
  (defun my/switch-to-messages () (interactive) (switch-to-buffer "*Messages*"))
  :general
  (my/leader-def
    "bs" '(my/switch-to-scratch :wk "open scratch")
    "bm" '(my/switch-to-messages :wk "open messages")
    "bR" 'rename-buffer))

(use-package menu-bar
  :ensure nil
  :general
  (my/leader-def
    "bk" 'kill-this-buffer))

(use-package window
  :ensure nil
  :general
  (my/leader-def
    "bb" 'switch-to-buffer
    "bK" 'kill-buffer-and-window))

(use-package ibuffer
  :ensure nil
  :general
  ([remap list-buffers] 'ibuffer)
  (my/leader-def
    "bI" 'ibuffer))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package evil-commands
  :ensure evil
  :after evil
  :general
  (my/leader-def
    "bn" 'evil-buffer-new
    "b]" 'evil-next-buffer
    "b[" 'evil-prev-buffer))

(use-package ibuffer-vc
  :after ibuffer
  :hook
  (ibuffer-hook . (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package window
  :ensure nil
  :general
  (evil-window-map
   "m" 'maximize-window
   "M" 'minimize-window))

(use-package winner
  :ensure nil
  :general
  (evil-window-map
   "u" 'winner-undo
   "U" 'winner-redo)
  :hook
  (after-init-hook . winner-mode))

(use-package winum
  :demand
  :general
  (my/leader-def
    "'" 'winum-select-window-by-number
    "0" 'winum-select-window-0-or-10
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9)
  :custom
  (winum-auto-setup-mode-line nil "For spaceline")
  (winum-scope 'frame-local)
  :config
  (winum-mode))

(use-package shackle
  :custom
  (shackle-default-alignment 'below)
  (shackle-default-size 0.3)
  (shackle-rules '((help-mode :align below :select t)
                   (helpful-mode :align below)
                   (flycheck-error-list-mode :align below)
                   (cider-repl-mode :align below)
                   (ansible-doc-module-mode :align below)
                   ("*Pack*" :align below)
                   ("\\*Async Shell Command\\*.*" :regexp t :ignore t)
                   (Man-mode :align below :select t)
                   ("\\*Man.*\\*" :regexp t :align below :select t)
                   ("*lsp-help*" :align below)
                   ("*Warnings*" :align below)
                   ("*Compile-Log*" :align below)
                   (compilation-mode :align below)
                   ("*company-documentation*" :align below)
                   ("*Go REPL*" :align below)
                   ("\\*docker-compose .*\\*" :regexp t :align below)))
  :config
  (shackle-mode 1))

(use-package eyebrowse
  :commands
  eyebrowse-create-window-config
  :preface
  (defun my/eyebrowse-create-window-config-with-tag ()
    (interactive)
    (let ((tag (read-string "Tag: ")))
      (eyebrowse-create-window-config)
      (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) tag)))
  (defun my/eyebrowse-create-projectile-window-config ()
    (interactive)
    (eyebrowse-create-window-config)
    (let* ((inhibit-quit t)
           (project-name (with-local-quit (projectile-switch-project))))
      (if (> (length project-name) 0)
          (eyebrowse-rename-window-config
           (eyebrowse--get 'current-slot)
           (file-name-nondirectory (directory-file-name project-name)))
        (progn
          (eyebrowse-close-window-config)
          (setq quit-flag nil)))))
  (defun my/eyebrowse-close-other-window-configs ()
    (interactive)
    (when (or (not eyebrowse-close-window-config-prompt)
              (yes-or-no-p "Close other window configs?"))
      (mapcar #'eyebrowse--delete-window-config
              (remove (eyebrowse--get 'current-slot)
                      (mapcar #'car (eyebrowse--get 'window-configs))))))
  :general
  (my/leader-def
    "w" '(:ignore t :wk "workspace")
    "wc" 'eyebrowse-close-window-config
    "w TAB" 'eyebrowse-last-window-config
    "wR" 'eyebrowse-rename-window-config
    "ww" 'eyebrowse-switch-to-window-config
    "w0" 'eyebrowse-switch-to-window-config-0
    "w1" 'eyebrowse-switch-to-window-config-1
    "w2" 'eyebrowse-switch-to-window-config-2
    "w3" 'eyebrowse-switch-to-window-config-3
    "w4" 'eyebrowse-switch-to-window-config-4
    "w5" 'eyebrowse-switch-to-window-config-5
    "w6" 'eyebrowse-switch-to-window-config-6
    "w7" 'eyebrowse-switch-to-window-config-7
    "w8" 'eyebrowse-switch-to-window-config-8
    "w9" 'eyebrowse-switch-to-window-config-9
    "w[" 'eyebrowse-prev-window-config
    "w]" 'eyebrowse-next-window-config
    "wn" 'my/eyebrowse-create-window-config-with-tag
    "wp" 'my/eyebrowse-create-projectile-window-config
    "wC" 'my/eyebrowse-close-other-window-configs)
  :custom
  (eyebrowse-new-workspace t "Clean up and display the scratch buffer")
  (eyebrowse-wrap-around t)
  (eyebrowse-close-window-config-prompt t)
  :config
  (eyebrowse-mode t))

(use-package undo-tree
  :defer t
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-history-directory-alist `(("." . ,temporary-file-directory))))

(use-package paradox
  :general
  (my/leader-def
    "op" 'paradox-list-packages)
  :custom
  (paradox-execute-asynchronously t)
  (paradox-github-token t "Don't ask github token")
  :hook
  (after-init-hook . paradox-enable))

(use-package calendar
  :ensure nil
  :defer t
  :custom
  (calendar-date-style 'iso)
  (calendar-week-start-day 1))

(use-package tramp
  :ensure nil
  :defer t
  :custom
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-aBhl --group-directories-first")
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-recursive-copies 'always "Never prompt for recursive copies of a directory")
  (dired-recursive-deletes 'always "Never prompt for recursive deletes of a directory")
  (dired-hide-details-hide-symlink-targets nil)
  :hook
  (dired-mode-hook . dired-hide-details-mode))

(use-package dired-x
  :ensure nil
  :custom
  (dired-bind-jump nil))

(use-package async
  :after dired
  :config
  (dired-async-mode t))

(use-package dired-hide-dotfiles
  :general
  (:keymaps 'dired-mode-map :states 'normal
            "M-." 'dired-hide-dotfiles-mode))

(use-package dired-subtree
  :defer t
  :preface
  (defun my/dired-subtree-revert ()
    (call-interactively 'revert-buffer)
    (recenter))
  :general
  (:keymaps 'dired-mode-map :states 'normal
            "TAB" 'dired-subtree-toggle)
  :custom
  (dired-subtree-use-backgrounds nil)
  :config
  ;; for treemacs-icons-dired
  (advice-add #'dired-subtree-toggle :after #'my/dired-subtree-revert))

(use-package dired-narrow
  :defer t
  :general
  (:keymaps 'dired-mode-map :states 'normal
            "M-n n" 'dired-narrow
            "M-n f" 'dired-narrow-fuzzy
            "M-n r" 'dired-narrow-regexp))

(use-package pack
  :general
  (:keymaps 'dired-mode-map :states 'normal
            "P" 'pack-dired-dwim)
  :custom
  (pack-dired-default-extension ".zip"))

(use-package dired-git-info
  :general
  (:keymaps 'dired-mode-map :states 'normal
            ")" 'dired-git-info-mode))

(use-package em-smart
  :ensure nil
  :after eshell
  :config (eshell-smart-initialize))

(use-package esh-autosuggest
  :after eshell
  :hook (eshell-mode-hook . esh-autosuggest-mode))

(use-package eshell-fringe-status
  :after eshell
  :hook (eshell-mode-hook . eshell-fringe-status-mode))

(use-package eshell-prompt-extras
  :after eshell
  :custom
  (eshell-highlight-prompt nil)
  (eshell-prompt-function 'epe-theme-lambda))

(use-package shell-pop
  :defer t
  :general
  ("s-t" 'shell-pop)
  :custom
  (shell-pop-full-span t "Spans full width of a window")
  (shell-pop-shell-type '("eshell" "*eshell-pop*" (lambda () (eshell)))))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package with-editor
  :general
  ([remap shell-command]       'with-editor-shell-command)
  ([remap async-shell-command] 'with-editor-async-shell-command)
  :hook
  (shell-mode-hook   . with-editor-export-editor)
  (term-exec-hook    . with-editor-export-editor)
  (eshell-mode-hook  . with-editor-export-editor))

(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
  :hook
  (ediff-prepare-buffer-hook . show-all)
  (ediff-quit-hook . winner-undo))

(use-package ivy
  :general
  (ivy-mode-map
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line)
  (my/leader-def
    "bb" 'ivy-switch-buffer)
  :custom
  (ivy-wrap t)
  (ivy-fixed-height-minibuffer t)
  (ivy-initial-inputs-alist nil "Don't use ^ as initial input")
  (ivy-format-function 'ivy-format-function-line "highlight til EOL")
  (ivy-use-virtual-buffers nil "don't show recent files in switch-buffer")
  (ivy-virtual-abbreviate 'full)
  (ivy-on-del-error-function nil)
  (ivy-use-selectable-prompt t)
  (ivy-re-builders-alist '((counsel-rg . ivy--regex-plus)
                           (swiper     . ivy--regex-plus)
                           (t          . ivy--regex-fuzzy)))
  :hook
  (after-init-hook . ivy-mode))

(use-package swiper
  :general
  (my/leader-def
    "/b" 'swiper))

(use-package smex)

(use-package counsel
  :general
  ([remap describe-face]            'counsel-describe-face)
  ([remap describe-function]        'counsel-describe-function)
  ([remap describe-variable]        'counsel-describe-variable)
  ([remap execute-extended-command] 'counsel-M-x)
  ([remap find-file]                'counsel-find-file)
  ([remap find-library]             'counsel-find-library)
  ([remap imenu]                    'counsel-imenu)
  (my/leader-def
    "." 'counsel-find-file

    "oL" 'counsel-find-library

    "ff" 'counsel-find-file
    "fr" 'counsel-recentf

    "/d" 'counsel-rg

    "tt" 'counsel-load-theme

    "hF" 'counsel-faces)
  :custom
  (counsel-describe-function-function 'helpful-callable)
  (counsel-describe-variable-function 'helpful-variable))

(use-package hydra)

(use-package ivy-hydra
  :after ivy hydra)

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package counsel-projectile
  :after counsel projectile
  :general
  (my/leader-def
    "/p" 'counsel-projectile-rg)
  :config
  (counsel-projectile-mode))

(use-package counsel-tramp
  :defer t)

(use-package ns-win
  :if (memq window-system '(mac ns))
  :ensure nil
  :custom
  (mac-command-modifier 'super))

(use-package files
  :if (memq window-system '(mac ns))
  :ensure nil
  :custom
  (insert-directory-program "gls"))

(use-package browse-url
  :if (file-exists-p "/mnt/c/Windows/System32/cmd.exe")
  :ensure nil
  :custom
  (browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe")
  (browse-url-generic-args '("/c" "start"))
  (browse-url-browser-function 'browse-url-generic))

(use-package menu-bar
  :ensure nil
  :commands clipboard-kill-ring-save
  :preface
  (defun my/copy-whole-buffer ()
    "Copy entire buffer to clipboard"
    (interactive)
    (clipboard-kill-ring-save (point-min) (point-max)))
  :general
  (my/leader-def
    "cb" '(my/copy-whole-buffer :wk "copy whole buffer")))

(use-package copy-as-format
  :general
  (my/leader-def
    "cf" '(:ignore t :wk "copy as format")
    "cff" 'copy-as-format
    "cfa" 'copy-as-format-asciidoc
    "cfb" 'copy-as-format-bitbucket
    "cfd" 'copy-as-format-disqus
    "cfg" 'copy-as-format-github
    "cfl" 'copy-as-format-gitlab
    "cfc" 'copy-as-format-hipchat
    "cfh" 'copy-as-format-html
    "cfj" 'copy-as-format-jira
    "cfm" 'copy-as-format-markdown
    "cfw" 'copy-as-format-mediawiki
    "cfo" 'copy-as-format-org-mode
    "cfp" 'copy-as-format-pod
    "cfr" 'copy-as-format-rst
    "cfs" 'copy-as-format-slack)
  :custom
  (copy-as-format-default "slack" "or Telegram"))

(use-package help
  :ensure nil
  :general
  (my/leader-def
    "hd" 'describe-mode))

(use-package help-fns
  :ensure nil
  :general
  (my/leader-def
    "hf" 'describe-function
    "hv" 'describe-variable))

(use-package man
  :ensure nil
  :general
  (my/leader-def
    "hM" 'man))

(use-package helpful
  :defer t
  :general
  (my/leader-def
    "h." 'helpful-at-point
    "hC" 'helpful-command
    "hc" 'helpful-callable
    "hk" 'helpful-key
    "hm" 'helpful-macro))

(use-package which-key
  :custom
  (which-key-idle-delay 0.3)
  (which-key-sort-uppercase-first nil)
  :config
  (which-key-mode +1))

(use-package discover-my-major
  :general
  (my/leader-def
    "hD" 'discover-my-major)
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'makey-key-mode 'motion)))

(use-package tldr
  :defer t)

(use-package restart-emacs
  :defer t
  :general
  (my/leader-def
    "qr" 'restart-emacs))

(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer")
  (with-eval-after-load 'evil
    ;; cyrillic tweaks
    (define-key evil-normal-state-map (kbd "C-х") #'evil-force-normal-state)
    (define-key evil-insert-state-map (kbd "C-х") #'evil-normal-state)
    (define-key evil-visual-state-map (kbd "C-х") #'evil-exit-visual-state)))

(use-package frame
  :ensure nil
  :general
  (my/leader-def
    "tm" 'toggle-frame-maximized
    "tf" 'toggle-frame-fullscreen)
  :custom
  (default-frame-alist '((left . 0.5) (top . 0.5)
                         (width . 0.7) (height . 0.9)))
  :config
  (blink-cursor-mode -1))

(use-package tool-bar
  :ensure nil
  :config
  (tool-bar-mode -1))

(use-package tooltip
  :ensure nil
  :config
  (tooltip-mode -1))

(use-package scroll-bar
  :ensure nil
  :config
  (scroll-bar-mode -1))

(use-package menu-bar
  :ensure nil
  :config
  (menu-bar-mode -1))

(use-package fringe
  :ensure nil
  :init
  (setf (cdr (assq 'continuation fringe-indicator-alist))
        ;; '(nil nil) ;; no continuation indicators
        '(nil right-curly-arrow) ;; right indicator only
        ;; '(left-curly-arrow nil) ;; left indicator only
        ;; '(left-curly-arrow right-curly-arrow) ;; default
        ))

(use-package ansi-color
  :preface
  ;; http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
  (defun endless/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))
  :hook
  (compilation-filter-hook . endless/colorize-compilation))

(use-package faces
  :ensure nil
  :config
  (set-face-attribute 'default nil :font "Fira Mono 14"))

(use-package font-lock+
  :ensure nil
  :quelpa
  (font-lock+ :repo "emacsmirror/font-lock-plus" :fetcher github))

(use-package all-the-icons
  :if window-system
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

(use-package faces
  :ensure nil
  :custom-face
  (mode-line ((t :inherit mode-line :box nil :underline nil :overline nil)))
  (mode-line-inactive ((t :inherit mode-line-inactive :box nil :underline nil :overline nil))))

(use-package hide-mode-line
  :hook
  (dired-sidebar-mode-hook . hide-mode-line-mode))

(use-package minions
  :config
  (minions-mode))

(use-package doom-modeline
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-minor-modes t)
  (doom-modeline-enable-word-count t)
  :config
  (doom-modeline-mode t))

(use-package solarized-theme
  ;; :disabled
  :custom
  (solarized-distinct-doc-face t)
  (solarized-use-variable-pitch nil)
  (solarized-emphasize-indicators t)
  (solarized-scale-org-headlines nil)
  (solarized-scale-outline-headlines nil)
  (solarized-height-minus-1 1.0)
  (solarized-height-plus-1 1.0)
  (solarized-height-plus-2 1.0)
  (solarized-height-plus-3 1.0)
  (solarized-height-plus-4 1.0)
  :config
  (load-theme 'solarized-dark t))

(use-package doom-themes
  :disabled
  :config
  (load-theme 'doom-city-lights t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package delsel
  :ensure nil
  :general
  ("C-c C-g" 'minibuffer-keyboard-quit)
  :config
  (delete-selection-mode 1))

(use-package simple
  :ensure nil
  :general
  (my/leader-def
    "SPC" 'execute-extended-command
    ":" 'eval-expression
    "tT" 'toggle-truncate-lines)
  :custom
  (backward-delete-char-untabify-method 'hungry)
  (async-shell-command-buffer 'new-buffer)
  :config
  (column-number-mode 1))

(use-package prog-mode
  :ensure nil
  :config
  (global-prettify-symbols-mode t))

(use-package hl-line
  :ensure nil
  :general
  (my/leader-def
    "tl" 'global-hl-line-mode)
  :config
  (global-hl-line-mode 1))

(use-package hl-todo
  :custom
  (hl-todo-highlight-punctuation ":")
  :config
  (global-hl-todo-mode))

(use-package highlight-indent-guides
  :defer t
  :general
  (my/leader-def
    "ti" 'highlight-indent-guides-mode))

(use-package highlight-numbers
  :hook
  (prog-mode-hook . highlight-numbers-mode))

(use-package highlight-blocks
  :defer t
  :general
  (my/leader-def
    "tb" 'highlight-blocks-mode))

(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

(use-package elec-pair
  :ensure nil
  :config
  (electric-pair-mode t))

(use-package rainbow-delimiters
  :hook
  (prog-mode-hook . rainbow-delimiters-mode)
  (cider-repl-mode-hook . rainbow-delimiters-mode))

(use-package rainbow-mode
  :general
  (my/leader-def
    "tr" 'rainbow-mode)
  :hook css-mode-hook)

(use-package whitespace
  :ensure nil
  :general
  (my/leader-def
    "tw" 'whitespace-mode))

(use-package page-break-lines
  :hook
  (after-init-hook . global-page-break-lines-mode))

(use-package show-eol
  :general
  (my/leader-def
    "te" 'show-eol-mode))

(use-package symbol-overlay
  :preface
  (defhydra hydra-symbol-overlay
    (:color pink)
    ("." symbol-overlay-put "put")
    ("n" symbol-overlay-jump-next "jump next")
    ("p" symbol-overlay-jump-prev "jump prev")
    ("R" symbol-overlay-rename "rename")
    ("C" symbol-overlay-remove-all "remove all")
    ("q" nil "cancel" :color blue))
  :general
  (my/leader-def
    "th" 'hydra-symbol-overlay/body)
  :custom-face
  (symbol-overlay-default-face ((t (:inherit 'region))))
  (symbol-overlay-face-1 ((t (:inherit 'org-level-1 :inverse-video t))))
  (symbol-overlay-face-2 ((t (:inherit 'org-level-2 :inverse-video t))))
  (symbol-overlay-face-3 ((t (:inherit 'org-level-3 :inverse-video t))))
  (symbol-overlay-face-4 ((t (:inherit 'org-level-4 :inverse-video t))))
  (symbol-overlay-face-5 ((t (:inherit 'org-level-5 :inverse-video t))))
  (symbol-overlay-face-6 ((t (:inherit 'org-level-6 :inverse-video t))))
  (symbol-overlay-face-7 ((t (:inherit 'org-level-7 :inverse-video t))))
  (symbol-overlay-face-8 ((t (:inherit 'org-level-8 :inverse-video t)))))

(use-package display-line-numbers
  :ensure nil
  :defer t
  :general
  (my/leader-def
    "tn" 'display-line-numbers-mode)
  :custom
  (display-line-numbers-width-start t))

(use-package yasnippet
  :hook
  (prog-mode-hook . yas-minor-mode-on)
  (text-mode-hook . yas-minor-mode-on))

(use-package yasnippet-snippets
  :defer t)

(use-package ivy-yasnippet
  :defer t
  :general
  (my/leader-def
    "is" 'ivy-yasnippet))

(use-package company
  :general
  ("M-S-SPC" 'company-complete)
  :custom
  (company-minimum-prefix-length 2)
  (company-require-match 'never)
  (company-selection-wrap-around t)
  (company-tooltip-minimum-width 30)
  (company-tooltip-align-annotations t)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  :hook
  (after-init-hook . global-company-mode))

(use-package company-box
  :disabled
  :after company all-the-icons
  :custom-face
  (company-box-candidate ((t :inherit company-tooltip-common)))
  (company-box-scrollbar ((t :inherit company-scrollbar-fg)))
  :custom
  (company-box-backends-colors nil)
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  :hook
  (company-mode-hook . company-box-mode))

(use-package company-shell
  :after company
  :config
  (add-to-list 'company-backends 'company-shell))

(use-package company-flx
  :after company
  :config
  (company-flx-mode +1))

(use-package company-statistics
  :after company
  :config
  (company-statistics-mode))

(use-package anzu
  :custom
  (anzu-cons-mode-line-p nil)
  :config
  (global-anzu-mode +1))

(use-package evil-anzu
  :after evil anzu)

(use-package hideshow
  :ensure nil
  :defer t
  :hook
  (prog-mode-hook . hs-minor-mode))

(use-package ispell
  :ensure nil
  :defer t
  :if (executable-find "hunspell")
  :init
  ;; ignore $LANG for choosing dictionary
  ;; (setenv "DICTIONARY" "ru_RU,en_US")
  (setenv "LANG" "en_US.UTF-8")
  :custom
  (ispell-really-aspell nil)
  (ispell-really-hunspell t)
  (ispell-dictionary "ru_RU,en_US")
  :config
  (setq ispell-program-name "hunspell")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "ru_RU,en_US"))

(use-package flyspell
  :defer t
  :general
  (my/leader-def
    "ts" 'flyspell-mode)
  (flyspell-mode-map
   "C-," nil
   "C-." nil
   "C-c $" nil)
  :custom
  (flyspell-delay 1)
  (flyspell-use-meta-tab nil)
  (flyspell-issue-message-flag nil)
  (flyspell-prog-text-faces '(;; font-lock-string-face
                              font-lock-comment-face
                              font-lock-doc-face))
  :hook
  (text-mode-hook . flyspell-mode)
  (org-mode-hook . flyspell-mode)
  (prog-mode-hook . flyspell-prog-mode))

(use-package flyspell-correct
  :after flyspell
  :general
  (flyspell-mode-map
   "C-;" 'flyspell-correct-at-point))

(use-package flyspell-correct-ivy
  :after ivy flyspell)

(use-package flycheck
  :defer t
  :hook
  (prog-mode-hook . flycheck-mode)
  :custom
  (flycheck-indication-mode 'right-fringe))

(use-package fringe-helper
  :after flycheck
  :config
  (fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
                        ".....X.."
                        "....XX.."
                        "...XXX.."
                        "..XXXX.."
                        "...XXX.."
                        "....XX.."
                        ".....X.."))

(use-package flycheck-inline
  :after flycheck
  :custom-face
  (flycheck-inline-error ((t :inherit compilation-error :box t :height 0.9)))
  (flycheck-inline-info ((t :inherit compilation-info :box t :height 0.9)))
  (flycheck-inline-warning ((t :inherit compilation-warning :box t :height 0.9)))
  :hook
  (flycheck-mode-hook . flycheck-inline-mode))

(use-package imenu
  :ensure nil
  :general
  (my/leader-def
    "ji" 'imenu))

(use-package avy
  :ensure t
  :preface
  (defhydra hydra-avy
    (:color blue :hint nil)
    "
^Line^       ^Region^       ^Goto^
^^───────────^^─────────────^^─────────────
_y_: yank    _Y_: yank      _c_: char
_m_: move    _M_: move      _w_: any word
_k_: kill    _K_: kill      _W_: word
^^           ^^             _l_: line
^^           ^^             _L_: end of line
"
    ;; line
    ("y" avy-copy-line)
    ("m" avy-move-line)
    ("k" avy-kill-whole-line)
    ;; region
    ("Y" avy-copy-region)
    ("M" avy-move-region)
    ("K" avy-kill-region)
    ;; goto
    ("c" avy-goto-char)
    ("w" avy-goto-word-0)
    ("W" avy-goto-word-1)
    ("l" avy-goto-line)
    ("L" avy-goto-end-of-line))
  :general
  (my/leader-def
    "j." '(hydra-avy/body :wk "hydra-avy")
    "jc" 'avy-goto-char
    "jw" 'avy-goto-word-0
    "jW" 'avy-goto-word-1
    "jl" 'avy-goto-line
    "jL" 'avy-goto-end-of-line)
  :custom
  (avy-background t))

(use-package ace-window
  :ensure t
  :general
  (evil-window-map
   "." 'ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame))

(use-package link-hint
  :ensure t
  :general
  (my/leader-def
    "ol" 'link-hint-open-link))

(use-package dumb-jump
  :defer t
  :preface
  (defhydra hydra-dumb-jump
    (:color blue :columns 3)
    ("j" dumb-jump-go "go")
    ("o" dumb-jump-go-other-window "other window")
    ("e" dumb-jump-go-prefer-external "go external")
    ("x" dumb-jump-go-prefer-external-other-window "go external other window")
    ("i" dumb-jump-go-prompt "prompt")
    ("l" dumb-jump-quick-look "quick look")
    ("b" dumb-jump-back "back"))
  :general
  (my/leader-def
    "jj" '(hydra-dumb-jump/body :wk "hydra-dumb-jump"))
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'rg))

(use-package projectile
  :general
  (my/leader-def
    "p" '(:keymap projectile-command-map :package projectile :wk "project"))
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode t))

(use-package lsp-mode
  :general
  (my/local-leader-def :keymaps 'lsp-mode-map
    "f" '(:ignore t :wk "find")
    "fd" '(lsp-find-definition :wk "definition")
    "fi" '(lsp-find-implementation :wk "implementation")
    "fr" '(lsp-find-references :wk "references")
    "ft" '(lsp-find-type-definition :wk "type definition")

    "g" '(:ignore t :wk "goto")
    "gd" '(lsp-goto-type-definition :wk "definition")
    "gi" '(lsp-goto-implementation :wk "implementation")

    "w" '(:ignore t :wk "workspace")
    "wa" '(lsp-workspace-folders-add :wk "add")
    "wr" '(lsp-workspace-folders-remove :wk "remove")
    "ws" '(lsp-workspace-folders-switch :wk "switch")
    "wR" '(lsp-workspace-restart :wk "restart")
    "wQ" '(lsp-workspace-shutdown :wk "shutdown")

    "R" '(:ignore t :wk "refactor")
    "Rr" '(lsp-rename :wk "rename")

    "=" '(lsp-format-buffer :wk "format")
    "d" '(lsp-describe-thing-at-point :wk "doc")
    "S" '(lsp-describe-session :wk "session"))
  :custom
  (lsp-prefer-flymake nil))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil))

(use-package lsp-treemacs
  :after lsp-mode
  :general
  (my/local-leader-def :keymaps 'lsp-mode-map
    "T" '(:ignore :wk "treemacs")
    "Te" '(lsp-treemacs-errors-list :wk "error list")))

(use-package company-lsp
  :after company lsp-mode
  :custom
  (company-lsp-cache-candidates 'auto)
  :config
  (add-to-list 'company-backends 'company-lsp))

(use-package dap-mode
  :after lsp-mode
  :general
  (my/local-leader-def :keymaps 'dap-mode-map
    "D" '(dap-hydra :wk "debug"))
  :config
  (dap-mode 1)
  (dap-ui-mode 1))

(use-package editorconfig
  :hook
  (prog-mode-hook . editorconfig-mode)
  (text-mode-hook . editorconfig-mode))

(use-package treemacs
  :defer t
  :preface
  (defun my/hide-fringes ()
    (when (display-graphic-p)
      (set-window-fringes nil 0 0)))
  :general
  (my/leader-def
    "0" 'treemacs-select-window
    "ft" 'treemacs)
  :custom-face
  (treemacs-root-face ((t :inherit font-lock-constant-face :bold t :height 1.1)))
  :custom
  (treemacs-collapse-dirs (if (executable-find "python") 3 0))
  (treemacs-follow-after-init t)
  (treemacs-show-cursor t)
  (treemacs-no-png-images nil)
  (treemacs-no-delete-other-windows nil)
  (treemacs-space-between-root-nodes nil)
  (treemacs-width 35)
  :hook
  (treemacs-mode-hook . hide-mode-line-mode)
  (treemacs-mode-hook . my/hide-fringes)
  :config
  (treemacs-create-theme "Icons"
    :config
    (progn
      (treemacs-create-icon
       :icon (concat (all-the-icons-octicon "repo" :v-adjust -0.1 :height 1.2) " ")
       :extensions (root))

      (treemacs-create-icon
       :icon (concat  (all-the-icons-octicon "file-directory" :v-adjust 0) " ")
       :extensions (dir-open))
      (treemacs-create-icon
       :icon (concat (all-the-icons-octicon "file-directory" :v-adjust 0) " ")
       :extensions (dir-closed))

      (treemacs-create-icon
       :icon (concat "  " (all-the-icons-octicon "tag" :v-adjust 0) " ")
       :extensions (tag-leaf))
      (treemacs-create-icon
       :icon (concat
              (all-the-icons-octicon "chevron-down" :v-adjust 0)
              " "
              (all-the-icons-octicon "tag" :v-adjust 0)
              " ")
       :extensions (tag-open))
      (treemacs-create-icon
       :icon (concat
              (all-the-icons-octicon "chevron-right" :v-adjust 0)
              " "
              (all-the-icons-octicon "tag" :v-adjust 0)
              " ")
       :extensions (tag-closed))

      (treemacs-create-icon
       :icon (concat (all-the-icons-octicon "file-code" :v-adjust 0) " ")
       :extensions (fallback))))

  (treemacs-load-theme "Icons")

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode -1)
  (treemacs-git-mode 'simple))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after dired
  :config
  (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

(use-package highlight-defined
  :defer t
  :custom
  (highlight-defined-face-use-itself t)
  :hook
  (emacs-lisp-mode-hook . highlight-defined-mode))

(use-package highlight-quoted
  :defer t
  :hook
  (emacs-lisp-mode-hook . highlight-quoted-mode))

(use-package erefactor
  :defer t
  :general
  (my/local-leader-def :keymaps 'emacs-lisp-mode-map
    "R" '(:keymap erefactor-map :wk "refactor")))

(use-package eros
  :defer t
  :hook
  (emacs-lisp-mode-hook . eros-mode))

(use-package clojure-mode
  :defer t)

(use-package clojure-mode-extra-font-locking
  :defer t)

(use-package clojure-snippets
  :defer t)

(use-package cider
  :general
  (my/local-leader-def :keymaps 'clojure-mode-map
    "c" '(:ignore t :wk "connect")
    "cc" '(cider-jack-in :wk "jack-in")
    "cj" '(cider-jack-in-clj :wk "jack-in-clj")
    "cs" '(cider-jack-in-cljs :wk "jack-in-cljs")
    "cC" '(cider-connect :wk "connect")
    "cR" '(cider-restart :wk "restart")
    "cQ" '(cider-quit :wk "quit")

    "=" '(cider-format-buffer :wk "format"))
  :custom
  (cider-repl-use-pretty-printing t)
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-history-display-style 'one-line)
  (cider-repl-history-highlight-current-entry t)
  (cider-repl-history-highlight-inserted-item t))

(use-package cider-hydra
  :general
  (my/local-leader-def :keymaps 'clojure-mode-map
    "d" '(cider-hydra-doc/body :wk "doc")
    "e" '(cider-hydra-eval/body :wk "eval")
    "t" '(cider-hydra-test/body :wk "test")
    "r" '(cider-hydra-repl/body :wk "repl"))
  :hook
  (clojure-mode-hook . cider-hydra-mode))

(use-package clj-refactor
  :general
  (my/local-leader-def :keymaps 'clojure-mode-map
    "R" '(hydra-cljr-help-menu/body :wk "refactor"))
  :hook
  (clojure-mode-hook . clj-refactor-mode))

(use-package eldoc
  :ensure nil
  :hook
  (clojure-mode-hook . eldoc-mode)
  (cider-repl-mode-hook . eldoc-mode))

(use-package lsp-java
  :after cc-mode
  :general
  (my/local-leader-def :keymaps 'java-mode-map
    "Re" '(:ignore t :wk "extract")
    "Rem" '(lsp-java-extract-method :wk "method")
    "Rec" '(lsp-java-extract-to-constant :wk "constant")
    "Rel" '(lsp-java-extract-to-local-variable :wk "local variable")

    "Ra" '(:ignore t :wk "add")
    "Rai" '(lsp-java-add-import :wk "missing import")
    "Rau" '(lsp-java-add-unimplemented-methods :wk "unimplemented methods")
    "Rat" '(lsp-java-add-throws :wk "throws")

    "Rc" '(:ignore t :wk "create")
    "Rcp" '(lsp-java-create-parameter :wk "parameter")
    "Rcf" '(lsp-java-create-field :wk "field")
    "Rcl" '(lsp-java-create-local :wk "local")

    "Ro" '(lsp-java-organize-imports :wk "organize imports")

    "G" '(:ignore t :wk "generate")
    "Gt" '(lsp-java-generate-to-string :wk "toString")
    "Ge" '(lsp-java-generate-equals-and-hash-code :wk "equals and hashCode")
    "Go" '(lsp-java-generate-overrides :wk "method overrides")
    "Gg" '(lsp-java-generate-getters-and-setters :wk "getters and setters")

    "P" '(:ignore t :wk "project")
    "Pb" '(lsp-java-build-project :wk "build")
    "Pc" '(lsp-java-update-project-configuration :wk "update configuration")
    "Pu" '(lsp-java-update-project-uris :wk "update URIs")

    "T" '(:ignore t :wk "treemacs")
    "Tr" '(lsp-java-treemacs-register :wk "register")
    "Tu" '(lsp-java-treemacs-unregister :wk "unregister"))
  :config
  (add-hook 'java-mode-hook 'lsp))

(use-package lsp-java-boot
  :ensure lsp-java
  :hook
  (lsp-mode-hook . lsp-lens-mode)
  (java-mode-hook . lsp-java-boot-lens-mode))

(use-package dap-java
  :ensure nil
  :after lsp-java)

(use-package go-mode
  :ensure-system-package
  (gopls . "go get -u golang.org/x/tools/cmd/gopls")
  :hook
  (go-mode-hook . lsp))

(use-package go-tag
  :after go-mode
  :general
  (my/local-leader-def :keymaps 'go-mode-map
    "Rt" '(:ignore t :wk "tag")
    "Rta" '(go-tag-add :wk "add")
    "Rtr" '(go-tag-remove :wk "remove"))
  :custom
  (go-tag-args '("-transform" "snakecase")))

(use-package gotest
  :after go-mode
  :general
  (my/local-leader-def :keymaps 'go-mode-map
    "e" '(:ignore t :wk "eval")
    "ee" '(go-run :wk "run")

    "t" '(:ignore t :wk "test")
    "tf" '(go-test-current-file :wk "file")
    "tt" '(go-test-current-test :wk "test")
    "tp" '(go-test-current-project :wk "project")

    "b" '(:ignore t :wk "benchmark")
    "bb" '(go-test-current-benchmark :wk "benchmark")
    "bf" '(go-test-current-file-benchmarks :wk "file")
    "bp" '(go-test-current-project-benchmarks :wk "project")))

(use-package go-playground
  :after go-mode)

(use-package gorepl-mode
  :ensure-system-package
  (gore . "go get -u github.com/motemen/gore/cmd/gore")
  :general
  (my/local-leader-def :keymaps 'go-mode-map
    "r" 'gorepl-hydra/body)
  :hook
  (go-mode-hook . gorepl-mode))

(use-package protobuf-mode
  :defer t)

(use-package makefile-executor
  :general
  (my/local-leader-def :keymaps 'makefile-mode-map
    "e" '(:ignore t :wk "eval")
    "ee" '(makefile-executor-execute-target :wk "execute")
    "eb" '(makefile-executor-execute-target :wk "execute in dedicated buffer")
    "el" '(makefile-executor-execute-target :wk "execute last"))
  :hook
  (makefile-mode-hook . makefile-executor-mode))

(use-package js2-mode
  :defer t
  :ensure-system-package
  ((typescript-language-server . "npm i -g typescript-language-server")
   (typescript                 . "npm i -g typescript"))
  :mode "\\.m?js\\'"
  :hook
  (js2-mode-hook . lsp))

(use-package rjsx-mode
  :defer t
  :mode "components/.+\\.js$"
  :hook
  (rjsx-mode-hook . lsp))

(use-package js2-refactor
  :defer t
  :general
  (my/local-leader-def :keymaps '(js2-mode-map rjsx-mode-map)
    "R." '(:keymap js2-refactor-mode-map :wk "js2-refactor"))
  :hook
  (js2-mode-hook  . js2-refactor-mode)
  (rjsx-mode-hook . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix ""))

(use-package npm-mode
  :defer t
  :hook
  (js2-mode-hook  . npm-mode)
  (rjsx-mode-hook . npm-mode))

(use-package plantuml-mode
  :defer t
  :general
  (my/local-leader-def :keymaps 'plantuml-mode-map
    "p" '(plantuml-preview :wk "preview"))
  :custom
  (plantuml-output-type "utxt")
  (plantuml-jar-path
   (car (last (file-expand-wildcards
               "/usr/local/Cellar/plantuml/*/libexec/plantuml.jar")))))

(use-package flycheck-plantuml
  :after plantuml-mode
  :config
  (flycheck-plantuml-setup))

(use-package ob-plantuml
  :ensure org-plus-contrib
  :after org
  :custom
  (org-plantuml-jar-path plantuml-jar-path))

(use-package sql
  :ensure nil
  :general
  (my/local-leader-def :keymaps 'sql-mode-map
    "c" '(:ignore t :wk "connect")
    "cc" '(sql-connect :wk "connect")

    "e" '(:ignore t :wk "eval")
    "ee" '(sql-send-paragraph :wk "paragraph")
    "el" '(sql-send-line-and-next :wk "line and next")
    "eb" '(sql-send-buffer :wk "buffer")
    "er" '(sql-send-region :wk "region")
    "es" '(sql-send-string :wk "string")

    "l" '(:ignore t :wk "list")
    "la" '(sql-list-all :wk "all")
    "lt" '(sql-list-table :wk "table"))
  :custom
  (sql-connection-alist '((pg-local
                           (sql-product 'postgres)
                           (sql-port 5432)
                           (sql-server "localhost")
                           (sql-user "postgres")
                           (sql-password "postgres")
                           (sql-database "postgres")))))

(use-package vimrc-mode
  :defer t)

(use-package groovy-mode
  :defer t)

(use-package markdown-mode
  :defer t
  :general
  (my/local-leader-def :keymaps 'markdown-mode-map
    "p" '(markdown-preview :wk "preview"))
  :custom
  (markdown-fontify-code-blocks-natively t)
  :config
  (add-to-list 'markdown-code-lang-modes '("clj" . clojure-mode)))

(use-package json-mode
  :hook
  (json-mode-hook . (lambda () (setq flycheck-checker 'json-jq))))

(use-package yaml-mode
  :defer t
  :mode "Procfile\\'"
  :hook
  (yaml-mode-hook . flycheck-mode))

(use-package flycheck-yamllint
  :after flycheck yaml-mode
  :hook
  (flycheck-mode-hook . flycheck-yamllint-setup))

(use-package magit
  :commands magit-blame
  :general
  (my/leader-def
    "g" '(:ignore t :wk "git")
    "g." 'magit-dispatch
    "gI" 'magit-init
    "gb" 'magit-blame
    "gc" 'magit-clone
    "gg" 'magit-status
    "gi" 'gitignore-templates-new-file
    "gl" 'magit-log-buffer-file
    "gt" 'git-timemachine)
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  (magit-clone-default-directory "~/Projects")
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-repository-directories `((,user-emacs-directory . 0)
                                  (,magit-clone-default-directory . 1))))

(use-package magit-todos
  :after magit
  :custom
  (magit-todos-keyword-suffix (rx (optional "(" (1+ (not (any ")"))) ")" ":")))
  :config
  (magit-todos-mode))

(use-package forge
  :after magit)

(use-package git-timemachine
  :defer t
  :general
  (my/leader-def
    "g" '(:ignore t :wk "git")
    "gt" 'git-timemachine))

(use-package gitattributes-mode
  :defer t)

(use-package gitconfig-mode
  :defer t)

(use-package gitignore-mode
  :defer t)

(use-package gitignore-templates
  :defer t
  :general
  (my/leader-def
    "g" '(:ignore t :wk "git")
    "gi" 'gitignore-templates-new-file)
  (my/local-leader-def :keymaps 'gitignore-mode-map
    "i" 'gitignore-templates-insert))

(use-package diff-hl
  :defer t
  :custom
  (diff-hl-draw-borders nil)
  :hook
  (prog-mode-hook . diff-hl-mode)
  (org-mode-hook . diff-hl-mode)
  (diff-hl-mode . diff-hl-flydiff-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package smerge-mode
  :defer t
  :preface
  (defhydra hydra-smerge
    (:color pink :hint nil)
    "
^Move^       ^Keep^             ^Diff^                ^Other^
^^───────────^^─────────────────^^────────────────────^^─────────────────
_n_: next    _b_: base          _<_: upper/base       _C_: combine
_p_: prev    _u_: upper         _=_: upper/lower      _r_: resolve
_J_: next    _l_: lower         _>_: base/lower       _k_: kill current
_K_: prev    _a_: all           _R_: refine           _ZZ_: save and bury
^^           _RET_: current     _E_: ediff            _q_: cancel
"
    ;; move
    ("n" smerge-next)
    ("p" smerge-prev)
    ("J" smerge-next)
    ("K" smerge-prev)
    ;; keep
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ;; diff
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ;; other
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer)) :color blue)
    ("q" nil :color blue))
  :general
  (my/local-leader-def :keymaps 'smerge-mode-map
    "." 'hydra-smerge/body))

(use-package org
  :ensure org-plus-contrib
  :defer t
  :preface
  (defun my/open-org-directory () (interactive) (find-file org-directory))
  (defun my/open-org-inbox-file () (interactive) (find-file my/org-inbox-file))
  (defun my/open-org-todo-file () (interactive) (find-file my/org-todo-file))
  (defun my/open-org-notes-file () (interactive) (find-file my/org-notes-file))
  :general
  (my/leader-def
    "Oa" '(org-agenda :wk "agenda")
    "O." '(my/open-org-directory :wk "open org-directory")
    "Oi" '(my/open-org-inbox-file :wk "open inbox")
    "Ot" '(my/open-org-todo-file :wk "open todo")
    "On" '(my/open-org-notes-file :wk "open notes")
    "Or" '(org-mode-restart :wk "restart"))
  :custom-face
  (org-tag ((t :inherit shadow)))
  (org-ellipsis ((t :underline nil)))
  :custom
  (org-insert-heading-respect-content t "Insert new headings after current subtree rather than inside it")

  (org-startup-indented t)
  (org-tags-column 0)
  (org-ellipsis "  ")
  (org-pretty-entities t)
  (org-use-sub-superscripts '{} "Require {} for sub/super scripts")
  (org-return-follows-link t)

  (org-list-allow-alphabetical t)
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))

  (org-startup-with-inline-images t)

  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-edit-src-content-indentation 0)
  (org-catch-invisible-edits 'smart)

  (org-hide-leading-stars t)
  (org-hide-leading-stars-before-indent-mode t)

  (org-fontify-done-headline nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)

  (org-todo-keywords '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!/@)" "CANCELED(c@/!)")))
  (org-log-into-drawer t)

  (org-directory "~/Org")
  (my/org-inbox-file (concat org-directory "/inbox.org"))
  (my/org-todo-file (concat org-directory "/todo.org"))
  (my/org-notes-file (concat org-directory "/notes.org"))
  (org-agenda-files `(,my/org-inbox-file ,my/org-todo-file))
  (org-archive-location (concat org-directory "/old/archive.org" "::* From %s")))

(use-package org-bullets
  :disabled
  :after org
  :custom
  ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
  ;; ► • ★ ▸
  (org-bullets-bullet-list '("◆"))
  :hook
  (org-mode-hook . org-bullets-mode))

(use-package toc-org
  :after org
  :hook
  (org-mode-hook . toc-org-enable))

(use-package ob-core
  :ensure org-plus-contrib
  :after org
  :config
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

(use-package ob-async
  :after org)

(use-package ob-shell
  :ensure org-plus-contrib
  :after org)

(use-package docker
  :defer t
  :general
  (my/leader-def
    "od" 'docker)
  :config
  ;; FIXME https://github.com/emacs-evil/evil-collection/pull/205
  (evil-collection-define-key 'normal 'docker-container-mode-map
    "." 'docker-container-ls-popup
    "?" 'docker-container-help-popup
    "C" 'docker-container-cp-popup
    "D" 'docker-container-rm-popup
    "I" 'docker-container-inspect-popup
    "K" 'docker-container-kill-popup
    "L" 'docker-container-logs-popup
    "O" 'docker-container-stop-popup
    "P" 'docker-container-pause-popup
    "R" 'docker-container-restart-popup
    "S" 'docker-container-start-popup
    "a" 'docker-container-attach-popup
    "b" 'docker-container-shell-popup
    "d" 'docker-container-diff-popup
    "f" 'docker-container-find-file-popup
    "q" 'quit-window
    "r" 'docker-container-rename-selection)

  (evil-collection-define-key 'normal 'docker-image-mode-map
    "." 'docker-image-ls-popup
    "?" 'docker-image-help-popup
    "D" 'docker-image-rm-popup
    "F" 'docker-image-pull-popup
    "I" 'docker-image-inspect-popup
    "P" 'docker-image-push-popup
    "R" 'docker-image-run-popup
    "T" 'docker-image-tag-selection
    "q" 'quit-window)

  (evil-collection-define-key 'normal 'docker-machine-mode-map
    "." 'docker-machine-ls-popup
    "?" 'docker-machine-help-popup
    "C" 'docker-machine-create
    "D" 'docker-machine-rm-popup
    "E" 'docker-machine-env-popup
    "O" 'docker-machine-stop-popup
    "R" 'docker-machine-restart-popup
    "S" 'docker-machine-start-popup
    "q" 'quit-window)

  (evil-collection-define-key 'normal 'docker-network-mode-map
    "." 'docker-network-ls-popup
    "?" 'docker-network-help-popup
    "D" 'docker-network-rm-popup
    "q" 'quit-window)

  (evil-collection-define-key 'normal 'docker-volume-mode-map
    "." 'docker-volume-ls-popup
    "?" 'docker-volume-help-popup
    "D" 'docker-volume-rm-popup
    "d" 'docker-volume-dired-selection
    "q" 'quit-window))

(use-package docker-tramp
  :defer t)

(use-package dockerfile-mode
  :defer t
  :general
  (my/local-leader-def :keymaps 'dockerfile-mode-map
    "b" 'dockerfile-build-buffer
    "B" 'dockerfile-build-no-cache-buffer))

(use-package docker-compose-mode
  :defer t
  :general
  (my/local-leader-def :keymaps 'docker-compose-mode-map
    "." 'docker-compose))

(use-package ansible-doc
  :after yaml-mode
  :general
  (my/local-leader-def :keymaps 'yaml-mode-map
    "h" '(ansible-doc :wh "doc"))
  :hook
  (yaml-mode-hook . ansible-doc-mode)
  :config
  (evil-set-initial-state 'ansible-doc-module-mode 'motion))

(use-package jinja2-mode
  :defer t
  :mode "\\.j2\\'")

(use-package company-ansible
  :after company yaml-mode
  :config
  (add-to-list 'company-backends 'company-ansible))

(use-package ansible-vault-with-editor
  :ensure nil
  :quelpa
  (ansible-vault-with-editor
   :fetcher github
   :repo "rynffoll/ansible-vault-with-editor")
  :general
  (my/local-leader-def :keymaps 'yaml-mode-map
    "e" '(ansible-vault-with-editor-edit :wk "edit")
    "E" '(ansible-vault-with-editor-encrypt :wk "encrypt")
    "D" '(ansible-vault-with-editor-decrypt :wk "decrypt")))

(use-package restclient
  :defer t
  :mode
  ("\\.http\\'" . restclient-mode))

(use-package restclient-test
  :hook
  (restclient-mode-hook . restclient-test-mode))

(use-package company-restclient
  :after company restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package ob-restclient
  :after org restclient)

(use-package httprepl
  :defer t)

(use-package know-your-http-well
  :defer t)

(use-package ssh-config-mode
  :defer t
  :init
  (autoload 'ssh-config-mode "ssh-config-mode" t))

(use-package password-generator
  :defer t
  :general
  (my/leader-def
    "ip" '(:ignore t :wk "password-generator")
    "ips" 'password-generator-simple
    "ipS" 'password-generator-strong
    "ipp" 'password-generator-paranoid
    "ipn" 'password-generator-numeric
    "ipP" 'password-generator-phonetic))

(use-package google-translate
  :defer t
  :general
  (my/leader-def
    "ht" 'google-translate-at-point
    "hT" 'google-translate-at-point-reverse)
  :custom
  (google-translate-default-target-language "ru")
  (google-translate-default-source-language "en")
  (google-translate-pop-up-buffer-set-focus t)
  (google-translate-backend-method 'curl))

(use-package olivetti
  :defer t
  :general
  (my/leader-def
    "to" 'olivetti-mode)
  :custom
  (olivetti-body-width 100))

(use-package crux
  :defer t
  :general
  (my/leader-def
    "fR" 'crux-rename-file-and-buffer
    "fD" 'crux-delete-file-and-buffer))

(use-package deadgrep
  :defer t
  :general
  (my/leader-def
    "/D" 'deadgrep))

(use-package try
  :defer t
  :general
  (my/leader-def
    "ot" 'try))

(use-package focus
  :defer t)

(use-package string-inflection
  :defer t)

(use-package memory-usage
  :defer t)

(setq debug-on-error nil)
(setq debug-on-quit nil)

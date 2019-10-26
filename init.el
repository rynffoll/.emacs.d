;;; -*- lexical-binding: t; -*-

(setq debug-on-error t)
(setq debug-on-quit t)

(setq load-prefer-newer t)
(setq message-log-max t)

(require 'package)
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/")))

(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)
(setq use-package-hook-name-suffix nil)
(setq use-package-enable-imenu-support t)
(setq use-package-compute-statistics t)

(eval-when-compile
  (require 'use-package))

(use-package quelpa-use-package
  :custom
  (quelpa-use-package-inhibit-loading-quelpa t "Improve startup performance"))

(use-package use-package-ensure-system-package)

(use-package general
  :config
  (general-create-definer my--leader-def
    :states '(normal visual insert emacs motion)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (general-create-definer my--local-leader-def
    :states '(normal visual insert emacs motion)
    :keymaps 'override
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m")
  (my--leader-def
    "" '(nil :wk "leader")
    "o" '(:ignore t :wk "open")
    "O" '(:ignore t :wk "org")
    "p" '(:ignore t :wk "project")
    "P" '(:ignore t :wk "package")
    "b" '(:ignore t :wk "buffer")
    "f" '(:ignore t :wk "file")
    "e" '(:ignore t :wk "emacs")
    "g" '(:ignore t :wk "git")
    "/" '(:ignore t :wk "search")
    "j" '(:ignore t :wk "jump")
    "h" '(:ignore t :wk "help")
    "t" '(:ignore t :wk "toggle")
    "i" '(:ignore t :wk "insert")
    "q" '(:ignore t :wk "quit"))
  (my--local-leader-def
    "" '(nil :wk "local leader")))

(use-package evil
  :preface
  (defun my--save-and-kill-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))
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
  (evil-mode t)
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "wq" 'my--save-and-kill-buffer))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer nil)
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :defer t
  :hook
  (after-init-hook . evil-commentary-mode))

(use-package evil-magit
  :after evil magit
  :custom
  (evil-magit-want-horizontal-movement t)
  (evil-magit-use-z-for-folds t))

(use-package evil-surround
  :defer t
  :hook
  (after-init-hook . global-evil-surround-mode))

(use-package evil-matchit
  :defer t
  :hook
  (after-init-hook . global-evil-matchit-mode))

(use-package evil-org
  :defer t
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
  :defer t
  :hook
  (after-init-hook . global-evil-mc-mode))

(use-package evil-traces
  :defer t
  :hook
  (after-init-hook . evil-traces-mode)
  :config
  (evil-traces-use-diff-faces))

(use-package which-key
  :defer t
  :custom
  (which-key-idle-delay 0.3)
  (which-key-sort-uppercase-first nil)
  :hook
  (after-init-hook . which-key-mode))

(use-package emacs
  :ensure nil
  :general
  (my--leader-def
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

(use-package async
  :defer t
  :hook
  (after-init-hook . async-bytecomp-package-mode)
  (dired-mode-hook . dired-async-mode))

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
  :defer t
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-check-vc-info t)
  :hook
  (after-init-hook . global-auto-revert-mode))

(use-package savehist
  :ensure nil
  :defer t
  :hook
  (after-init-hook . savehist-mode))

(use-package saveplace
  :ensure nil
  :defer t
  :hook
  (after-init-hook . save-place-mode))

(use-package recentf
  :ensure nil
  :defer t
  :custom
  (recentf-max-saved-items 300)
  :hook
  (after-init-hook . recentf-mode))

(use-package iqa
  :defer t
  :general
  (my--leader-def
    "ed" 'iqa-find-user-init-directory
    "ee" 'iqa-find-user-init-file
    "er" 'iqa-reload-user-init-file)
  :custom
  (iqa-user-init-file (concat user-emacs-directory "config.org")))

(use-package cus-edit
  :ensure nil
  :defer t
  :general
  (my--leader-def
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
  (defun my--switch-to-scratch () (interactive) (switch-to-buffer "*scratch*"))
  (defun my--switch-to-messages () (interactive) (switch-to-buffer "*Messages*"))
  :general
  (my--leader-def
    "bs" '(my--switch-to-scratch :wk "open scratch")
    "bm" '(my--switch-to-messages :wk "open messages")
    "bR" 'rename-buffer))

(use-package menu-bar
  :ensure nil
  :general
  (my--leader-def
    "bk" 'kill-this-buffer))

(use-package window
  :ensure nil
  :general
  (my--leader-def
    "bb" 'switch-to-buffer
    "bK" 'kill-buffer-and-window))

(use-package ibuffer
  :ensure nil
  :defer t
  :general
  ([remap list-buffers] 'ibuffer)
  (my--leader-def
    "bI" 'ibuffer))

(use-package uniquify
  :ensure nil
  :defer 2
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package evil-commands
  :ensure evil
  :after evil
  :general
  (my--leader-def
    "bn" 'evil-buffer-new
    "b]" 'evil-next-buffer
    "b[" 'evil-prev-buffer))

(use-package ibuffer-vc
  :defer t
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
  (my--leader-def
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
  :defer t
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
  :hook
  (after-init-hook . shackle-mode))

(use-package eyebrowse
  :defer t
  :commands
  eyebrowse-create-window-config
  :preface
  (defun my--eyebrowse-create-window-config-with-tag ()
    (interactive)
    (let ((tag (read-string "Tag: ")))
      (eyebrowse-create-window-config)
      (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) tag)))
  (defun my--eyebrowse-create-projectile-window-config ()
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
  (defun my--eyebrowse-close-other-window-configs ()
    (interactive)
    (when (or (not eyebrowse-close-window-config-prompt)
              (yes-or-no-p "Close other window configs?"))
      (mapcar #'eyebrowse--delete-window-config
              (remove (eyebrowse--get 'current-slot)
                      (mapcar #'car (eyebrowse--get 'window-configs))))))
  :general
  (my--leader-def
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
    "wn" 'my--eyebrowse-create-window-config-with-tag
    "wp" 'my--eyebrowse-create-projectile-window-config
    "wC" 'my--eyebrowse-close-other-window-configs)
  :custom
  (eyebrowse-new-workspace t "Clean up and display the scratch buffer")
  (eyebrowse-wrap-around t)
  (eyebrowse-close-window-config-prompt t)
  :hook
  (after-init-hook . eyebrowse-mode))

(use-package undo-tree
  :defer t
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-history-directory-alist `(("." . ,temporary-file-directory))))

(use-package volatile-highlights
  :defer t
  :after undo-tree
  :hook
  (after-init-hook . volatile-highlights-mode)
  :config
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))

(use-package paradox
  :defer t
  :general
  (my--leader-def
    "Pl" 'paradox-list-packages)
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
  :defer t
  :custom
  (dired-listing-switches "-lah --group-directories-first")
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-recursive-copies 'always "Never prompt for recursive copies of a directory")
  (dired-recursive-deletes 'always "Never prompt for recursive deletes of a directory")
  (dired-hide-details-hide-symlink-targets nil)
  :hook
  (dired-mode-hook . dired-hide-details-mode))

(use-package dired-hide-dotfiles
  :defer t
  :general
  (:keymaps 'dired-mode-map :states 'normal
            "M-." 'dired-hide-dotfiles-mode))

(use-package dired-subtree
  :defer t
  :preface
  (defun my--dired-subtree-revert ()
    (call-interactively 'revert-buffer)
    (recenter))
  :general
  (:keymaps 'dired-mode-map :states 'normal
            "TAB" 'dired-subtree-toggle)
  :custom
  (dired-subtree-use-backgrounds nil)
  :config
  ;; for treemacs-icons-dired
  (advice-add #'dired-subtree-toggle :after #'my--dired-subtree-revert))

(use-package pack
  :defer t
  :general
  (:keymaps 'dired-mode-map :states 'normal
            "P" 'pack-dired-dwim)
  :custom
  (pack-dired-default-extension ".zip"))

(use-package dired-git-info
  :defer t
  :general
  (:keymaps 'dired-mode-map :states 'normal
            ")" 'dired-git-info-mode))

(use-package em-smart
  :ensure nil
  :after eshell
  :config
  (eshell-smart-initialize))

(use-package esh-autosuggest
  :defer t
  :hook
  (eshell-mode-hook . esh-autosuggest-mode))

(use-package eshell-fringe-status
  :defer t
  :hook
  (eshell-mode-hook . eshell-fringe-status-mode))

(use-package eshell-prompt-extras
  :after eshell
  :custom
  (eshell-highlight-prompt nil)
  (eshell-prompt-function 'epe-theme-lambda))

(use-package eshell-toggle
  :defer t
  :general
  (my--leader-def
    "ot" 'eshell-toggle)
  :custom
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package with-editor
  :defer t
  :general
  ([remap shell-command]       'with-editor-shell-command)
  ([remap async-shell-command] 'with-editor-async-shell-command)
  :hook
  (shell-mode-hook   . with-editor-export-editor)
  (term-exec-hook    . with-editor-export-editor)
  (eshell-mode-hook  . with-editor-export-editor))

(use-package ediff
  :ensure nil
  :defer t
  :custom
  (winner-dont-bind-my-keys t "Unbind C-right/C-left")
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
  :hook
  (ediff-prepare-buffer-hook . show-all)
  (ediff-quit-hook . winner-undo))

(use-package ivy
  :defer t
  :general
  (ivy-mode-map
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line)
  (my--leader-def
    "bb" 'ivy-switch-buffer)
  :custom
  (ivy-wrap t)
  (ivy-fixed-height-minibuffer t)
  (ivy-initial-inputs-alist nil)
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'full)
  (ivy-on-del-error-function nil)
  (ivy-use-selectable-prompt t)
  (ivy-re-builders-alist '((counsel-rg . ivy--regex-plus)
                           (swiper     . ivy--regex-plus)
                           (t          . ivy--regex-fuzzy)))
  :hook
  (after-init-hook . ivy-mode)
  ;; custom doesn't work
  (ivy-mode-hook . (lambda () (setq ivy-initial-inputs-alist nil))))

(use-package ivy-hydra
  :defer t)

(use-package ivy-rich
  :defer t
  :custom
  (ivy-rich-path-style 'abbrev)
  :hook
  (ivy-mode-hook . ivy-rich-mode))

(use-package counsel
  :defer t
  :general
  ([remap describe-face]            'counsel-describe-face)
  ([remap describe-function]        'counsel-describe-function)
  ([remap describe-variable]        'counsel-describe-variable)
  ([remap execute-extended-command] 'counsel-M-x)
  ([remap find-file]                'counsel-find-file)
  ([remap find-library]             'counsel-find-library)
  ([remap imenu]                    'counsel-imenu)
  (my--leader-def
    "." 'counsel-find-file

    "oL" 'counsel-find-library
    "oh" 'counsel-command-history

    "Pp" 'counsel-package

    "ff" 'counsel-find-file
    "fr" 'counsel-recentf

    "/b" 'swiper
    "/d" 'counsel-rg

    "tt" 'counsel-load-theme

    "hF" '(:ignore t :wk "face")
    "hFf" 'counsel-faces
    "hFe" 'counsel-colors-emacs
    "hFw" 'counsel-colors-web)
  :custom
  (counsel-describe-function-function 'helpful-callable)
  (counsel-describe-variable-function 'helpful-variable))

(use-package counsel-projectile
  :defer t
  :general
  (my--leader-def
    "/p" 'counsel-projectile-rg)
  :hook
  (after-init-hook . counsel-projectile-mode))

(use-package amx
  :defer t
  :custom
  (amx-backend 'ivy))

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
  :disabled
  :if (file-exists-p "/mnt/c/Windows/System32/cmd.exe")
  :ensure nil
  :custom
  (browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe")
  (browse-url-generic-args '("/c" "start"))
  (browse-url-browser-function 'browse-url-generic))

(use-package help
  :ensure nil
  :defer t
  :general
  (my--leader-def
    "hd" 'describe-mode))

(use-package help-fns
  :ensure nil
  :defer t
  :general
  (my--leader-def
    "hf" 'describe-function
    "hv" 'describe-variable))

(use-package man
  :ensure nil
  :defer t
  :general
  (my--leader-def
    "hM" 'man))

(use-package helpful
  :defer t
  :general
  (my--leader-def
    "h." 'helpful-at-point
    "hC" 'helpful-command
    "hc" 'helpful-callable
    "hk" 'helpful-key
    "hm" 'helpful-macro))

(use-package restart-emacs
  :defer t
  :general
  (my--leader-def
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
  (my--leader-def
    "tm" 'toggle-frame-maximized
    "tf" 'toggle-frame-fullscreen)
  :config
  (blink-cursor-mode -1))

(use-package tooltip
  :ensure nil
  :config
  (tooltip-mode -1))

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
  :defer t
  :preface
  ;; http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
  (defun endless/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))
  :hook
  (compilation-filter-hook . endless/colorize-compilation))

(use-package font-lock+
  :ensure nil
  :quelpa
  (font-lock+ :repo "emacsmirror/font-lock-plus" :fetcher github))

(use-package all-the-icons
  :if (display-graphic-p)
  :defer t
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

(use-package faces
  :ensure nil
  :custom-face
  (mode-line ((t :inherit mode-line :box nil :underline nil :overline nil)))
  (mode-line-inactive ((t :inherit mode-line-inactive :box nil :underline nil :overline nil))))

(use-package hide-mode-line
  :defer t
  :hook
  (dired-sidebar-mode-hook . hide-mode-line-mode))

(use-package minions
  :defer t
  :hook
  (after-init-hook . minions-mode))

(use-package doom-modeline
  :defer t
  :custom
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  :hook
  (after-init-hook . doom-modeline-mode))

(use-package solarized-theme
  :disabled
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
  :custom
  (doom-themes-treemacs-theme "doom-colors")
  :config
  (load-theme 'doom-city-lights t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package delsel
  :ensure nil
  :defer t
  :general
  ("C-c C-g" 'minibuffer-keyboard-quit)
  :hook
  (after-init-hook . delete-selection-mode))

(use-package simple
  :ensure nil
  :defer t
  :general
  (my--leader-def
    "SPC" 'execute-extended-command
    ":" 'eval-expression
    "tT" 'toggle-truncate-lines)
  :custom
  (backward-delete-char-untabify-method 'hungry)
  (async-shell-command-buffer 'new-buffer)
  :hook
  (after-init-hook . column-number-mode))

(use-package prog-mode
  :ensure nil
  :defer t
  :hook
  (after-init-hook . global-prettify-symbols-mode))

(use-package subword
  :ensure nil
  :defer t
  :hook
  (prog-mode-hook . subword-mode))

(use-package hungry-delete
  :defer t
  :hook
  (after-init-hook . global-hungry-delete-mode))

(use-package hl-line
  :ensure nil
  :defer t
  :general
  (my--leader-def
    "tl" 'global-hl-line-mode)
  :hook
  (after-init-hook . global-hl-line-mode))

(use-package hl-todo
  :defer t
  :custom
  (hl-todo-highlight-punctuation ":")
  :hook
  (after-init-hook . global-hl-todo-mode))

(use-package highlight-indent-guides
  :defer t
  :general
  (my--leader-def
    "ti" 'highlight-indent-guides-mode))

(use-package highlight-numbers
  :defer t
  :hook
  (prog-mode-hook . highlight-numbers-mode))

(use-package highlight-blocks
  :defer t
  :general
  (my--leader-def
    "tb" 'highlight-blocks-mode))

(use-package paren
  :ensure nil
  :defer t
  :hook
  (after-init-hook . show-paren-mode))

(use-package elec-pair
  :ensure nil
  :defer t
  :hook
  (after-init-hook . electric-pair-mode))

(use-package rainbow-delimiters
  :defer t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode)
  (cider-repl-mode-hook . rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :general
  (my--leader-def
    "tr" 'rainbow-mode)
  :hook css-mode-hook)

(use-package whitespace
  :ensure nil
  :defer t
  :general
  (my--leader-def
    "tw" 'whitespace-mode))

(use-package page-break-lines
  :defer t
  :hook
  (after-init-hook . global-page-break-lines-mode))

(use-package show-eol
  :defer t
  :general
  (my--leader-def
    "te" 'show-eol-mode))

(use-package hi-lock
  :ensure nil
  :defer t
  :general
  (my--leader-def
    "th" '(:ignore t :wh "highlight")
    "th." 'highlight-symbol-at-point
    "thp" 'highlight-phrase
    "thr" 'highlight-regexp
    "thl" 'highlight-lines-matching-regexp
    "thu" 'unhighlight-regexp))

(use-package so-long
  :ensure nil
  :defer t
  :hook
  (after-init-hook . global-so-long-mode))

(use-package display-line-numbers
  :ensure nil
  :defer t
  :general
  (my--leader-def
    "tn" 'display-line-numbers-mode)
  :custom
  (display-line-numbers-width-start t))

(use-package yasnippet
  :defer t
  :hook
  (text-mode-hook . yas-minor-mode-on)
  (prog-mode-hook . yas-minor-mode-on))

(use-package yasnippet-snippets
  :defer t)

(use-package ivy-yasnippet
  :defer t
  :general
  (my--leader-def
    "is" 'ivy-yasnippet))

(use-package company
  :defer t
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

(use-package company-shell
  :defer t
  :after company
  :init
  (add-to-list 'company-backends 'company-shell))

(use-package company-statistics
  :defer t
  :after company
  :config
  (company-statistics-mode))

(use-package anzu
  :defer t
  :custom
  (anzu-cons-mode-line-p nil)
  :hook
  (after-init-hook . global-anzu-mode))

(use-package evil-anzu
  :defer t
  :after evil anzu)

(use-package hideshow
  :ensure nil
  :defer t
  :hook
  (prog-mode-hook . hs-minor-mode))

(use-package ispell
  :if (executable-find "hunspell")
  :ensure nil
  :defer t
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
  (my--leader-def
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
  :defer t
  :general
  (flyspell-mode-map
   "C-;" 'flyspell-correct-at-point))

(use-package flyspell-correct-ivy
  :defer t
  :after flyspell-correct
  :custom
  (flyspell-correct-interface 'flyspell-correct-ivy))

(use-package flycheck
  :defer t
  :custom
  (flycheck-indication-mode 'right-fringe)
  :hook
  (prog-mode-hook . flycheck-mode))

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
  :defer t
  :custom-face
  (flycheck-inline-error ((t :inherit compilation-error :box t :height 0.9)))
  (flycheck-inline-info ((t :inherit compilation-info :box t :height 0.9)))
  (flycheck-inline-warning ((t :inherit compilation-warning :box t :height 0.9)))
  :hook
  (flycheck-mode-hook . flycheck-inline-mode))

(use-package imenu
  :ensure nil
  :defer 1
  :general
  (my--leader-def
    "ji" 'imenu))

(use-package avy
  :defer t
  :general
  (my--leader-def
    "jc" 'avy-goto-char
    "jw" 'avy-goto-word-0
    "jW" 'avy-goto-word-1
    "jl" 'avy-goto-line
    "jL" 'avy-goto-end-of-line)
  :custom
  (avy-background t))

(use-package ace-window
  :defer t
  :general
  (evil-window-map
   "." 'ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame))

(use-package link-hint
  :defer t
  :general
  (my--leader-def
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
  (my--leader-def
    "jj" '(hydra-dumb-jump/body :wk "hydra-dumb-jump"))
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'rg))

(use-package projectile
  :defer t
  :general
  (my--leader-def
    "p" '(:keymap projectile-command-map :package projectile :wk "project"))
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy)
  :hook
  (after-init-hook . projectile-mode))

(use-package lsp-mode
  :defer t
  :general
  (my--local-leader-def :keymaps 'lsp-mode-map
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
  :defer t
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil))

(use-package lsp-treemacs
  :defer t
  :after lsp-mode treemacs
  :general
  (my--local-leader-def :keymaps 'lsp-mode-map
    "T" '(:ignore :wk "treemacs")
    "Te" '(lsp-treemacs-errors-list :wk "error list")))

(use-package company-lsp
  :defer t
  :after company lsp-mode
  :custom
  (company-lsp-cache-candidates 'auto)
  :init
  (add-to-list 'company-backends 'company-lsp))

(use-package dap-mode
  :defer t
  :after lsp-mode
  :general
  (my--local-leader-def :keymaps 'dap-mode-map
    "D" '(dap-hydra :wk "debug"))
  :config
  (dap-mode 1)
  (dap-ui-mode 1))

(use-package editorconfig
  :defer t
  :hook
  (prog-mode-hook . editorconfig-mode)
  (text-mode-hook . editorconfig-mode))

(use-package treemacs
  :defer t
  :preface
  (defun my--hide-fringes ()
    (when (display-graphic-p)
      (set-window-fringes nil 0 0)))
  :general
  (my--leader-def
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
  (treemacs-recenter-after-file-follow 'on-distance)
  (treemacs-recenter-after-tag-follow 'on-distance)
  :hook
  (treemacs-mode-hook . hide-mode-line-mode)
  (treemacs-mode-hook . my--hide-fringes)
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

  (treemacs-load-theme "Icons"))

(use-package treemacs-evil
  :defer t
  :after treemacs evil)

(use-package treemacs-projectile
  :defer t
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :defer t
  :hook
  (dired-mode-hook . treemacs-icons-dired-mode))

(use-package treemacs-magit
  :defer t
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
  (my--local-leader-def :keymaps 'emacs-lisp-mode-map
    "R" '(:keymap erefactor-map :wk "refactor")))

(use-package eros
  :defer t
  :custom-face
  (eros-result-overlay-face ((t :background "grey90")))
  :hook
  (emacs-lisp-mode-hook . eros-mode))

(use-package clojure-mode
  :defer t)

(use-package clojure-mode-extra-font-locking
  :defer t)

(use-package clojure-snippets
  :defer t)

(use-package cider
  :defer t
  :general
  (my--local-leader-def :keymaps 'clojure-mode-map
    "c" '(:ignore t :wk "connect")
    "cc" '(cider-jack-in :wk "jack-in")
    "cj" '(cider-jack-in-clj :wk "jack-in-clj")
    "cs" '(cider-jack-in-cljs :wk "jack-in-cljs")
    "cC" '(cider-connect :wk "connect")
    "cR" '(cider-restart :wk "restart")
    "cQ" '(cider-quit :wk "quit")

    "b" '(:ignore t :wk "buffer")
    "bs" 'cider-scratch

    "=" '(cider-format-buffer :wk "format"))
  :custom-face
  (cider-result-overlay-face ((t :background "grey90")))
  :custom
  (cider-repl-use-pretty-printing t)
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-history-display-style 'one-line)
  (cider-repl-history-highlight-current-entry t)
  (cider-repl-history-highlight-inserted-item t)
  :hook
  (cider-repl-mode-hook . subword-mode)
  (cider-mode-hook . cider-company-enable-fuzzy-completion)
  (cider-repl-mode-hook . cider-company-enable-fuzzy-completion))

(use-package cider-hydra
  :defer t
  :general
  (my--local-leader-def :keymaps 'clojure-mode-map
    "d" '(cider-hydra-doc/body :wk "doc")
    "e" '(cider-hydra-eval/body :wk "eval")
    "t" '(cider-hydra-test/body :wk "test")
    "r" '(cider-hydra-repl/body :wk "repl"))
  :hook
  (clojure-mode-hook . cider-hydra-mode))

(use-package clj-refactor
  :pin melpa-stable
  :defer t
  :general
  (my--local-leader-def :keymaps 'clojure-mode-map
    "R" '(hydra-cljr-help-menu/body :wk "refactor"))
  :hook
  (clojure-mode-hook . clj-refactor-mode))

(use-package eldoc
  :ensure nil
  :defer t
  :hook
  (clojure-mode-hook . eldoc-mode)
  (cider-repl-mode-hook . eldoc-mode))

(use-package lsp-java
  :defer t
  :after cc-mode
  :general
  (my--local-leader-def :keymaps 'java-mode-map
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
  :defer t
  :hook
  (lsp-mode-hook . lsp-lens-mode)
  (java-mode-hook . lsp-java-boot-lens-mode))

(use-package dap-java
  :ensure nil
  :defer t
  :after lsp-java)

(use-package go-mode
  :defer t
  :ensure-system-package
  (gopls . "go get -u golang.org/x/tools/cmd/gopls")
  :hook
  (go-mode-hook . lsp))

(use-package go-tag
  :defer t
  :after go-mode
  :general
  (my--local-leader-def :keymaps 'go-mode-map
    "Rt" '(:ignore t :wk "tag")
    "Rta" '(go-tag-add :wk "add")
    "Rtr" '(go-tag-remove :wk "remove"))
  :custom
  (go-tag-args '("-transform" "snakecase")))

(use-package gotest
  :defer t
  :after go-mode
  :general
  (my--local-leader-def :keymaps 'go-mode-map
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
  :defer t
  :after go-mode)

(use-package gorepl-mode
  :defer t
  :ensure-system-package
  (gore . "go get -u github.com/motemen/gore/cmd/gore")
  :general
  (my--local-leader-def :keymaps 'go-mode-map
    "r" 'gorepl-hydra/body)
  :hook
  (go-mode-hook . gorepl-mode))

(use-package protobuf-mode
  :defer t)

(use-package makefile-executor
  :defer t
  :general
  (my--local-leader-def :keymaps 'makefile-mode-map
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
  (my--local-leader-def :keymaps '(js2-mode-map rjsx-mode-map)
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
  (my--local-leader-def :keymaps 'plantuml-mode-map
    "p" '(plantuml-preview :wk "preview"))
  :custom
  (plantuml-output-type (if (display-images-p) "png" "txt"))
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path
   (car (last (file-expand-wildcards
               "/usr/local/Cellar/plantuml/*/libexec/plantuml.jar")))))

(use-package flycheck-plantuml
  :defer t
  :hook
  (plantuml-mode-hook . flycheck-plantuml-setup))

(use-package ob-plantuml
  :ensure org-plus-contrib
  :defer t
  :after org
  :custom
  (org-plantuml-jar-path plantuml-jar-path))

(use-package sql
  :ensure nil
  :defer t
  :general
  (my--local-leader-def :keymaps 'sql-mode-map
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
  (my--local-leader-def :keymaps 'markdown-mode-map
    "p" 'markdown-preview)
  :custom
  (markdown-command "pandoc")
  (markdown-fontify-code-blocks-natively t)
  :config
  (add-to-list 'markdown-code-lang-modes '("clj" . clojure-mode)))

(use-package grip-mode
  :defer t
  :general
  (my--local-leader-def :keymaps 'markdown-mode-map
    "g" 'grip-mode))

(use-package json-mode
  :defer t
  :hook
  (json-mode-hook . (lambda () (setq flycheck-checker 'json-jq))))

(use-package yaml-mode
  :defer t
  :mode "Procfile\\'"
  :hook
  (yaml-mode-hook . flycheck-mode))

(use-package flycheck-yamllint
  :defer t
  :hook
  (yaml-mode-hook . flycheck-yamllint-setup))

(use-package magit
  :defer t
  :commands magit-blame
  :general
  (my--leader-def
    "g." 'magit-dispatch
    "gI" 'magit-init
    "gb" 'magit-blame
    "gc" 'magit-clone
    "gg" 'magit-status
    "gl" 'magit-log-buffer-file)
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  (magit-clone-default-directory "~/Projects")
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-repository-directories `((,user-emacs-directory . 0)
                                  (,magit-clone-default-directory . 1))))

(use-package magit-todos
  :defer t
  :custom
  (magit-todos-keyword-suffix (rx (optional "(" (1+ (not (any ")"))) ")" ":")))
  :hook
  (magit-mode-hook . magit-todos-mode))

(use-package git-timemachine
  :defer t
  :general
  (my--leader-def
    "gt" 'git-timemachine))

(use-package gitignore-templates
  :defer t
  :general
  (my--leader-def
    "gi" 'gitignore-templates-new-file)
  (my--local-leader-def :keymaps 'gitignore-mode-map
    "i" 'gitignore-templates-insert))

(use-package gitattributes-mode :defer t)
(use-package gitconfig-mode :defer t)
(use-package gitignore-mode :defer t)

(use-package diff-hl
  :defer t
  :custom
  (diff-hl-draw-borders nil)
  :hook
  (after-init-hook . global-diff-hl-mode)
  (diff-hl-mode-hook . diff-hl-flydiff-mode)
  (dired-mode-hook . diff-hl-dired-mode)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh))

(use-package org
  :ensure org-plus-contrib
  :defer t
  :preface
  (defun my--open-org-directory () (interactive) (find-file org-directory))
  (defun my--open-org-inbox-file () (interactive) (find-file my--org-inbox-file))
  (defun my--open-org-todo-file () (interactive) (find-file my--org-todo-file))
  (defun my--open-org-notes-file () (interactive) (find-file my--org-notes-file))
  :general
  (my--leader-def
    "Oa" '(org-agenda :wk "agenda")
    "O." '(my--open-org-directory :wk "open org-directory")
    "Oi" '(my--open-org-inbox-file :wk "open inbox")
    "Ot" '(my--open-org-todo-file :wk "open todo")
    "On" '(my--open-org-notes-file :wk "open notes"))
  :custom-face
  (org-block-begin-line ((t :background "grey90")))
  (org-block ((t :background "grey95")))
  (org-tag ((t :inherit shadow)))
  (org-ellipsis ((t :underline nil)))
  :custom
  (org-insert-heading-respect-content t "Insert new headings after current subtree rather than inside it")

  (org-startup-indented t)
  (org-tags-column 0)
  ;; (org-ellipsis "  ")
  (org-ellipsis "…")
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
  (org-priority-faces '((?A . (:inherit error :weight bold))
                        (?B . (:inherit warning :weight bold))
                        (?C . (:inherit success :weight bold))))
  (org-log-into-drawer t)

  (org-directory "~/Org")
  (my--org-inbox-file (concat org-directory "/inbox.org"))
  (my--org-todo-file (concat org-directory "/todo.org"))
  (my--org-notes-file (concat org-directory "/notes.org"))
  (org-agenda-files `(,my--org-inbox-file ,my--org-todo-file))
  (org-archive-location (concat org-directory "/old/archive.org" "::* From %s")))

(use-package org-bullets
  :defer t
  :after org
  :custom
  ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
  ;; ► • ★ ▸
  (org-bullets-bullet-list '("○"))
  :hook
  (org-mode-hook . org-bullets-mode))

(use-package toc-org
  :defer t
  :hook
  (org-mode-hook . toc-org-enable))

(use-package ob-core
  :ensure org-plus-contrib
  :defer t
  :hook
  (org-babel-after-execute-hook . org-redisplay-inline-images)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (restclient . t)
     (plantuml . t))))

(use-package ob-async
  :after org)

(use-package docker
  :defer t
  :general
  (my--leader-def
    "od" 'docker))

(use-package docker-tramp
  :defer t)

(use-package dockerfile-mode
  :defer t
  :general
  (my--local-leader-def :keymaps 'dockerfile-mode-map
    "b" 'dockerfile-build-buffer
    "B" 'dockerfile-build-no-cache-buffer))

(use-package docker-compose-mode
  :defer t
  :general
  (my--local-leader-def :keymaps 'docker-compose-mode-map
    "." 'docker-compose))

(use-package ansible-doc
  :defer t
  :general
  (my--local-leader-def :keymaps 'yaml-mode-map
    "h" '(ansible-doc :wh "doc"))
  :hook
  (yaml-mode-hook . ansible-doc-mode)
  :config
  (evil-set-initial-state 'ansible-doc-module-mode 'motion))

(use-package jinja2-mode
  :defer t
  :mode "\\.j2\\'")

(use-package company-ansible
  :defer t
  :after company yaml-mode
  :init
  (add-to-list 'company-backends 'company-ansible))

(use-package ansible-vault-with-editor
  :ensure nil
  :quelpa
  (ansible-vault-with-editor
   :fetcher github
   :repo "rynffoll/ansible-vault-with-editor")
  :defer t
  :general
  (my--local-leader-def :keymaps 'yaml-mode-map
    "e" '(ansible-vault-with-editor-edit :wk "edit")
    "E" '(ansible-vault-with-editor-encrypt :wk "encrypt")
    "D" '(ansible-vault-with-editor-decrypt :wk "decrypt")))

(use-package restclient
  :defer t
  :mode
  ("\\.http\\'" . restclient-mode))

(use-package company-restclient
  :defer t
  :after company restclient
  :init
  (add-to-list 'company-backends 'company-restclient))

(use-package ob-restclient
  :after org restclient)

(use-package restclient-test
  :defer t
  :hook
  (restclient-mode-hook . restclient-test-mode))

(use-package ssh-config-mode
  :defer t
  :init
  (autoload 'ssh-config-mode "ssh-config-mode" t))

(use-package password-generator
  :defer t
  :general
  (my--leader-def
    "ip" '(:ignore t :wk "password-generator")
    "ips" 'password-generator-simple
    "ipS" 'password-generator-strong
    "ipp" 'password-generator-paranoid
    "ipn" 'password-generator-numeric
    "ipP" 'password-generator-phonetic))

(use-package google-translate
  :defer t
  :general
  (my--leader-def
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
  (my--leader-def
    "to" 'olivetti-mode)
  :custom
  (olivetti-body-width 100))

(use-package crux
  :defer t
  :general
  (my--leader-def
    "fR" 'crux-rename-file-and-buffer
    "fD" 'crux-delete-file-and-buffer))

(use-package deadgrep
  :defer t
  :general
  (my--leader-def
    "/D" 'deadgrep))

(use-package try
  :defer t
  :general
  (my--leader-def
    "Pt" 'try))

(use-package string-inflection
  :defer t)

(setq debug-on-error nil)
(setq debug-on-quit nil)

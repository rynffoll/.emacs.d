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

(use-package auto-compile
  :disabled
  :custom
  (auto-compile-display-buffer nil)
  :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))

(use-package general
  :preface
  (defun my/switch-to-scratch () (interactive) (switch-to-buffer "*scratch*"))
  (defun my/switch-to-messages () (interactive) (switch-to-buffer "*Messages*"))
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
  (my/leader-def
    "" '(nil :wk "leader")
    "." 'counsel-find-file
    "SPC" 'execute-extended-command
    ":" 'eval-expression

    "o" '(:ignore t :wk "open")
    "od" 'docker
    "ol" 'link-hint-open-link
    "oL" 'counsel-find-library
    "op" 'package-list-packages
    "oc" 'customize-group
    "ot" 'shell-pop

    "O" '(:ignore t :wk "org")
    "Oa" 'org-agenda
    "O." 'my/open-org-directory
    "Oi" 'my/open-org-inbox-file
    "Ot" 'my/open-org-todo-file
    "On" 'my/open-org-notes-file

    "p" '(:keymap projectile-command-map :package projectile :wk "project")

    "b" '(:ignore t :wk "buffer")
    "b TAB" 'evil-switch-to-windows-last-buffer
    "bI" 'ibuffer
    "bn" 'evil-buffer-new
    "bb" 'ivy-switch-buffer
    "bk" 'kill-this-buffer
    "bK" 'kill-buffer-and-window
    "b]" 'evil-next-buffer
    "b[" 'evil-prev-buffer
    "bR" 'rename-buffer
    "bm" 'my/switch-to-messages
    "bs" 'my/switch-to-scratch

    "f" '(:ignore t :wk "file")
    "fd" 'counsel-dired-jump
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "fR" 'crux-rename-file-and-buffer
    "fD" 'crux-delete-file-and-buffer
    ;; "ft" 'dired-sidebar-toggle-sidebar
    "ft" 'treemacs

    "e" '(:ignore t :wk "emacs")
    "ed" 'iqa-find-user-init-directory
    "ee" 'iqa-find-user-init-file
    "er" 'iqa-reload-user-init-file

    "g" '(:ignore t :wk "git")
    "g." 'magit-dispatch
    "gI" 'magit-init
    "gb" 'magit-blame
    "gc" 'magit-clone
    "gg" 'magit-status
    "gi" 'gitignore-templates-new-file
    "gl" 'magit-log-buffer-file
    "gt" 'git-timemachine

    "/" '(:ignore t :wk "search")
    "/b" 'swiper
    "/d" 'counsel-rg
    "/p" 'counsel-projectile-rg

    "j" '(:ignore t :wk "jump")
    "ji" 'imenu
    "jj" 'dumb-jump-hydra/body

    "h" '(:ignore t :wk "help")
    "h." 'helpful-at-point
    "hC" 'helpful-command
    "hT" 'google-translate-at-point-reverse
    "hc" 'helpful-callable
    "hf" 'describe-function
    "hk" 'helpful-key
    "hm" 'helpful-macro
    "ht" 'google-translate-at-point
    "hv" 'describe-variable
    "hF" 'counsel-faces
    "hM" 'man

    "t" '(:ignore t :wk "toggle")
    "to" 'olivetti-mode
    "tt" 'counsel-load-theme
    "tr" 'rainbow-mode
    "tw" 'whitespace-mode
    "tm" 'toggle-frame-maximized
    "tf" 'toggle-frame-fullscreen
    "tn" 'display-line-numbers-mode
    "tT" 'toggle-truncate-lines
    "ti" 'highlight-indent-guides-mode
    "te" 'toggle-indicate-empty-lines
    "tl" 'global-hl-line-mode

    "i" '(:ignore t :wk "insert")
    "is" 'ivy-yasnippet
    "ip" '(:ignore t :wk "password-generator")
    "ips" 'password-generator-simple
    "ipS" 'password-generator-strong
    "ipp" 'password-generator-paranoid
    "ipn" 'password-generator-numeric
    "ipP" 'password-generator-phonetic

    "q" '(:ignore t :wk "quit")
    "qq" 'kill-emacs
    "qr" 'restart-emacs)
  (my/local-leader-def
    "" '(nil :wk "local leader")))

(use-package evil
  :custom
  (evil-want-keybinding nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-emacs-state-cursor 'hbar)
  (evil-mode-line-format nil)
  (evil-symbol-word-search t)
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
  (recentf-max-saved-items 300))

(use-package ibuffer
  :ensure nil
  :general ([remap list-buffers] 'ibuffer))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package ibuffer-vc
  :after ibuffer
  :hook
  (ibuffer-hook . (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package winner
  :ensure nil
  :general
  (evil-window-map
   "u" 'winner-undo
   "U" 'winner-redo)
  :config
  (winner-mode 1))

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
  :preface
  (defun my/new-workspace ()
    (interactive)
    (eyebrowse-create-window-config)
    (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) nil))
  (defun my/new-project-workspace ()
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
  (defun my/delete-other-workspaces ()
    (interactive)
    (mapcar #'eyebrowse--delete-window-config
            (mapcar #'car
                    (assq-delete-all (eyebrowse--get 'current-slot)
                                     (eyebrowse--get 'window-configs)))))
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
    "wn" 'my/new-workspace
    "wp" 'my/new-project-workspace
    "wC" 'my/delete-other-workspaces)
  :custom
  (eyebrowse-new-workspace t "Clean up and display the scratch buffer")
  (eyebrowse-wrap-around t)
  :config
  (eyebrowse-mode t))

(use-package cus-edit
  :ensure nil
  :custom
  (custom-file null-device "Don't store customizations"))

(use-package undo-tree
  :defer t
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-history-directory-alist `(("." . ,temporary-file-directory)))
  :config
  (global-undo-tree-mode -1))

(use-package undo-propose
  :general
  (ctl-x-map "u" 'undo-propose))

(use-package paradox
  :defer 5
  :custom
  (paradox-execute-asynchronously t)
  (paradox-github-token t "Don't ask github token")
  :config
  (paradox-enable))

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

(use-package epa
  :ensure nil
  :defer t
  :custom
  (epa-pinentry-mode 'loopback))

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
  :general
  (:keymaps 'dired-mode-map :states 'normal
            "TAB" 'dired-subtree-toggle)
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package dired-narrow
  :defer t
  :general
  (:keymaps 'dired-mode-map :states 'normal
            "M-n n" 'dired-narrow
            "M-n f" 'dired-narrow-fuzzy
            "M-n r" 'dired-narrow-regexp))

(use-package dired-sidebar
  :defer t
  :custom
  ;; (dired-sidebar-theme 'none)
  (dired-sidebar-no-delete-other-windows t)
  (dired-sidebar-toggle-hidden-commands '(balance-windows
                                          evil-window-delete
                                          delete-other-windows)))

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
  :custom
  (ivy-wrap t)
  (ivy-fixed-height-minibuffer t)
  (ivy-initial-inputs-alist nil "Don't use ^ as initial input")
  (ivy-format-function 'ivy-format-function-line "highlight til EOL")
  (ivy-use-virtual-buffers nil "don't show recent files in switch-buffer")
  (ivy-virtual-abbreviate 'full)
  (ivy-on-del-error-function nil)
  (ivy-use-selectable-prompt t)
  (ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  :config
  (ivy-mode +1))

(use-package swiper)

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
  :after counsel projectile)

(use-package with-editor
  :general
  ([remap shell-command]       'with-editor-shell-command)
  ([remap async-shell-command] 'with-editor-async-shell-command)
  :hook
  (shell-mode-hook   . with-editor-export-editor)
  (term-exec-hook    . with-editor-export-editor)
  (eshell-mode-hook  . with-editor-export-editor))

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

(use-package restart-emacs
  :defer t)

(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer")
  (with-eval-after-load 'evil
    ;; cyrillic tweaks
    (define-key evil-normal-state-map (kbd "C-х") #'evil-force-normal-state)
    (define-key evil-insert-state-map (kbd "C-х") #'evil-normal-state)
    (define-key evil-visual-state-map (kbd "C-х") #'evil-exit-visual-state)))

(use-package iqa
  :defer t
  :custom
  (iqa-user-init-file (concat user-emacs-directory "config.org")))

(use-package shell-pop
  :defer t
  :custom
  (shell-pop-full-span t "Spans full width of a window")
  (shell-pop-shell-type '("eshell" "*eshell-pop*" (lambda () (eshell)))))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package tldr
  :defer t)

(use-package helpful
  :defer t)

(use-package which-key
  :custom
  (which-key-idle-delay 0.5)
  (which-key-sort-uppercase-first nil)
  :config
  (which-key-mode +1))

(use-package ssh-config-mode
  :defer t
  :init
  (autoload 'ssh-config-mode "ssh-config-mode" t))

(use-package frame
  :ensure nil
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

(use-package all-the-icons)

(use-package all-the-icons-dired
  :disabled
  :hook
  (dired-mode-hook . all-the-icons-dired-mode))

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
  (doom-modeline-major-mode-color-icon t)
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
  (load-theme 'doom-one t)
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
  :custom
  (backward-delete-char-untabify-method 'hungry)
  (async-shell-command-buffer 'new-buffer)
  :config
  (column-number-mode 1))

(use-package prog-mode
  :ensure nil
  :config
  (global-prettify-symbols-mode t))

(use-package rainbow-mode
  :hook css-mode-hook)

(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

(use-package rainbow-delimiters
  :hook
  (prog-mode-hook . rainbow-delimiters-mode)
  (cider-repl-mode-hook . rainbow-delimiters-mode))

(use-package elec-pair
  :ensure nil
  :config
  (electric-pair-mode t))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1))

(use-package hl-todo
  :custom
  (hl-todo-highlight-punctuation ":")
  :config
  (global-hl-todo-mode))

(use-package highlight-indent-guides
  :defer t)

(use-package highlight-numbers
  :hook
  (prog-mode-hook . highlight-numbers-mode))

(use-package page-break-lines
  :hook
  (after-init-hook . global-page-break-lines-mode))

(use-package editorconfig
  :hook
  (prog-mode-hook . editorconfig-mode)
  (text-mode-hook . editorconfig-mode))

(use-package display-line-numbers
  :ensure nil
  :defer t
  :custom
  (display-line-numbers-width-start t))

(use-package yasnippet
  :hook
  (prog-mode-hook . yas-minor-mode-on)
  (text-mode-hook . yas-minor-mode-on))

(use-package yasnippet-snippets
  :defer t)

(use-package ivy-yasnippet
  :defer t)

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
  (company-box-show-single-candidate t)
  (company-box-backends-colors nil)
  (company-box-max-candidates 50)
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

(use-package flycheck
  :defer t
  :hook
  (prog-mode-hook . flycheck-mode)
  :custom
  (flycheck-indication-mode 'right-fringe)
  :config
  (use-package fringe-helper)
  (fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
    ".....X.."
    "....XX.."
    "...XXX.."
    "..XXXX.."
    "...XXX.."
    "....XX.."
    ".....X.."))

(use-package flycheck-inline
  :disabled
  :after flycheck
  :custom-face
  (flycheck-inline-error ((t :inherit compilation-error :box t)))
  (flycheck-inline-info ((t :inherit compilation-info :box t)))
  (flycheck-inline-warning ((t :inherit compilation-warning :box t)))
  :hook
  (flycheck-mode-hook . flycheck-inline-mode))

(use-package dumb-jump
  :defer t
  :preface
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'rg))

(use-package projectile
  :defer t
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode t))

(use-package lsp-mode
  :general
  (my/local-leader-def :keymaps 'lsp-mode-map
    "f" '(nil :wk "find")
    "fd" '(lsp-find-definition :wk "definition")
    "fi" '(lsp-find-implementation :wk "implementation")
    "fr" '(lsp-find-references :wk "references")

    "g" '(nil :wk "goto")
    "gd" '(lsp-goto-type-definition :wk "definition")
    "gi" '(lsp-goto-implementation :wk "implementation")

    "w" '(nil :wk "workspace")
    "wa" '(lsp-workspace-folders-add :wk "add")
    "wr" '(lsp-workspace-folders-remove :wk "remove")
    "ws" '(lsp-workspace-folders-switch :wk "switch")
    "wR" '(lsp-restart-workspace :wk "restart")

    "R" '(nil :wk "refactor")
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

(use-package treemacs
  :defer t
  :preface
  ;; (defun my/convert-to-treemacs-format-icon (item)
  ;;   (-let* (((pattern f . spec) item)
  ;;           (key (s-replace-all '(("^" . "") ("\\" . "") ("$" . "") ("." . "")) pattern))
  ;;           (icon (apply f spec)))
  ;;     (cons key icon)))
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
  (setq treemacs-icon-root-png (concat " " (all-the-icons-octicon "repo" :v-adjust -0.1 :height 1.2) " ")
        treemacs-icon-open-png (concat (all-the-icons-octicon "file-directory" :v-adjust 0) " ")
        treemacs-icon-closed-png (concat (all-the-icons-octicon "file-directory" :v-adjust 0) " ")
        treemacs-icon-closed treemacs-icon-closed-png ;; For treemacs-icons-dired
        treemacs-icon-tag-node-open-png (concat (all-the-icons-octicon "chevron-down") " ")
        treemacs-icon-tag-node-closed-png (concat (all-the-icons-octicon "chevron-right") " ")
        treemacs-icon-tag-leaf-png "- "

        treemacs-icons-hash (make-hash-table :size 200 :test #'equal)
        treemacs-icon-fallback (concat (all-the-icons-octicon "file-code" :v-adjust 0) " ")
        treemacs-icon-text treemacs-icon-fallback)

  ;; (--each (-map #'my/convert-to-treemacs-format-icon all-the-icons-icon-alist)
  ;;   (-let [(file-ext . icon) it]
  ;;     (treemacs-define-custom-icon icon file-ext)))

  ;; (dolist (face '(treemacs-root-face
  ;;                 treemacs-git-unmodified-face
  ;;                 treemacs-git-modified-face
  ;;                 treemacs-git-renamed-face
  ;;                 treemacs-git-ignored-face
  ;;                 treemacs-git-untracked-face
  ;;                 treemacs-git-added-face
  ;;                 treemacs-git-conflict-face
  ;;                 treemacs-directory-face
  ;;                 treemacs-directory-collapsed-face
  ;;                 treemacs-file-face))
  ;;   (let ((faces (face-attribute face :inherit nil)))
  ;;     (set-face-attribute
  ;;      face nil :inherit
  ;;      `(variable-pitch ,@(delq 'unspecified (if (listp faces) faces (list faces)))))))

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

(use-package lisp
  :disabled
  :ensure nil
  :hook
  (after-save-hook . check-parens))

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
    "j" 'cider-jack-in)
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
  :config
  (add-hook 'java-mode-hook 'lsp))

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
    "t" '(nil :wk "tag")
    "tt" '(go-tag-add :wk "add")
    "tT" '(go-tag-remove :wk "remove"))
  :custom
  (go-tag-args '("-transform" "snakecase")))

(use-package gotest
  :after go-mode
  :general
  (my/local-leader-def :keymaps 'go-mode-map
    "e" '(nil :wk "eval")
    "ee" '(go-run :wk "run")

    "T" '(nil :wk "test")
    "Tf" '(go-test-current-file :wk "file")
    "Tt" '(go-test-current-test :wk "test")
    "Tp" '(go-test-current-project :wk "project")

    "b" '(nil :wk "benchmark")
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
    "e" '(nil :wk "eval")
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

(use-package magit
  :commands magit-blame
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  (magit-clone-default-directory "~/Projects")
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-repository-directories `((,user-emacs-directory . 0)
                                  (,magit-clone-default-directory . 1))))

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode))

(use-package forge
  :after magit)

(use-package git-timemachine
  :defer t)

(use-package gitattributes-mode
  :defer t)

(use-package gitconfig-mode
  :defer t)

(use-package gitignore-mode
  :defer t)

(use-package gitignore-templates
  :defer t
  :general
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
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_: next    _b_: base            _<_: upper/base        _C_: combine
_p_: prev    _u_: upper           _=_: upper/lower       _r_: resolve
_J_: next    _l_: lower           _>_: base/lower        _k_: kill current
_K_: prev    _a_: all             _R_: refine
^^           _RET_: current       _E_: ediff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("J" smerge-next)
    ("K" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook
  (magit-diff-visit-file-hook . (lambda ()
                                  (when smerge-mode
                                    (unpackaged/smerge-hydra/body)))))

(use-package org
  :ensure org-plus-contrib
  :defer t
  :preface
  (defun my/open-org-directory () (interactive) (find-file org-directory))
  (defun my/open-org-inbox-file () (interactive) (find-file my/org-inbox-file))
  (defun my/open-org-todo-file () (interactive) (find-file my/org-todo-file))
  (defun my/open-org-notes-file () (interactive) (find-file my/org-notes-file))
  :custom-face
  (org-tag ((t :inherit shadow)))
  :custom
  (org-modules '(org-expiry))
  (org-insert-heading-respect-content t "Insert new headings after current subtree rather than inside it")

  (org-startup-indented t)
  (org-tags-column 0)
  (org-ellipsis "  ")
  (org-pretty-entities t)
  (org-use-sub-superscripts '{} "Require {} for sub/super scripts")
  (org-return-follows-link t)

  (org-startup-with-inline-images t)

  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-edit-src-content-indentation 0)

  (org-hide-leading-stars t)
  (org-hide-leading-stars-before-indent-mode t)

  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)

  (org-todo-keywords '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!/@)" "CANCELED(c@/!)")))
  (org-log-into-drawer t)
  (org-expiry-inactive-timestamps t)

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

(use-package ob-async
  :after org ob)

(use-package docker
  :defer t
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'docker-container-mode 'motion)
    (evil-set-initial-state 'docker-image-mode 'motion)
    (evil-set-initial-state 'docker-network-mode 'motion)
    (evil-set-initial-state 'docker-volume-mode 'motion)
    (evil-set-initial-state 'docker-machine-mode 'motion)))

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
    "m" 'docker-compose))

(use-package yaml-mode
  :defer t
  :mode "Procfile\\'")

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

(use-package ob-restclient
  :after org restclient)

(use-package company-restclient
  :after company restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package httprepl
  :defer t)

(use-package password-generator
  :defer t)

(use-package google-translate
  :defer t
  :custom
  (google-translate-default-target-language "ru")
  (google-translate-default-source-language "en")
  (google-translate-pop-up-buffer-set-focus t))

(use-package olivetti
  :defer t
  :custom
  (olivetti-body-width 100))

(use-package crux
  :defer t)

(use-package link-hint
  :defer t)

(use-package try
  :defer t)

(use-package focus
  :defer t)

(use-package string-inflection
  :defer t)

(setq debug-on-error nil)
(setq debug-on-quit nil)

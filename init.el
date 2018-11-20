(setq debug-on-error t)
(setq debug-on-quit t)

(setq load-prefer-newer t)
(setq message-log-max t) ;; we don't want to lose any startup log info

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold 800000)))

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

(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.01)

(use-package quelpa
  :defer t
  :custom
  (quelpa-use-package-inhibit-loading-quelpa t "Improve startup performance")
  (quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :custom
  (quelpa-use-package-inhibit-loading-quelpa t "Improve startup performance"))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1)
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t))

(use-package general
  :preface
  (defun my/switch-to-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))
  (defun my/switch-to-messages ()
    (interactive)
    (switch-to-buffer "*Messages*"))
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
    "" '(nil :which-key "Leader")
    "." 'counsel-find-file

    "o" '(:ignore t :which-key "Open")
    "od" 'docker
    "ol" 'link-hint-open-link
    "oL" 'counsel-find-library
    "op" 'package-list-packages
    "oc" 'customize-group
    "oo" '(:ignore t :which-key "Org")
    "ooa" 'org-agenda
    "oo." 'my/open-org-directory
    "ooi" 'my/open-org-inbox-file
    "oot" 'my/open-org-todo-file
    "oon" 'my/open-org-notes-file

    "b" '(:ignore t :which-key "Buffers")
    "b TAB" 'evil-switch-to-windows-last-buffer
    "bI" 'ibuffer
    "bn" 'evil-buffer-new
    "bb" 'ivy-switch-buffer
    "bk" 'kill-this-buffer
    "b]" 'evil-next-buffer
    "b[" 'evil-prev-buffer
    "bR" 'crux-rename-buffer-and-file
    "bD" 'crux-delete-buffer-and-file
    "bp" 'counsel-projectile
    "bm" 'my/switch-to-messages
    "bs" 'my/switch-to-scratch

    "f" '(:ignore t :which-key "Files")
    "fd" 'counsel-dired-jump
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "fR" 'crux-rename-file-and-buffer
    "fD" 'crux-delete-file-and-buffer
    "fp" 'projectile-find-file

    "e" '(:ignore t :which-key "Emacs")
    "ed" 'iqa-find-user-init-directory
    "ee" 'iqa-find-user-init-file
    "er" 'iqa-reload-user-init-file

    "g" '(:ignore t :which-key "Git")
    "g." 'magit-dispatch-popup
    "gg" 'magit-status
    "gb" 'magit-blame
    "gI" 'magit-init
    "gc" 'magit-clone
    "gl" 'magit-log-buffer-file
    "gt" 'git-timemachine
    "gi" 'gitignore-templates-new-file
    "gh" '(:ignore t :which-key "GitHub")
    "gh." 'magithub-dispatch-popup
    "ghc" 'magithub-clone

    "/" '(:ignore t :which-key "Search")
    "//" 'swiper
    "/p" 'counsel-projectile-rg

    "j" '(:ignore t :which-key "Jump")
    "ji" 'imenu
    "jj" 'dumb-jump-hydra/body

    "h" '(:ignore t :which-key "Help")
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

    "t" '(:ignore t :which-key "Toggle")
    "to" 'olivetti-mode
    "tt" 'counsel-load-theme
    "tr" 'rainbow-mode
    "tw" 'whitespace-mode
    "tm" 'toggle-frame-maximized
    "tn" 'display-line-numbers-mode
    "tT" 'toggle-truncate-lines
    "ti" 'highlight-indent-guides-mode
    "te" 'toggle-indicate-empty-lines
    "tl" 'global-hl-line-mode

    "q" '(:ignore t :which-key "Quit")
    "qq" 'kill-emacs
    "qr" 'restart-emacs)
  (my/local-leader-def
    "" '(nil :which-key "Local Leader")))

(use-package evil
  :custom
  (evil-want-keybinding nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-emacs-state-cursor 'hbar)
  (evil-mode-line-format nil)
  :config
  ;; TODO move to :general section
  (general-define-key :keymaps 'evil-window-map
                      "u" 'winner-undo
                      "U" 'winner-redo)
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
  (org-mode . evil-org-mode))

(use-package evil-org-agenda
  :ensure evil-org
  :after evil org-agenda
  :config
  (evil-org-agenda-set-keys))

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

(use-package super-save
  :config
  (super-save-mode +1))

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
  (ibuffer . (lambda ()
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

(use-package winum
  :demand
  :general
  (:keymaps 'evil-window-map
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
  (shackle-default-size 0.4)
  (shackle-rules '((help-mode :align below :select t)
                   (helpful-mode :align below)
                   (flycheck-error-list-mode :align below :size 0.25)
                   (cider-repl-mode :align below :size 0.3)
                   (ansible-doc-module-mode :align below)))
  :config
  (shackle-mode 1))

(use-package eyebrowse
  :defer 1
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
    "w" '(:ignore t :which-key "Workspaces")
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

(use-package helpful
  :defer t)

(use-package which-key
  :custom
  (which-key-idle-delay 0.5)
  (which-key-sort-uppercase-first nil)
  :config
  (which-key-mode +1))

(use-package undo-tree
  :defer t
  :custom
  (undo-tree-auto-save-history t)
  ;; undo-in-region is known to cause undo history corruption, which can
  ;; be very destructive! Disabling it deters the error, but does not fix
  ;; it entirely!
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "/.cache/undo-tree"))))
  :config
  (global-undo-tree-mode t))

(use-package paradox
  :defer 5
  :custom
  (paradox-execute-asynchronously t)
  (paradox-github-token t "Don't ask github token")
  :config
  (paradox-enable))

(use-package calendar
  :ensure nil
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
  (dired-mode . dired-hide-details-mode))

(use-package dired-x
  :ensure nil
  :custom
  (dired-bind-jump nil))

(use-package async
  :after dired
  :config
  (dired-async-mode t))

(use-package dired-hide-dotfiles
  :after dired
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
            "M-n f" 'ired-narrow-fuzzy
            "M-n r" 'dired-narrow-regexp))

(use-package dired-sidebar
  :defer t
  :general
  ("M-f" 'dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-theme 'none)
  (dired-sidebar-no-delete-other-windows t)
  (dired-sidebar-toggle-hidden-commands '(balance-windows
                                          evil-window-delete)))

(use-package eshell
  :ensure nil
  :defer t)

(use-package em-smart
  :ensure nil
  :after eshell
  :config (eshell-smart-initialize))

(use-package esh-autosuggest
  :after eshell
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package eshell-fringe-status
  :after eshell
  :hook (eshell-mode . eshell-fringe-status-mode))

(use-package eshell-prompt-extras
  :after eshell
  :custom
  (eshell-highlight-prompt nil)
  (eshell-prompt-function 'epe-theme-lambda))

(use-package ediff
  :ensure nil
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
  :hook
  (ediff-prepare-buffer . show-all)
  (ediff-quit . winner-undo))

(use-package ivy
  :general
  ([remap switch-to-buffer] 'ivy-switch-buffer)
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
  (ivy-re-builders-alist '((counsel-ag . ivy--regex-plus)
                           (counsel-grep . ivy--regex-plus)
                           (swiper . ivy--regex-plus)
                           (t . ivy--regex-fuzzy)))
  :config
  (ivy-mode +1))

(use-package swiper)

(use-package smex)

(use-package counsel
  :after swiper
  :general
  ([remap apropos]                  'counsel-apropos)
  ([remap bookmark-jump]            'counsel-bookmark)
  ([remap describe-face]            'counsel-describe-face)
  ([remap describe-function]        'counsel-describe-function)
  ([remap describe-variable]        'counsel-describe-variable)
  ([remap execute-extended-command] 'counsel-M-x)
  ([remap find-file]                'counsel-find-file)
  ([remap find-library]             'counsel-find-library)

  ([remap info-lookup-symbol]       'counsel-info-lookup-symbol)
  ([remap imenu]                    'counsel-imenu)
  ([remap recentf-open-files]       'counsel-recentf)
  ([remap org-capture]              'counsel-org-capture)
  ([remap swiper]                   'counsel-grep-or-swiper)
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
  ([remap projectile-find-file]        'counsel-projectile-find-file)
  ([remap projectile-find-dir]         'counsel-projectile-find-dir)
  ([remap projectile-switch-to-buffer] 'counsel-projectile-switch-to-buffer)
  ([remap projectile-grep]             'counsel-projectile-grep)
  ([remap projectile-ag]               'counsel-projectile-ag)
  ([remap projectile-ripgrep]          'counsel-projectile-rg)
  ([remap projectile-switch-project]   'counsel-projectile-switch-project))

(use-package ns-win
  :if (memq window-system '(mac ns))
  :ensure nil
  :custom
  (mac-command-modifier 'meta))

(use-package files
  :ensure nil
  :if (memq window-system '(mac ns))
  :custom
  (insert-directory-program "gls"))

(use-package browse-url
  :ensure nil
  :config
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic))))

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
  :general ("M-`" 'shell-pop)
  :custom
  (shell-pop-full-span t "Spans full width of a window")
  (shell-pop-shell-type '("eshell" "*eshell-pop*" (lambda () (eshell)))))

(use-package exec-path-from-shell
  :defer 0.1
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :defer t
  :custom
  (which-key-idle-delay 0.5)
  (which-key-sort-uppercase-first nil)
  :config
  (which-key-mode +1))

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

(use-package frame
  :ensure nil
  :config
  (blink-cursor-mode -1)
  (when window-system
    (setq frame-parameters '((left . 0.5) (top . 0.5)
			     (width . 0.7) (height . 0.9)))
    (dolist (fp frame-parameters)
      (add-to-list 'default-frame-alist fp))))

(use-package fringe
  :ensure nil
  :init
  (setf (cdr (assq 'continuation fringe-indicator-alist))
	;; '(nil nil) ;; no continuation indicators
	'(nil right-curly-arrow) ;; right indicator only
	;; '(left-curly-arrow nil) ;; left indicator only
	;; '(left-curly-arrow right-curly-arrow) ;; default
	))

(use-package font-lock+
  :ensure nil
  :quelpa
  (font-lock+ :repo "emacsmirror/font-lock-plus" :fetcher github))

(use-package faces
  :ensure nil
  :config
  (set-face-attribute 'default nil :font "Fira Mono 14"))

(use-package faces
  :ensure nil
  :custom-face
  (mode-line ((t :inherit mode-line :box nil :underline nil :overline nil)))
  (mode-line-inactive ((t (:inherit mode-line-inactive :box nil :underline nil :overline nil)))))

(use-package hide-mode-line
  :defer t
  :hook (dired-sidebar-mode . hide-mode-line-mode))

(use-package spaceline
  :custom
  (powerline-default-separator nil)
  (spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-minor-modes-p nil)
  (spaceline-hud-p nil)
  (spaceline-purpose-p nil)
  (spaceline-buffer-position-p nil)
  (spaceline-buffer-modified-p nil)
  (spaceline-buffer-encoding-abbrev-p nil)
  (spaceline-buffer-size-p nil)
  (spaceline-input-method-p t)
  (spaceline-org-clock-p t)
  (spaceline-org-pomodoro-p t))

(use-package spaceline-config
  :ensure spaceline
  :preface
  (defun spaceline-custom-theme (&rest additional-segments)
    "My custom spaceline theme."
    (apply 'spaceline--theme
           '(((((persp-name :fallback workspace-number)
                window-number) :separator "|"))
             :fallback evil-state
             :face highlight-face
             :priority 100)
           '((buffer-modified buffer-size buffer-id remote-host)
             :priority 98)
           additional-segments))
  :config
  (spaceline-custom-theme))

(use-package solarized-theme
  :custom
  (solarized-distinct-doc-face t "Emphasize docstrings")
  (solarized-use-variable-pitch nil "Don't change the font for some headings and titles")
  (solarized-emphasize-indicators nil "Use less colors for indicators such as git:gutter, flycheck and similar")
  (solarized-scale-org-headlines nil "Don't change size of org-mode headlines (but keep other size-changes)")
  ;; Avoid all font-size changes
  (solarized-height-minus-1 1.0)
  (solarized-height-plus-1 1.0)
  (solarized-height-plus-2 1.0)
  (solarized-height-plus-3 1.0)
  (solarized-height-plus-4 1.0)
  :config
  (load-theme 'solarized-light t))

(use-package spacemacs-common
  :disabled
  :ensure spacemacs-theme
  :custom
  (spacemacs-theme-org-agenda-height nil)
  (spacemacs-theme-org-bold t)
  (spacemacs-theme-org-height nil)
  (spacemacs-theme-org-highlight t)
  :config
  (load-theme 'spacemacs-light t))

(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode 1))

(use-package simple
  :ensure nil
  :custom
  (backward-delete-char-untabify-method 'hungry)
  :config
  (column-number-mode 1))

(use-package rainbow-mode
  :defer t
  :hook css-mode)

(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

(use-package rainbow-delimiters
  :defer t
  :hook ((prog-mode conf-mode) . rainbow-delimiters-mode))

(use-package smartparens
  :defer t
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-show-pair-from-inside t)
  (sp-cancel-autoskip-on-backward-movement nil)
  :config
  (use-package smartparens-config :ensure nil)

  ;; smartparens breaks evil-mode's replace state
  (with-eval-after-load 'evil
    (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
    (add-hook 'evil-replace-state-exit-hook  #'turn-on-smartparens-mode))

  (smartparens-global-mode t))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1))

(use-package hl-todo
  :defer t
  :hook ((prog-mode conf-mode) . hl-todo-mode))

(use-package highlight-indent-guides
  :defer t)

(use-package highlight-numbers
  :defer t
  :hook ((prog-mode conf-mode) . highlight-numbers-mode))

(use-package editorconfig
  :defer t
  :hook ((prog-mode conf-mode) . editorconfig-mode))

(use-package display-line-numbers
  :ensure nil
  :defer t
  :custom
  (display-line-numbers-width-start 1))

(use-package company
  :defer t
  :general
  ("C-;" 'company-complete)
  :custom
  (company-minimum-prefix-length 2)
  (company-require-match 'never)
  (company-selection-wrap-around t)
  (company-tooltip-minimum-width 30)
  (company-tooltip-margin 2)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-dabbrev-code-other-buffers t)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  :hook
  (after-init . global-company-mode))

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
  :hook (prog-mode . hs-minor-mode))

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets)

(use-package company-yasnippet
  :ensure company
  :after company yasnippet
  :preface
  (defun my/add-snippets-to-company-backend (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  :custom
  (company-backends (mapcar #'my/add-snippets-to-company-backend company-backends)))

(use-package flycheck
  :defer t
  :hook
  (prog-mode . flycheck-mode)
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
  (dumb-jump-selector 'ivy))

(use-package lisp
  :ensure nil
  :hook
  (after-save . check-parens))

(use-package highlight-defined
  :defer t
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted
  :defer t
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

(use-package erefactor
  :defer t
  :general
  (my/local-leader-def :keymaps 'emacs-lisp-mode-map
    "r" '(:keymap erefactor-map)))

(use-package eros
  :defer t
  :hook
  (emacs-lisp-mode . eros-mode))

(use-package clojure-mode
  :defer t)

(use-package clojure-mode-extra-font-locking
  :defer t)

(use-package clojure-snippets
  :defer t)

(use-package cider
  :defer t
  :general
  (my/local-leader-def :keymaps 'clojure-mode-map
    "'" 'cider-jack-in)
  :custom
  (cider-repl-use-pretty-printing t)
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-history-display-style 'one-line)
  (cider-repl-history-highlight-current-entry t)
  (cider-repl-history-highlight-inserted-item t))

(use-package clj-refactor
  :after clojure-mode
  :defer t
  :general
  (my/local-leader-def :keymaps 'clojure-mode-map
    "r" 'hydra-cljr-help-menu/body)
  :hook
  (clojure-mode . clj-refactor-mode))

(use-package eldoc
  :ensure nil
  :hook
  ((clojure-mode cider-repl-mode) . eldoc-mode))

(use-package projectile
  :defer t
  :general
  (my/leader-def
    "p" '(:keymap projectile-command-map :which-key "Projects"))
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode t))

(use-package magit
  :defer t
  :commands magit-blame
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-repository-directories `((,user-emacs-directory . 0)
                                  ("~/Projects" . 1))))

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode))

(use-package magithub
  :after magit
  :commands magithub-clone
  :custom
  (magithub-clone-default-directory "~/Projects")
  :config
  (magithub-feature-autoinject t))

(use-package git-timemachine
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
  ((prog-mode conf-mode org-mode) . diff-hl-mode)
  (diff-hl-mode . diff-hl-flydiff-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package org
  :ensure org-plus-contrib
  :defer t
  :preface
  (defun my/open-org-directory ()
    (interactive)
    (find-file org-directory))
  (defun my/open-org-inbox-file ()
    (interactive)
    (find-file my/org-inbox-file))
  (defun my/open-org-todo-file ()
    (interactive)
    (find-file my/org-todo-file))
  (defun my/open-org-notes-file ()
    (interactive)
    (find-file my/org-notes-file))
  :custom-face
  (org-tag ((t (:inherit shadow))))
  :custom
  (org-modules '(org-expiry))
  (org-insert-heading-respect-content t "Insert new headings after current subtree rather than inside it")

  (org-startup-indented t)
  (org-tags-column 0)
  (org-ellipsis "  ")
  (org-pretty-entities t)
  (org-use-sub-superscripts '{} "Require {} for sub/super scripts")

  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-edit-src-content-indentation 0)

  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)

  (org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!/@)" "CANCELLED(c@/!)")))
  (org-log-into-drawer t)
  (org-expiry-inactive-timestamps t)

  (org-directory "~/Org")
  (my/org-inbox-file (concat org-directory "/inbox.org"))
  (my/org-todo-file (concat org-directory "/todo.org"))
  (my/org-notes-file (concat org-directory "/notes.org"))
  (org-agenda-files `(,my/org-inbox-file ,my/org-todo-file))
  (org-archive-location (concat org-directory "/old/archive.org" "::* From %s")))

(use-package org-bullets
  :after org
  :custom
  ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
  ;; ► • ★ ▸
  (org-bullets-bullet-list '("◆"))
  :hook
  (org-mode . org-bullets-mode))

(use-package toc-org
  :after org
  :hook
  (org-mode . toc-org-enable))

(use-package docker
  :defer t
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'docker-container-mode 'emacs)
    (evil-set-initial-state 'docker-image-mode 'emacs)
    (evil-set-initial-state 'docker-network-mode 'emacs)
    (evil-set-initial-state 'docker-volume-mode 'emacs)
    (evil-set-initial-state 'docker-machine-mode 'emacs)))

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

(use-package ansible-mode
  :ensure nil
  :quelpa (ansible-mode :fetcher github :repo "rynffoll/ansible-mode")
  :defer t
  :general
  (my/local-leader-def :keymaps 'ansible-mode-map
    "d" 'ansible-mode-decrypt-buffer
    "e" 'ansible-mode-encrypt-buffer)
  :custom
  (ansible-mode-enable-auto-decrypt-encrypt t)
  :hook
  (yaml-mode . ansible-mode-maybe-enable))

(use-package ansible-doc
  :after ansible-mode
  :general
  (my/local-leader-def :keymaps 'ansible-mode-map
    "h" 'ansible-doc)
  :hook
  (ansible-mode . ansible-doc-mode)
  :config
  (evil-set-initial-state 'ansible-doc-module-mode 'motion))

(use-package jinja2-mode
  :defer t
  :mode "\\.j2\\'")

(use-package company-ansible
  :after company ansible-mode
  :config
  (add-to-list 'company-backends 'company-ansible))

(use-package restclient
  :defer t
  :mode
  ("\\.http\\'" . restclient-mode))

(use-package ob-restclient
  :after org restclient
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package company-restclient
  :after company restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package password-generator
  :defer t)

(use-package google-translate
  :defer t
  :custom
  (google-translate-default-target-language "ru")
  (google-translate-default-source-language "en")
  (google-translate-output-destination nil)
  (google-translate-pop-up-buffer-set-focus t))

(use-package olivetti
  :defer t
  :custom (olivetti-body-width 100))

(use-package crux
  :defer t)

(use-package link-hint
  :defer t)

(setq debug-on-error nil)
(setq debug-on-quit nil)

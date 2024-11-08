;;; -*- lexical-binding: t; -*-

(setq user-full-name "Ruslan Kamashev"
      user-login-name "rynffoll"
      user-mail-address "rynffoll@gmail.com")

(setq package-archives '(("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa"  . "https://melpa.org/packages/")))

(package-initialize)

(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(setq use-package-hook-name-suffix nil)
(setq use-package-enable-imenu-support t)
(setq use-package-compute-statistics t)
(setq use-package-expand-minimally t)

(use-package gnu-elpa-keyring-update)

(use-package mule
  :ensure nil
  :init
  (setq default-input-method 'russian-computer)
  :config
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

(use-package emacs
  :ensure nil
  :init
  (setq buffer-file-coding-system 'utf-8))

(use-package select
  :ensure nil
  :init
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package calendar
  :ensure nil
  :init
  (setq calendar-date-style 'iso)
  (setq calendar-week-start-day 1))

(use-package compile
  :ensure nil
  :init
  (setq compilation-scroll-output 'first-error))

(use-package ansi-color
  :ensure nil
  :preface
  (defun -ansi-color-apply-on-compilation-buffer ()
    (with-silent-modifications
      (ansi-color-apply-on-region compilation-filter-start (point))))
  :hook
  (compilation-filter-hook . -ansi-color-apply-on-compilation-buffer))

(use-package gcmh
  :hook
  (emacs-startup-hook . gcmh-mode))

(use-package async
  :hook
  (after-init-hook . async-bytecomp-package-mode)
  (dired-mode-hook . dired-async-mode))

(use-package general
  :config
  (general-create-definer -leader-def
    :states '(normal visual insert emacs motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-S-SPC")
  (general-create-definer -local-leader-def
    :states '(normal visual insert emacs motion)
    :keymaps 'override
    :prefix "SPC m"
    :global-prefix "M-,")
  (general-define-key
   :states '(normal visual)
   "," (general-simulate-key "SPC m" :which-key "local leader"))
  (-leader-def
    ""    '(nil :wk "leader")
    "o"   '(:ignore t :wk "open")
    "O"   '(:ignore t :wk "org")
    "p"   '(:ignore t :wk "project")
    "P"   '(:ignore t :wk "package")
    "F"   '(:ignore t :wk "frame")
    "TAB" '(:ignore t :wk "tab")
    "b"   '(:ignore t :wk "buffer")
    "f"   '(:ignore t :wk "file")
    "e"   '(:ignore t :wk "emacs")
    "g"   '(:ignore t :wk "git")
    "/"   '(:ignore t :wk "search")
    "j"   '(:ignore t :wk "jump")
    "h"   '(:ignore t :wk "help")
    "t"   '(:ignore t :wk "toggle")
    "i"   '(:ignore t :wk "insert")
    "q"   '(:ignore t :wk "quit"))
  (-local-leader-def
    ""    '(nil :wk "local leader")))

(use-package evil
  :demand
  :preface
  (defun -save-and-kill-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))
  (defun -disable-evil-cursor ()
    (setq-local evil-default-cursor '(nil)))
  :general
  (evil-insert-state-map
   "C-k" nil)
  (-leader-def
    "j[" 'evil-jump-backward
    "j]" 'evil-jump-forward)
  :custom-face
  (evil-ex-substitute-matches
   ((t (:inherit diff-removed :foreground unspecified :background unspecified :strike-through t))))
  (evil-ex-substitute-replacement
   ((t (:inherit diff-added :foreground unspecified :background unspecified :underline nil))))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-emacs-state-cursor 'hbar)
  (setq evil-mode-line-format nil)
  (setq evil-symbol-word-search t)
  ;; (setq evil-move-beyond-eol nil)
  ;; (setq evil-move-cursor-back t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode t)
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "wq" '-save-and-kill-buffer))

(use-package evil-collection
  :demand
  :after evil
  :init
  (setq evil-collection-magit-want-horizontal-movement t)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :hook
  (after-init-hook . evil-commentary-mode))

(use-package evil-surround
  :hook
  (after-init-hook . global-evil-surround-mode))

(use-package evil-matchit
  :hook
  (after-init-hook . global-evil-matchit-mode))

(use-package evil-org
  :init
  (setq evil-org-key-theme '(todo textobjects insert navigation heading))
  :hook
  (org-mode-hook . evil-org-mode))

(use-package evil-org-agenda
  :demand
  :ensure evil-org
  :after org-agenda
  :config
  (evil-org-agenda-set-keys))

(use-package evil-mc
  :hook
  (after-init-hook . global-evil-mc-mode))

(use-package evil-traces
  :disabled
  :hook
  (after-init-hook . evil-traces-mode)
  :config
  (evil-traces-use-diff-faces))

(use-package which-key
  :ensure nil
  :hook
  (after-init-hook . which-key-mode))

(use-package char-fold
  :ensure nil
  :init
  (setq char-fold-symmetric t)
  (setq search-default-mode #'char-fold-to-regexp))

(use-package reverse-im
  :general
  (evil-normal-state-map "C-х" 'evil-force-normal-state)
  (evil-insert-state-map "C-х" 'evil-normal-state)
  (evil-visual-state-map "C-х" 'evil-exit-visual-state)
  :init
  (setq reverse-im-cache-file (locate-user-emacs-file "reverse-im-cache.el"))
  (setq reverse-im-char-fold t)
  (setq reverse-im-read-char-advice-function #'reverse-im-read-char-exclude)
  (setq reverse-im-input-methods '("russian-computer"))
  :hook
  (after-init-hook . reverse-im-mode))

(use-package xt-mouse
  :unless (display-graphic-p)
  :ensure nil
  :hook
  (after-init-hook . xterm-mouse-mode))

;; Better window divider in terminal: | -> │
;; https://www.reddit.com/r/emacs/comments/3u0d0u/how_do_i_make_the_vertical_window_divider_more/
(unless (display-graphic-p)
  (with-eval-after-load 'disp-table
    (defun -update-window-divider ()
      (let ((display-table (or buffer-display-table
                               standard-display-table))
            (divider (make-glyph-code ?│)))
        (set-display-table-slot display-table 'vertical-border divider)))
    (add-hook 'window-configuration-change-hook '-update-window-divider)))

(use-package pixel-scroll
  :ensure nil
  :config
  (pixel-scroll-precision-mode))

(when (eq window-system 'ns)
  (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend))

(use-package ligature
  :if (display-graphic-p)
  :config
  ;; https://github.com/mickeynp/ligature.el/wiki#jetbrains-mono
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
                                       "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                                       "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                                       "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                                       "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                                       "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                                       "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                                       "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                                       "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                                       "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                                       "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                                       ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                                       "<:<" ";;;"))
  ;; https://github.com/mickeynp/ligature.el/wiki#iosevka
  ;; (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
  ;;                                      "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
  ;;                                      "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
  ;;                                      ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  :hook
  (after-init-hook . global-ligature-mode))

(defvar -with-icons nil)

(use-package all-the-icons
  :disabled
  :if (and -with-icons (display-graphic-p))
  :autoload all-the-icons-octicon
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

(use-package nerd-icons
  :disabled (not -with-icons)
  :if (and -with-icons (display-graphic-p))
  :init
  (setq nerd-icons-color-icons nil)
  :config
  (unless (member "Symbols Nerd Font Mono" (font-family-list))
    (nerd-icons-install-fonts)))

(use-package faces
  :ensure nil
  :custom-face
  (mode-line ((t (:inherit mode-line :box nil :underline nil :overline nil))))
  (mode-line-inactive ((t (:inherit mode-line-inactive :box nil :underline nil :overline nil)))))

(use-package hide-mode-line)

(use-package minions
  :hook
  (after-init-hook . minions-mode))

(use-package doom-modeline
  :init
  (setq doom-modeline-bar-width 2)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-buffer-modification-icon nil)
  (setq doom-modeline-workspace-name nil)
  :hook
  (after-init-hook . doom-modeline-mode))

(use-package custom
  :ensure nil
  :general
  (-leader-def
    "tt" 'load-theme))

(use-package emacs
  ;; :disabled
  :ensure nil
  :init
  (setq modus-themes-common-palette-overrides
        '(;; (bg-region bg-cyan-intense)
          (fg-region unspecified)
          (bg-prose-block-delimiter bg-inactive)
          (fg-prose-block-delimiter fg-dim)
          (bg-prose-block-contents bg-dim)))
  :config
  (require-theme 'modus-themes) ; `require-theme' is ONLY for the built-in Modus themes
  (load-theme 'modus-operandi :no-confirm))

(use-package ef-themes
  ;; :disabled
  ;; :demand
  :config
  (load-theme 'ef-melissa-light :no-confirm))

(use-package solarized-theme
  ;; :disabled
  ;; :demand
  :init
  (setq solarized-distinct-doc-face t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-scale-outline-headlines nil)
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  :config
  (load-theme 'solarized-gruvbox-dark :no-confirm))

(use-package doom-themes
  ;; :disabled
  ;; :demand
  :config
  (load-theme 'doom-earl-grey :no-confirm)
  ;; (setq doom-themes-treemacs-theme "doom-atom")
  ;; (setq doom-themes-treemacs-theme "doom-colors")
  ;; (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package frame
  :ensure nil
  :general
  (-leader-def
    "Ff" 'select-frame-by-name
    "Fn" 'make-frame-command
    "Fc" 'delete-frame
    "FC" 'delete-other-frames
    "Fo" 'other-frame
    "Fb" 'switch-to-buffer-other-frame
    "FM" 'toggle-frame-maximized
    "FF" 'toggle-frame-fullscreen)
  :config
  (blink-cursor-mode -1))

(use-package ns-win
  :if (eq window-system 'ns)
  :ensure nil
  :general
  (-leader-def
    "F[" 'ns-prev-frame
    "F]" 'ns-next-frame))

(use-package fringe
  :if (display-graphic-p)
  :ensure nil
  :init
  (setf (cdr (assq 'continuation fringe-indicator-alist))
        '(nil nil) ;; no continuation indicators
        ;; '(nil right-curly-arrow) ;; right indicator only
        ;; '(left-curly-arrow nil) ;; left indicator only
        ;; '(left-curly-arrow right-curly-arrow) ;; default
        ))

(use-package default-text-scale
  :hook
  (after-init-hook . default-text-scale-mode))

(use-package tab-bar
  :ensure nil
  :general
  (-leader-def
    "TAB" '(:keymap tab-prefix-map :wk "tab-bar"))
  (tab-prefix-map
   "." 'tab-bar-select-tab-by-name
   "n" 'tab-new
   "[" 'tab-previous
   "]" 'tab-next
   ">" 'tab-bar-move-tab
   "<" 'tab-bar-move-tab-backward
   "c" 'tab-close
   "C" 'tab-close-other)
  :init
  (setq tab-bar-show 1)
  (setq tab-bar-format '(tab-bar-format-tabs-groups
                         tab-bar-separator))
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-tab-choice "*scratch*")
  :hook
  (after-init-hook . tab-bar-history-mode))

(use-package tab-bar-theme
  :ensure nil
  :load-path "site-lisp/tab-bar-theme"
  :hook
  (after-init-hook . tab-bar-theme-mode))

(use-package project-tab-groups
  :hook
  (after-init-hook . project-tab-groups-mode))

(use-package tab-line
  :ensure nil
  :init
  (setq tab-line-close-button-show nil)
  (setq tab-line-new-button-show nil))

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
  :init
  (setq winner-dont-bind-my-keys t)
  :hook
  (after-init-hook . winner-mode))

(use-package winum
  :general
  (-leader-def
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
  :init
  (setq winum-auto-setup-mode-line nil)
  (setq winum-scope 'frame-local)
  :hook
  (after-init-hook . winum-mode))

(use-package shackle
  :disabled
  :init
  (setq shackle-default-size 0.3)
  (setq shackle-rules
        '((help-mode :align below :select t)
          (helpful-mode :align below)
          (flycheck-error-list-mode :align below)
          (cider-repl-mode :align below)
          (ansible-doc-module-mode :align below)
          ("\\*Async Shell Command\\*.*" :regexp t :ignore t)
          (Man-mode :align below :select t)
          ("\\*Man.*\\*" :regexp t :align below :select t)
          ;; ("*Warnings*" :align below)
          ("*Compile-Log*" :align below)
          (compilation-mode :align below)
          ("\\*vc-git :.*" :regexp t :align below :ignore t :select t)
          ("\\*docker-compose .*\\*" :regexp t :align below)
          (comint-mode :align below)))
  :hook
  (after-init-hook . shackle-mode))

(use-package popper
  :disabled
  :general
  ("C-`"   'popper-toggle-latest)
  ("C-§"   'popper-toggle-latest)
  ;; ("M-`"   'popper-cycle)
  ;; ("M-~"   'popper-cycle-backwards)
  ("C-M-`" 'popper-toggle-type)
  ("C-M-§" 'popper-toggle-type)
  :init
  (setq popper-mode-line '(:eval (propertize " POP " 'face '(region bold))))
  (setq popper-display-control nil) ;; for shackle
  (setq popper-window-height 0.3)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*[Wo]Man.*\\*$"
          ;; "\\*Warnings\\*"
          "\\*Compile-Log\\*"
          "\\*vc-git : .*"
          
          help-mode
          helpful-mode
          
          compilation-mode
          comint-mode
          
          flymake-diagnostics-buffer-mode
          flycheck-error-list-mode
          flycheck-verify-mode
          
          cider-repl-mode
          ansible-doc-module-mode))
  :hook
  (after-init-hook . popper-mode))

(use-package popper-echo
  :disabled
  :ensure popper
  :init
  (setq popper-echo-dispatch-actions t)
  (setq popper-echo-lines 3)
  :hook
  (after-init-hook . popper-echo-mode)
  ;; (after-init-hook . popper-tab-line-mode)
  )

(use-package emacs
  :ensure nil
  :preface
  (defun -switch-to-scratch () (interactive) (switch-to-buffer "*scratch*"))
  (defun -switch-to-messages () (interactive) (switch-to-buffer "*Messages*"))
  :general
  (-leader-def
    "bs" '(-switch-to-scratch :wk "open scratch")
    "bm" '(-switch-to-messages :wk "open messages")
    "bR" 'rename-buffer))

(use-package menu-bar
  :ensure nil
  :general
  (-leader-def
    "bk" 'kill-current-buffer

    "tde" 'toggle-debug-on-error
    "tdq" 'toggle-debug-on-quit))

(use-package window
  :ensure nil
  :general
  (-leader-def
    "bb" 'switch-to-buffer
    "bK" 'kill-buffer-and-window))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package evil-commands
  :ensure evil
  :after evil
  :general
  (-leader-def
    "bn" 'evil-buffer-new
    "b]" 'evil-next-buffer
    "b[" 'evil-prev-buffer))

(use-package ibuffer
  :ensure nil
  :general
  ([remap list-buffers] 'ibuffer)
  (-leader-def
    "bi" 'ibuffer))

(use-package ibuffer-vc
  :preface
  (defun -setup-ibuffer-vc ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook
  (ibuffer-hook . -setup-ibuffer-vc))

(use-package nerd-icons-ibuffer
  :disabled (not -with-icons)
  :if (and -with-icons (display-graphic-p))
  :hook
  (ibuffer-mode-hook . nerd-icons-ibuffer-mode))

(use-package persistent-scratch
  :hook
  (after-init-hook . persistent-scratch-setup-default))

(use-package desktop
  :disabled
  :ensure nil
  :general
  (-leader-def
    "eDs" 'desktop-save
    "eDr" 'desktop-read)
  :init
  (setq desktop-path `(,user-emacs-directory))
  :config
  (dolist (mode '(magit-mode
                  git-commit-mode))
    (add-to-list 'desktop-modes-not-to-save mode))
  :hook
  (after-init-hook . desktop-save-mode))

(use-package savehist
  :ensure nil
  :hook
  (after-init-hook . savehist-mode))

(use-package saveplace
  :ensure nil
  :hook
  (after-init-hook . save-place-mode))

(use-package recentf
  :ensure nil
  :general
  (-leader-def
    "fr" 'recentf-open-files)
  :init
  (setq recentf-max-saved-items 300)
  :hook
  (after-init-hook . recentf-mode))

(use-package consult
  :general
  ([remap apropos]                       'consult-apropos)
  ([remap bookmark-jump]                 'consult-bookmark)
  ([remap goto-line]                     'consult-goto-line)
  ([remap imenu]                         'consult-imenu)
  ([remap locate]                        'consult-locate)
  ([remap load-theme]                    'consult-theme)
  ([remap man]                           'consult-man)
  ([remap recentf-open-files]            'consult-recent-file)
  ([remap switch-to-buffer]              'consult-buffer)
  ([remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame]  'consult-buffer-other-frame)
  ([remap yank-pop]                      'consult-yank-pop)
  (-leader-def
    "/." 'consult-ripgrep
    "/b" 'consult-line)
  :init
  (setq register-preview-delay 0)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  :hook
  (completion-list-mode-hook . consult-preview-at-point-mode))

(use-package consult-xref
  :ensure consult
  :init
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref))

(use-package consult-dir
  :general
  ([remap list-directory] 'consult-dir))

(use-package marginalia
  :general
  (:keymaps 'minibuffer-local-map
            "M-A" 'marginalia-cycle)
  :hook
  (after-init-hook . marginalia-mode))

(use-package nerd-icons-completion
  :disabled (not -with-icons)
  :if (and -with-icons (display-graphic-p))
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  ('marginalia-mode-hook . nerd-icons-completion-marginalia-setup))

(use-package vertico
  :general
  (vertico-map
   "C-j" 'vertico-next
   "C-k" 'vertico-previous)
  :init
  (setq vertico-resize 'grow-only)
  (setq vertico-cycle t)
  :hook
  (after-init-hook . vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless))
  (setq orderless-matching-styles '(orderless-literal
                                    ;; orderless-flex
                                    orderless-prefixes
                                    orderless-regexp))
  (setq completion-category-overrides '((file (styles . (partial-completion))))))

(use-package corfu
  :general
  ("M-S-SPC" 'completion-at-point)
  :init
  (setq corfu-auto t)
  (setq corfu-cycle t)
  (setq corfu-min-width 40)
  :hook
  (after-init-hook . global-corfu-mode))

(use-package corfu-echo
  :ensure corfu
  :hook
  (corfu-mode-hook . corfu-echo-mode))

(use-package corfu-info
  :ensure corfu
  :unless (display-graphic-p)
  :after corfu
  :general
  (corfu-map
   "C-h" 'corfu-info-documentation))

(use-package corfu-popupinfo
  :ensure corfu
  :if (display-graphic-p)
  :general
  (corfu-map
   "C-h" 'corfu-popupinfo-documentation)
  :init
  (setq corfu-popupinfo-delay nil)
  :hook
  (corfu-mode-hook . corfu-popupinfo-mode))

(use-package corfu-history
  :ensure corfu
  :hook
  (corfu-mode-hook . corfu-history-mode))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :hook
  (corfu-mode-hook . corfu-terminal-mode))

(use-package kind-icon
  :unless -with-icons
  :after corfu
  :demand
  :preface
  (defun -kind-icon-reset-cache (theme)
    (call-interactively 'kind-icon-reset-cache))
  :init
  (setq kind-icon-default-face 'corfu-default)
  (setq kind-icon-blend-background t)
  (setq kind-icon-use-icons nil)
  (setq kind-icon-extra-space nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (advice-add #'disable-theme :before #'-kind-icon-reset-cache))

(use-package nerd-icons-corfu
  :disabled (not -with-icons)
  :if (and -with-icons (display-graphic-p))
  :after corfu
  :demand
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :general
  ("C-c p" 'cape-prefix-map)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers.
  (add-to-list 'completion-at-point-functions #'cape-file) ;; Complete file name.
  (add-to-list 'completion-at-point-functions #'cape-elisp-block) ;; Complete Elisp in Org or Markdown code block.
  )

(use-package files
  :ensure nil
  :general
  (-leader-def
    "." 'find-file
    "ff" 'find-file
    "br" 'revert-buffer
    "eR" 'restart-emacs)
  :init
  (setq require-final-newline t)
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq enable-local-variables :all)
  (setq enable-local-eval t))

(use-package autorevert
  :ensure nil
  :init
  (setq auto-revert-verbose nil)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-check-vc-info t)
  :hook
  (after-init-hook . global-auto-revert-mode))

(use-package files
  :if (eq system-type 'darwin)
  :ensure nil
  :init
  (setq insert-directory-program "gls")
  (setq trash-directory "~/.Trash/emacs"))

(use-package iqa
  :preface
  ;; for integration with project-tab-groups
  (defun -iqa-find-file-project (file)
    (let* ((dir (file-name-directory file))
           (default-directory dir))
      (project-current t)
      (find-file file)))
  :general
  (-leader-def
    "ed" 'iqa-find-user-init-directory
    "ee" 'iqa-find-user-init-file
    "ec" 'iqa-find-user-custom-file
    "er" 'iqa-reload-user-init-file)
  :init
  (setq iqa-find-file-function #'-iqa-find-file-project)
  (setq iqa-user-init-file (concat user-emacs-directory "config.org")))

(use-package cus-edit
  :ensure nil
  :general
  (-leader-def
    "oc" 'customize-group))

(use-package epg-config
  :ensure nil
  :init
  (setq epg-pinentry-mode 'loopback))

(use-package project
  :ensure nil
  :general
  (-leader-def
    "p" '(:keymap project-prefix-map :package project :wk "project"))
  (:keymaps 'project-prefix-map
            "m" 'magit-project-status
            "b" 'consult-project-buffer)
  :init
  (setq project-kill-buffers-display-buffer-list t)
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-find-dir "Find directory")
          (magit-project-status "Magit"))))

(use-package dired
  :ensure nil
  :init
  (setq dired-listing-switches "-lah --group-directories-first")
  (setq dired-auto-revert-buffer t)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-mouse-drag-files t)
  (setq mouse-drag-and-drop-region-cross-program t)
  :hook
  (dired-mode-hook . dired-hide-details-mode))

(use-package dired-aux
  :ensure nil
  :init
  (setq dired-vc-rename-file t)
  (setq dired-create-destination-dirs 'ask))

(use-package dired-x
  :ensure nil
  :after dired
  :general
  (:keymaps 'dired-mode-map :states 'normal
            "M-." 'dired-omit-mode)
  :init
  ;; (setq dired-omit-files (rx (seq bol ".")))
  (setq dired-omit-files "^\\.\\.?$")
  (setq dired-omit-extensions nil)
  (setq dired-omit-verbose nil)
  :hook
  (dired-mode-hook . dired-omit-mode))

(use-package dired-subtree
  :preface
  (defun -dired-subtree-revert ()
    (call-interactively 'revert-buffer)
    (recenter))
  :general
  (:keymaps 'dired-mode-map :states 'normal
            "TAB" 'dired-subtree-toggle)
  :init
  (setq dired-subtree-use-backgrounds nil)
  :config
  (when -with-icons
    ;; for treemacs-icons-dired
    (advice-add #'dired-subtree-toggle :after #'-dired-subtree-revert)))

(use-package diredfl
  ;; :disabled
  :preface
  (defun -toggle-diredfl-mode ()
    (if dired-hide-details-mode
        (diredfl-mode -1)
      (diredfl-mode +1)))
  :custom-face
  (diredfl-dir-name ((t (:bold t))))
  :hook
  ;; (dired-hide-details-mode-hook . -toggle-diredfl-mode)
  (after-init-hook . diredfl-global-mode))

(use-package nerd-icons-dired
  :disabled
  :if (and -with-icons (display-graphic-p))
  :hook
  (dired-mode-hook . nerd-icons-dired-mode))

(use-package dired-git-info
  :general
  (:keymaps 'dired-mode-map :states 'normal
            ")" 'dired-git-info-mode)
  :init
  (setq dgi-auto-hide-details-p nil))

;; Back to `quelpa' because `package-vc' doesn't support keyword like `:files'
(use-package quelpa-use-package
  :demand
  :init
  (setq quelpa-use-package-inhibit-loading-quelpa t))

(use-package dirvish
  ;; :vc (:url "https://github.com/hlissner/dirvish" :rev :newest)
  :ensure nil
  :quelpa (dirvish
           :fetcher github
           :repo "hlissner/dirvish"
           :files ("*.el" "extensions/*.el"))
  :preface
  (defun winum-assign-0-to-dirvish-side ()
    (when (and (functionp 'dirvish-side--session-visible-p)
               (eq (selected-window) (dirvish-side--session-visible-p))
               (eq (selected-window) (frame-first-window)))
      0))
  (defun +dired--init-fringes (dir buffer setup)
    (when diff-hl-dired-mode
      (set-window-fringes nil 8 1)))
  :general
  (-leader-def
    "0" 'dirvish-side
    "ft" 'dirvish-side
    "fd" 'drivish)
  ;; TODO: + evil-collection
  (:keymaps 'dirvish-mode-map :states 'normal
            "?"   'dirvish-dispatch
            "q"   'dirvish-quit
            "b"   'dirvish-quick-access
            "f"   'dirvish-file-info-menu
            "p"   'dirvish-yank
            "S"   'dirvish-quicksort
            "F"   'dirvish-layout-toggle
            "z"   'dirvish-history-jump
            "gh"  'dirvish-subtree-up
            "gl"  'dirvish-subtree-toggle
            "TAB" 'dirvish-subtree-toggle
            "h"   'dired-up-directory
            "l"   'dired-find-file
            "[h"  'dirvish-history-go-backward
            "]h"  'dirvish-history-go-forward
            "[e"  'dirvish-emerge-next-group
            "]e"  'dirvish-emerge-previous-group
            "M-e" 'dirvish-emerge-menu
            "M-n" 'dirvish-narrow
            "M-m" 'dirvish-mark-menu
            "M-s" 'dirvish-setup-menu
            "y"    '(:ignore t :wk "yank")
            "yl"   'dirvish-copy-file-true-path
            "yn"   'dirvish-copy-file-name
            "yp"   'dirvish-copy-file-path
            "yr"   'dirvish-copy-remote-path
            "yy"   'dired-do-copy
            "s"    '(:ignore t :wk "symlinks")
            "ss"   'dirvish-symlink
            "sS"   'dirvish-relative-symlink
            "sh"   'dirvish-hardlink)
  :custom-face
  (dirvish-hl-line ((t (:inherit hl-line))))
  :init
  (setq dirvish-mode-line-height   (+ (frame-char-height) 4)) ;; see `doom-modeline-height'
  (setq dirvish-header-line-height (+ (frame-char-height) 4)) ;; see `doom-modeline-height'
  ;; (setq dirvish-attributes '(vc-state)) ;; back to `diff-hl-dir-mode'
  (setq dirvish-attributes nil)
  (setq dirvish-path-separators '("  ~" "  " "/"))
  (setq dirvish-reuse-session nil)
  :hook
  (after-init-hook . dirvish-override-dired-mode)
  :config
  (with-eval-after-load 'winum
    (add-to-list 'winum-assign-functions #'winum-assign-0-to-dirvish-side)
    ;; TODO: contribute to upstream
    (dirvish-define-mode-line winum
      "A `winum-mode' indicator."
      (and (bound-and-true-p winum-mode)
           (let ((num (winum-get-number-string)))
             (propertize (format " %s " num)
                         'face 'winum-face))))
    (setq dirvish-mode-line-format
          '(:left (winum sort) :right (omit yank))))
  ;; https://github.com/doomemacs/doomemacs/blob/master/modules/emacs/dired/config.el#L109
  (advice-add 'dirvish-data-for-dir :before #'+dired--init-fringes))

(use-package tramp
  :ensure nil
  :init
  (setq tramp-default-method "ssh"))

(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :demand
  :init
  (setq exec-path-from-shell-arguments '("-l"))
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

(use-package help
  :ensure nil
  :general
  (-leader-def
    "h" '(:keymap help-map :package help :wk "help")))

(use-package helpful
  :general
  ([remap describe-command]             'helpful-command)
  ([remap describe-key]                 'helpful-key)
  ([remap describe-variable]            'helpful-variable)
  ([remap describe-function]            'helpful-callable)
  ([remap Info-goto-emacs-command-node] 'helpful-function)
  (-leader-def
    "h." 'helpful-at-point))

(use-package find-func
  :ensure nil
  :general
  (-leader-def
    "fl" 'find-library))

(use-package emacs
  :ensure nil
  :init
  (setq-default tab-width 4))

(use-package delsel
  :ensure nil
  :general
  ("C-c C-g" 'minibuffer-keyboard-quit)
  :hook
  (after-init-hook . delete-selection-mode))

(use-package simple
  :ensure nil
  :general
  (-leader-def
    ":" 'execute-extended-command
    "tT" 'toggle-truncate-lines)
  :init
  (setq backward-delete-char-untabify-method 'hungry)
  (setq async-shell-command-buffer 'new-buffer)
  :hook
  (after-init-hook . column-number-mode))

(use-package prog-mode
  :ensure nil
  :hook
  (after-init-hook . global-prettify-symbols-mode))

(use-package so-long
  :ensure nil
  :hook
  (after-init-hook . global-so-long-mode))

(use-package hungry-delete
  :preface
  (defun -disable-hungry-delete-mode ()
    (hungry-delete-mode -1))
  :hook
  (after-init-hook . global-hungry-delete-mode)
  (minibuffer-setup-hook . -disable-hungry-delete-mode))

(use-package ediff
  :ensure nil
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally)
  :hook
  (ediff-prepare-buffer-hook . show-all)
  (ediff-quit-hook . winner-undo))

(use-package undo-tree
  :disabled
  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . ,temporary-file-directory)))
  :hook
  (after-init-hook . global-undo-tree-mode))

(use-package undo-fu
  :disabled)

(use-package undo-fu-session
  :hook
  (org-mode-hook . undo-fu-session-mode))

(use-package vundo
  :general
  ("C-x u" 'vundo)
  :hook
  (vundo-mode-hook . -disable-global-hl-line-mode)
  (vundo-mode-hook . -disable-evil-cursor)
  :custom-face
  (vundo-highlight ((t (:inherit success :foreground unspecified))))
  (vundo-last-saved ((t (:inherit error :foreground unspecified))))
  (vundo-saved ((t (:inherit warning :foreground unspecified))))
  :config
  (setq vundo-compact-display t)
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package hl-line
  :ensure nil
  :preface
  (defun -disable-global-hl-line-mode ()
    (setq-local global-hl-line-mode nil))
  :general
  (-leader-def
    "tl" 'global-hl-line-mode)
  :hook
  (after-init-hook . global-hl-line-mode))

(use-package paren
  :ensure nil
  :hook
  (after-init-hook . show-paren-mode))

(use-package elec-pair
  :ensure nil
  :hook
  (after-init-hook . electric-pair-mode))

(use-package highlight-parentheses
  :hook
  (prog-mode-hook . highlight-parentheses-mode)
  (cider-repl-mode-hook . highlight-parentheses-mode)
  (minibuffer-setup-hook . highlight-parentheses-minibuffer-setup))

(use-package paren-face
  :hook
  (after-init-hook . global-paren-face-mode))

(use-package rainbow-mode
  :general
  (-leader-def
    "tr" 'rainbow-mode)
  :hook
  (css-mode-hook . rainbow-mode))

(use-package whitespace
  :ensure nil
  :general
  (-leader-def
    "tw" 'whitespace-mode))

(use-package page-break-lines
  :hook
  (after-init-hook . global-page-break-lines-mode))

(use-package highlight-indent-guides
  :general
  (-leader-def
    "ti" 'highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top))

(use-package hl-todo
  :init
  (setq hl-todo-highlight-punctuation ":")
  (setq hl-todo-keyword-faces '(("TODO"  . hl-todo)
                                ("FIXME" . hl-todo)))
  :hook
  (after-init-hook . global-hl-todo-mode))

(use-package hi-lock
  :ensure nil
  :general
  (-leader-def
    "/h" '(:ignore t :wh "highlight")
    "/h." 'highlight-symbol-at-point
    "/hp" 'highlight-phrase
    "/hr" 'highlight-regexp
    "/hl" 'highlight-lines-matching-regexp
    "/hu" 'unhighlight-regexp))

(use-package color-identifiers-mode
  :general
  (-leader-def
    "tc" 'color-identifiers-mode))

(use-package prism
  :general
  (-leader-def
    "tp" 'prism-mode))

(use-package display-line-numbers
  :ensure nil
  :general
  (-leader-def
    "tn" 'display-line-numbers-mode)
  :init
  (setq display-line-numbers-width-start t))

(use-package anzu
  :init
  (setq anzu-cons-mode-line-p nil)
  :hook
  (after-init-hook . global-anzu-mode))

(use-package evil-anzu
  :demand
  :after evil anzu)

(use-package outline
  :ensure nil
  :init
  (setq outline-blank-line t))

(use-package hideshow
  :ensure nil
  :hook
  (prog-mode-hook . hs-minor-mode))

(use-package outline-indent
  :hook
  (yaml-ts-mode-hook . outline-indent-minor-mode))

(use-package ispell
  :if (executable-find "hunspell")
  :ensure nil
  :after flyspell
  :init
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-really-aspell nil)
  (setq ispell-really-hunspell t)
  (setq ispell-dictionary "ru_RU,en_US")
  :config
  (setq ispell-program-name "hunspell")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "ru_RU,en_US"))

(use-package flyspell
  :general
  (-leader-def
    "ts" 'flyspell-mode)
  (flyspell-mode-map
   "C-," nil
   "C-." nil
   "C-c $" nil)
  :init
  (setq flyspell-delay 1)
  (setq flyspell-use-meta-tab nil)
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-prog-text-faces '(;; font-lock-string-face
                                   font-lock-comment-face
                                   font-lock-doc-face))
  :hook
  ;; (text-mode-hook . flyspell-mode)
  ;; (org-mode-hook . flyspell-mode)
  ;; (prog-mode-hook . flyspell-prog-mode)
  (git-commit-mode-hook . flyspell-mode))

(use-package flyspell-correct
  :general
  (flyspell-mode-map
   "C-;" 'flyspell-correct-wrapper))

(use-package flycheck
  ;; :disabled
  :preface
  ;; https://www.flycheck.org/en/latest/user/error-reports.html#fringe-and-margin-icons
  (defun -flycheck-set-indication-mode ()
    (pcase flycheck-indication-mode
      (`left-margin
       (setq left-margin-width (max 1 left-margin-width)))
      (`right-margin
       (setq right-margin-width (max 1 right-margin-width))))
    (flycheck-refresh-fringes-and-margins))
  :init
  (setq flycheck-indication-mode (if (display-graphic-p)
                                     'right-fringe
                                   'right-margin))
  (setq flycheck-temp-prefix ".flycheck")
  :hook
  (after-init-hook . global-flycheck-mode)
  (flycheck-mode-hook . -flycheck-set-indication-mode)
  :config
  ;; (when (display-graphic-p)
  ;;   (define-fringe-bitmap '-flycheck-fringe-indicator
  ;;     (vector #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000100
  ;;             #b00001100
  ;;             #b00011100
  ;;             #b00111100
  ;;             #b00011100
  ;;             #b00001100
  ;;             #b00000100
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000))

  ;;   (flycheck-define-error-level 'error
  ;;     :severity 2
  ;;     :overlay-category 'flycheck-error-overlay
  ;;     :fringe-bitmap '-flycheck-fringe-indicator
  ;;     :fringe-face 'flycheck-fringe-error)

  ;;   (flycheck-define-error-level 'warning
  ;;     :severity 1
  ;;     :overlay-category 'flycheck-warning-overlay
  ;;     :fringe-bitmap '-flycheck-fringe-indicator
  ;;     :fringe-face 'flycheck-fringe-warning)

  ;;   (flycheck-define-error-level 'info
  ;;     :severity 0
  ;;     :overlay-category 'flycheck-info-overlay
  ;;     :fringe-bitmap '-flycheck-fringe-indicator
  ;;     :fringe-face 'flycheck-fringe-info))
  (flycheck-redefine-standard-error-levels "!" 'exclamation-mark))

(use-package consult-flycheck
  :requires flycheck
  :general
  (-leader-def
    "je" 'consult-flycheck))

(use-package flymake
  :disabled ;; too slowly
  :ensure nil
  :init
  (setq flymake-fringe-indicator-position 'right-fringe)
  :hook
  (prog-mode-hook . flymake-mode))

(use-package flymake-collection
  :hook
  (after-init-hook . flymake-collection-hook-setup))

(use-package imenu
  :ensure nil
  :general
  (-leader-def
    "ji" 'imenu))

(use-package avy
  :general
  (-leader-def
    "jc" 'avy-goto-char
    "jw" 'avy-goto-word-0
    "jW" 'avy-goto-word-1
    "jl" 'avy-goto-line
    "jL" 'avy-goto-end-of-line)
  :init
  (setq avy-background t))

(use-package link-hint
  :general
  (-leader-def
    "ol" 'link-hint-open-link))

(use-package treemacs
  :disabled
  ;; :preface
  ;; (defun -setup-treemacs-theme ()
  ;;   (treemacs-create-theme "Icons"
  ;;     :config
  ;;     (progn
  ;;       (treemacs-create-icon
  ;;        :icon (format "%s " (all-the-icons-octicon "repo" :v-adjust -0.1 :height 1.2))
  ;;        :extensions (root-open))
  ;;       (treemacs-create-icon
  ;;        :icon (format "%s " (all-the-icons-octicon "repo" :v-adjust -0.1 :height 1.2))
  ;;        :extensions (root-closed))

  ;; 	(treemacs-create-icon
  ;; 	 :icon (format "%s " (all-the-icons-octicon "file-directory" :v-adjust 0))
  ;; 	 :extensions (dir-open))
  ;; 	(treemacs-create-icon
  ;; 	 :icon (format "%s " (all-the-icons-octicon "file-directory" :v-adjust 0))
  ;; 	 :extensions (dir-closed))

  ;; 	(treemacs-create-icon
  ;; 	 :icon (format "  %s " (all-the-icons-octicon "tag" :v-adjust 0))
  ;; 	 :extensions (tag-leaf))
  ;; 	(treemacs-create-icon
  ;; 	 :icon (format "%s %s "
  ;; 		       (all-the-icons-octicon "chevron-down" :v-adjust 0)
  ;; 		       (all-the-icons-octicon "tag" :v-adjust 0))
  ;; 	 :extensions (tag-open))
  ;; 	(treemacs-create-icon
  ;; 	 :icon (format "%s %s "
  ;; 		       (all-the-icons-octicon "chevron-right" :v-adjust 0)
  ;; 		       (all-the-icons-octicon "tag" :v-adjust 0))
  ;; 	 :extensions (tag-closed))

  ;; 	(treemacs-create-icon
  ;; 	 :icon (format "%s " (all-the-icons-octicon "alert" :v-adjust 0 :face 'error))
  ;; 	 :extensions (error))
  ;; 	(treemacs-create-icon
  ;; 	 :icon (format "%s " (all-the-icons-octicon "stop"  :v-adjust 0 :face 'warning))
  ;; 	 :extensions (warning))
  ;; 	(treemacs-create-icon
  ;; 	 :icon (format "%s " (all-the-icons-octicon "info"  :v-adjust 0 :face 'success))
  ;; 	 :extensions (info))

  ;; 	(treemacs-create-icon
  ;; 	 :icon (format "%s " (all-the-icons-octicon "file-text" :v-adjust 0))
  ;; 	 :extensions ("md" "markdown" "rst" "log" "org" "txt"
  ;; 		      "CONTRIBUTE" "LICENSE" "README" "CHANGELOG"))
  ;; 	(treemacs-create-icon
  ;; 	 :icon (format "%s " (all-the-icons-octicon "file-zip" :v-adjust 0))
  ;; 	 :extensions ("zip" "7z" "tar" "gz" "rar" "tgz"
  ;; 		      "xz" "dmg" "iso"))
  ;; 	(treemacs-create-icon
  ;; 	 :icon (format "%s " (all-the-icons-octicon "file-binary" :v-adjust 0))
  ;; 	 :extensions ("exe" "dll" "obj" "so" "o" "out" "elc"))
  ;; 	(treemacs-create-icon
  ;; 	 :icon (format "%s " (all-the-icons-octicon "file-pdf" :v-adjust 0))
  ;; 	 :extensions ("pdf"))
  ;; 	(treemacs-create-icon
  ;; 	 :icon (format "%s " (all-the-icons-octicon "file-media" :v-adjust 0))
  ;; 	 :extensions ("png" "jpg" "jpeg" "gif" "ico" "svg" "bmp"
  ;; 		      "mov" "avi" "mp4" "webm" "mkv"
  ;; 		      "wav" "mp3" "ogg" "midi"))

  ;; 	(treemacs-create-icon
  ;; 	 :icon (format "%s " (all-the-icons-octicon "file-code" :v-adjust 0))
  ;; 	 :extensions (fallback))))

  ;;   (treemacs-load-theme "Icons"))
  :general
  (-leader-def
    "0" 'treemacs-select-window
    "ft" 'treemacs)
  (:keymaps 'justl-mode-map :states 'normal
            "gr" 'treemacs-refresh)
  :init
  (setq treemacs-show-cursor t)
  (setq treemacs-follow-after-init t)
  (setq treemacs-space-between-root-nodes nil)
  (setq treemacs-recenter-after-file-follow 'on-distance)
  (setq treemacs-recenter-after-tag-follow 'on-distance)
  (setq treemacs-no-png-images (not -with-icons))
  :hook
  (treemacs-mode-hook . hide-mode-line-mode)
  (treemacs-mode-hook . -disable-evil-cursor)
  ;; :config
  ;; (when (display-graphic-p)
  ;;   (-setup-treemacs-theme))
  )

(use-package treemacs-fringe-indicator
  :ensure treemacs
  :after treemacs
  :config
  (treemacs-fringe-indicator-mode -1))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-icons-dired
  :if (and -with-icons (display-graphic-p))
  :hook
  (dired-mode-hook . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-tab-bar
  :after treemacs tab-bar
  :config
  (treemacs-set-scope-type 'Tabs))

(use-package treemacs-nerd-icons
  :disabled (not -with-icons)
  :if (and -with-icons (display-graphic-p))
  :after treemacs
  :demand
  :config
  (treemacs-modify-theme "nerd-icons"
    :config
    (treemacs-create-icon
     :icon (format "%s%s%s%s"
                   treemacs-nerd-icons-tab
                   treemacs-nerd-icons-tab
                   (nerd-icons-faicon "nf-fa-folder"  :face 'treemacs-nerd-icons-file-face)
                   treemacs-nerd-icons-tab)
     :extensions (dir-closed dir-open)
     :fallback 'same-as-icon))
  (treemacs-load-theme "nerd-icons"))

(use-package em-smart
  :ensure nil
  :after eshell
  :config
  (eshell-smart-initialize))

(use-package eshell-fringe-status
  :hook
  (eshell-mode-hook . eshell-fringe-status-mode))

(use-package eshell-prompt-extras
  :after eshell
  :commands epe-theme-lambda
  :init
  (setq eshell-highlight-prompt nil)
  (setq eshell-prompt-function 'epe-theme-lambda))

(use-package vterm
  :preface
  (defun -vterm ()
    (interactive)
    (let ((default-directory "~"))
      (if (get-buffer "vterm")
          (switch-to-buffer "vterm")
        (vterm))))
  :general
  (-leader-def
    "ot" '-vterm)
  :init
  (setq vterm-max-scrollback 10000)
  (setq vterm-clear-scrollback-when-clearing t)
  :hook
  (vterm-mode-hook . -disable-global-hl-line-mode)
  (vterm-mode-hook . hide-mode-line-mode))

(use-package eshell-toggle
  :preface
  (defun -eshell-toggle-init-vterm (dir)
    (let ((default-directory dir))
      (vterm)))
  :general
  ("§" 'eshell-toggle)
  (-leader-def
    "`" 'eshell-toggle)
  :init
  (setq eshell-toggle-init-function '-eshell-toggle-init-vterm)
  (setq eshell-toggle-find-project-root-package 'project)
  (setq eshell-toggle-run-command nil))

(use-package magit
  :commands magit-blame
  :preface
  (defun -magit-status ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'magit-status)))
  :general
  (-leader-def
    "g." 'magit-dispatch
    "gI" 'magit-init
    "gb" 'magit-blame
    "gc" 'magit-clone
    "gg" 'magit-status
    "gl" '-magit-status
    "gL" 'magit-log-buffer-file)
  :init
  (setq magit-clone-default-directory "~/Projects/")
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-repository-directories `((,user-emacs-directory . 0)
                                       (,magit-clone-default-directory . 1)))
  (setq magit-diff-refine-hunk t))

(use-package magit-todos
  :init
  (setq magit-todos-keyword-suffix (rx (optional "(" (1+ (not (any ")"))) ")" ":")))
  :hook
  (magit-mode-hook . magit-todos-mode))

(use-package git-timemachine
  :general
  (-leader-def
    "gt" 'git-timemachine))

(use-package git-modes)

(use-package diff-hl
  :init
  (setq diff-hl-draw-borders nil)
  :hook
  (after-init-hook         . global-diff-hl-mode)
  ;; (after-init-hook         . diff-hl-margin-mode)
  (diff-hl-mode-hook       . diff-hl-flydiff-mode)
  (dired-mode-hook         . diff-hl-dired-mode)
  (magit-pre-refresh-hook  . diff-hl-magit-pre-refresh)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh))

(use-package org
  :ensure nil
  :preface
  (defun -open-org-directory  () (interactive) (find-file org-directory))
  (defun -open-org-inbox-file () (interactive) (find-file -org-inbox-file))
  (defun -open-org-todo-file  () (interactive) (find-file -org-todo-file))
  (defun -open-org-notes-file () (interactive) (find-file -org-notes-file))
  :general
  (-leader-def
    "O." '(-open-org-directory  :wk "open org-directory")
    "Oi" '(-open-org-inbox-file :wk "open inbox.org")
    "Ot" '(-open-org-todo-file  :wk "open todo.org")
    "On" '(-open-org-notes-file :wk "open notes.org"))
  :init
  (setq org-directory "~/Org")
  (setq -org-inbox-file (concat org-directory "/inbox.org"))
  (setq -org-todo-file  (concat org-directory "/todo.org"))
  (setq -org-notes-file (concat org-directory "/notes.org"))

  (setq org-startup-folded t)
  (setq org-startup-indented t)
  (setq org-insert-heading-respect-content t)
  (setq org-hide-leading-stars t)

  (setq org-agenda-files `(,-org-todo-file))
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-skip-unavailable-files t)

  (setq org-archive-location (concat org-directory "/archive.org::datetree/"))

  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)
  ;; (setq org-ellipsis "…")
  (setq org-ellipsis " ⌄ ")
  (setq org-pretty-entities t)
  (setq org-hide-emphasis-markers t)
  (setq org-use-sub-superscripts '{})

  (setq org-use-fast-todo-selection 'expert)
  (setq org-todo-keywords '((sequence
                             "TODO(t)"
                             "STARTED(s)"
                             "NEXT(n)"
                             "WAITING(w)"
                             "HOLD(h)"
                             "|"
                             "DONE(d)"
                             "OBSOLETE(o)"
                             "CANCELLED(c)")))

  (setq org-log-done 'time)

  (setq org-startup-with-inline-images t)

  (setq org-catch-invisible-edits 'smart)

  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-done-headline nil))

(use-package org-archive
  :ensure org
  :init
  (setq org-archive-file-header-format nil))

(use-package org-refile
  :ensure org
  :init
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t))

(use-package org-src
  :ensure org
  :init
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-content-indentation 0))

(use-package org-list
  :ensure org
  :init
  (setq org-list-allow-alphabetical t)
  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))))

(use-package org-agenda
  :ensure org
  :general
  (-leader-def
    "Oa" '(org-agenda :wk "agenda"))
  :init
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-tags-column 0))

(use-package org-faces
  :ensure org
  :custom-face
  (org-tag              ((t (:inherit shadow))))
  (org-ellipsis         ((t (:underline nil))))
  (org-block-begin-line ((t (:underline nil))))
  (org-block-end-line   ((t (:overline nil))))
  :init
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-priority-faces
        '((?A . (:inherit (bold error)))
          (?B . (:inherit (bold warning)))
          (?C . (:inherit (bold success)))))
  (setq org-todo-keyword-faces
        '(("STARTED"   . (:inherit (bold font-lock-constant-face org-todo)))
          ("NEXT"      . (:inherit (bold font-lock-constant-face org-todo)))
          ("WAITING"   . (:inherit (bold warning org-todo)))
          ("HOLD"      . (:inherit (bold warning org-todo)))
          ("OBSOLETE"  . (:inherit (bold shadow org-todo)))
          ("CANCELLED" . (:inherit (bold shadow org-todo))))))

(use-package org-bullets
  :init
  (setq org-bullets-bullet-list '("•"))
  (setq org-bullets--keywords
        `(("^\\*+ "
           (0 (let* ((level (- (match-end 0) (match-beginning 0) 1)))
                (compose-region (- (match-end 0) 2)
                                (- (match-end 0) 1)
                                (org-bullets-level-char level))
                (dolist (n (number-sequence
                            (match-beginning 0)
                            (- (match-end 0) 3)))
                  (compose-region n (+ n 1) " "))
                (put-text-property (match-beginning 0)
                                   (- (match-end 0) 2)
                                   'face (list :inherit 'org-hide))
                nil)))))
  :hook
  (org-mode-hook . org-bullets-mode))

(use-package toc-org
  :init
  (setq toc-org-max-depth 4)
  :hook
  (org-mode-hook . toc-org-enable))

(use-package ob-core
  :ensure org
  :init
  (setq org-babel-load-languages
        '((emacs-lisp . t)
          (shell      . t)
          (plantuml   . t)))
  :hook
  (org-babel-after-execute-hook . org-redisplay-inline-images))

(use-package ob-plantuml
  :ensure nil
  :init
  (setq org-plantuml-exec-mode 'plantuml))

(use-package verb
  ;; :after org
  :general
  (org-mode-map
   "C-c C-r" '(:keymap verb-command-map :package verb :wk "verb"))
  :init
  (setq verb-auto-kill-response-buffers t)
  (setq verb-json-use-mode 'json-ts-mode)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((verb . t))))

(use-package treesit-auto
  :init
  (setq treesit-auto-install 'prompt)
  :hook
  (after-init-hook . global-treesit-auto-mode))

(use-package eglot
  :init
  (setq eglot-autoshutdown t))

(use-package eglot-booster
  :if (executable-find "emacs-lsp-booster")
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :rev :newest)
  ;; :after eglot
  :init
  (setq eglot-booster-no-remote-boost t)
  :hook
  (after-init-hook . eglot-booster-mode))

(use-package eglot-hierarchy
  :vc (:url "https://github.com/dolmens/eglot-hierarchy" :rev :newest))

(use-package flycheck-eglot
  :demand
  :after flycheck eglot
  :init
  (setq flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode))

(use-package dape
  :custom-face
  (dape-breakpoint-face ((t (:inherit error))))
  :init
  (setq dape-key-prefix (kbd "C-x C-a"))
  (setq dape-inlay-hints t)
  ;; (setq dape-buffer-window-arrangement 'right)
  (setq dape-buffer-window-arrangement 'gud)
  (setq dape-info-hide-mode-line nil)
  :config
  (dape-breakpoint-global-mode)
  :hook
  (kill-emacs-hook . dape-breakpoint-save)
  (after-init-hook . dape-breakpoint-load))

(use-package repeat
  :ensure nil
  :config
  (repeat-mode))

(use-package highlight-defined
  :init
  (setq highlight-defined-face-use-itself t)
  :hook
  (emacs-lisp-mode-hook . highlight-defined-mode))

(use-package highlight-quoted
  :hook
  (emacs-lisp-mode-hook . highlight-quoted-mode))

(use-package erefactor
  :general
  (-local-leader-def :keymaps 'emacs-lisp-mode-map
    "R" '(:keymap erefactor-map :wk "refactor")))

(use-package eros
  :custom-face
  (eros-result-overlay-face ((t (:inherit shadow :box t))))
  :hook
  (emacs-lisp-mode-hook . eros-mode))

(use-package package-lint)

(use-package flycheck-package
  :after flycheck
  :demand
  :config
  (flycheck-package-setup))

(use-package clojure-ts-mode)

(use-package flycheck-clj-kondo
  :preface
  (defun -setup-flycheck-clj-kondo ()
    (require 'flycheck-clj-kondo))
  :hook
  (clojure-ts-mode-hook . -setup-flycheck-clj-kondo))

(use-package cider
  :custom-face
  (cider-result-overlay-face ((t (:inherit shadow :box t))))
  :general
  (-local-leader-def :keymaps 'clojure-ts-mode-map
    "c" '(:ignore t           :wk "connect")
    "cc" '(cider-jack-in      :wk "jack-in")
    "cj" '(cider-jack-in-clj  :wk "jack-in-clj")
    "cs" '(cider-jack-in-cljs :wk "jack-in-cljs")
    "cC" '(cider-connect      :wk "connect")
    "cR" '(cider-restart      :wk "restart")
    "cQ" '(cider-quit         :wk "quit")

    "b" '(:ignore t           :wk "buffer")
    "bs" 'cider-scratch

    "=" '(cider-format-buffer :wk "format"))
  :init
  (setq cider-eldoc-display-context-dependent-info t)
  :hook
  (clojure-ts-mode-hook . cider-mode))

(use-package cider-hydra
  :general
  (-local-leader-def :keymaps 'clojure-ts-mode-map
    "d" '(cider-hydra-doc/body  :wk "doc")
    "e" '(cider-hydra-eval/body :wk "eval")
    "t" '(cider-hydra-test/body :wk "test")
    "r" '(cider-hydra-repl/body :wk "repl"))
  :hook
  (clojure-ts-mode-hook . cider-hydra-mode))

(use-package clj-refactor
  :general
  (-local-leader-def :keymaps 'clojure-ts-mode-map
    "R" '(hydra-cljr-help-menu/body :wk "refactor"))
  :hook
  (clojure-ts-mode-hook . clj-refactor-mode))

(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'"
  :init
  (setq go-ts-mode-indent-offset 4)
  :hook
  (go-ts-mode-hook . eglot-ensure))

(use-package go-mod-ts-mode
  :ensure go-ts-mode
  :mode "go\\.mod\\'")

(use-package makefile-executor
  :general
  (-local-leader-def :keymaps 'makefile-mode-map
    "e" 'makefile-executor-execute-target)
  :hook
  (makefile-mode-hook . makefile-executor-mode))

(use-package just-mode)

(use-package justl
  :general
  (:keymaps 'justl-mode-map :states 'normal
            "?" 'justl-help-popup
            "e" 'justl-exec-recipe
            "E" 'justl-exec-eshell
            "w" 'justl--exec-recipe-with-args
            "W" 'justl-no-exec-eshell)
  (-local-leader-def :keymaps 'just-mode-map
    "e" '(:ignore t :wk "eval")
    "e." 'justl
    "ee" 'justl-exec-recipe-in-dir))

(use-package web-mode
  :mode "\\.html?\\'"
  :init
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-comment-interpolation t)
  (setq web-mode-enable-current-element-highlight t))

(use-package plantuml-mode
  :general
  (-local-leader-def :keymaps 'plantuml-mode-map
    "p" '(plantuml-preview :wk "preview"))
  :init
  (setq plantuml-output-type (if (display-images-p) "png" "txt"))
  (setq plantuml-default-exec-mode 'executable))

(use-package sql
  :ensure nil
  :general
  (-local-leader-def :keymaps 'sql-mode-map
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
  :init
  (setq sql-connection-alist '((pg-local
                                (sql-product 'postgres)
                                (sql-port 5432)
                                (sql-server "localhost")
                                (sql-user "postgres")
                                (sql-password "postgres")
                                (sql-database "postgres")))))

(use-package groovy-mode)

(use-package markdown-mode
  :custom-face
  (markdown-code-face ((t (:inherit default))))
  :general
  (-local-leader-def :keymaps 'markdown-mode-map
    "." '(:keymap markdown-mode-command-map))
  :init
  (setq markdown-command "pandoc")
  (setq markdown-fontify-code-blocks-natively t)
  :config
  (add-to-list 'markdown-code-lang-modes '("clj" . clojure-mode)))

(use-package grip-mode
  :general
  (-local-leader-def :keymaps 'markdown-mode-map
    "g" 'grip-mode)
  :init
  (setq grip-update-after-change nil)
  (setq grip-preview-use-webkit t))

(use-package markdown-toc)

(use-package edit-indirect)

(use-package json-ts-mode
  :ensure nil
  :general
  (-local-leader-def :keymaps 'json-ts-mode-map
    "=" '(json-pretty-print-buffer :wk "format")))

(use-package yaml-ts-mode
  :ensure nil
  :hook
  (yaml-ts-mode-hook . flycheck-mode)
  (yaml-ts-mode-hook . highlight-indent-guides-mode))

(use-package lua-mode
  :init
  (setq lua-indent-level 2)
  :hook
  (lua-mode-hook . eglot-ensure))

(use-package sh-script
  :ensure nil)

(use-package executable
  :ensure nil
  :hook
  (after-save-hook . executable-make-buffer-file-executable-if-script-p))

(use-package flymake-shellcheck
  :hook
  (sh-mode-hook . flymake-shellcheck-load))

(use-package vimrc-mode)

(use-package ssh-config-mode
  :init
  (autoload 'ssh-config-mode "ssh-config-mode" t))

(use-package protobuf-ts-mode
  :mode "\\.proto\\'")

(use-package xwidget
  :if (display-graphic-p)
  :ensure nil
  :general
  (-leader-def
    "ow" 'xwidget-webkit-browse-url))

(use-package xwwp
  :if (display-graphic-p)
  :after xwidget
  :general
  (:keymaps 'xwidget-webkit-mode-map :states 'normal
            "f" 'xwwp-follow-link))

(use-package editorconfig
  :ensure nil
  :hook
  (after-init-hook . editorconfig-mode))

(use-package docker
  :general
  (-leader-def
    "od" 'docker))

(use-package dockerfile-mode
  :general
  (-local-leader-def :keymaps 'dockerfile-mode-map
    "b" 'dockerfile-build-buffer
    "B" 'dockerfile-build-no-cache-buffer))

(use-package docker-compose-mode
  :general
  (-local-leader-def :keymaps 'docker-compose-mode-map
    "." 'docker-compose))

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package ansible-vault-with-editor
  :vc (:url "https://github.com/rynffoll/ansible-vault-with-editor" :rev :newest)
  :general
  (-local-leader-def :keymaps 'yaml-ts-mode-map
    "e" '(ansible-vault-with-editor-edit :wk "edit")
    "E" '(ansible-vault-with-editor-encrypt :wk "encrypt")
    "D" '(ansible-vault-with-editor-decrypt :wk "decrypt")))

(use-package direnv
  :if (executable-find "direnv")
  :preface
  (defun -direnv-hook ()
    (add-hook
     'after-save-hook
     (lambda ()
       (call-interactively 'direnv-update-environment))
     nil t))
  :general
  (-local-leader-def :keymaps 'direnv-envrc-mode-map
    "a" 'direnv-allow
    "u" 'direnv-update-environment)
  :init
  (setq direnv-always-show-summary nil)
  :hook
  (after-init-hook . direnv-mode)
  (direnv-envrc-mode-hook . -direnv-hook))

(use-package envrc
  :disabled
  :if (executable-find "direnv")
  :hook
  (after-init-hook . envrc-global-mode))

(use-package focus
  :general
  (-leader-def
    "tf" 'focus-mode))

(use-package olivetti
  :general
  (-leader-def
    "to" 'olivetti-mode))

(use-package crux
  :general
  (-leader-def
    "fR" 'crux-rename-file-and-buffer
    "fD" 'crux-delete-file-and-buffer))

(use-package deadgrep
  :general
  (-leader-def
    "/D" 'deadgrep))

(use-package try)

(use-package password-generator)

(use-package string-inflection)

(use-package show-font)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

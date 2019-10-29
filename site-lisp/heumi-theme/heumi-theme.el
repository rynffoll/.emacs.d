;;; heumi-theme.el --- Heumi Theme

;; Copyright (C) 2019  Ruslan Kamashev

;; Author: Ruslan Kamashev <rynffoll@gmail.com>
;; Keywords: themes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defgroup heumi
  nil
  "Heumi Theme"
  :group 'faces)

(deftheme heumi "Heumi Theme.")

(let ((fg "#333")
      (bg "#fafafa")

      (grey-0 "#f5f5f5")
      (grey-1 "#f0f0f0")
      (grey-2 "#e0e0e0")
      (grey-3 "#ccc")
      (grey-4 "#666")

      (yellow "#ffffdf")

      (green-0 "#f3ffe0")
      (green-1 "#9fef9d")
      (green-2 "#39a362")

      (magenta-0 "#fae6fa")
      (magenta-1 "#c6617e")

      (red-0 "#ff9090")
      (red-1 "#d64f4f")

      (blue-0 "#e0ecf7")
      (blue-1 "#b4d8fd")
      (blue-2 "#3c70a4")

      (orange-0 "#f6c89f")
      (orange-1 "#e6921e"))
  (custom-theme-set-faces
   `heumi
   `(default ((t (:background ,bg :foreground ,fg))))
   `(region ((t (:background ,blue-0))))
   `(vertical-border ((t :foreground ,grey-3)))
   `(hl-line ((t (:background ,grey-1 :extend t))))

   `(success ((t (:foreground ,green-2 :weight bold))))
   `(warning ((t (:foreground ,orange-1 :weight bold))))
   `(error ((t (:foreground ,red-1 :weight bold))))

   `(font-lock-builtin-face ((t (:foreground ,magenta-1))))
   `(font-lock-constant-face ((t (:background ,magenta-0))))
   `(font-lock-comment-face ((t (:background ,yellow :foreground ,grey-4 :extend nil))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :weight bold))))
   `(font-lock-string-face ((t (:background ,green-0 :extend nil))))
   `(font-lock-doc-face ((t (:inherit font-lock-string-face :slant italic))))
   `(font-lock-function-name-face ((t (:foreground ,blue-2))))
   `(font-lock-keyword-face ((t (:foreground ,fg))))
   `(font-lock-negation-char-face ((t (:foreground ,fg :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,fg :weight bold))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,fg :weight bold))))
   `(font-lock-type-face ((t (:foreground ,fg))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))

   ;; dired
   `(dired-directory ((t (:foreground ,blue-2))))

   ;; dired-async
   `(dired-async-message ((t (:inherit warning))))
   `(dired-async-failures ((t (:inherit error))))
   `(dired-async-mode-message ((t (:inherit success))))

   ;; Info
   `(Info-quoted ((t (:underline t :weight bold))))

   ;; diff
   `(diff-added ((t (:background ,green-1))))
   `(diff-removed ((t (:background ,red-0))))
   `(diff-changed ((t (:background ,blue-1))))
   `(diff-indicator-added ((t (:weight bold))))
   `(diff-indicator-removed ((t (:inherit diff-indicator-added))))
   `(diff-indicator-changed ((t (:inherit diff-indicator-added))))

   ;; diff-hl
   `(diff-hl-insert ((t (:inherit diff-added))))
   `(diff-hl-delete ((t (:inherit diff-removed))))
   `(diff-hl-change ((t (:inherit diff-changed))))
   `(diff-hl-margin-insert ((t (:inherit diff-added))))
   `(diff-hl-margin-delete ((t (:inherit diff-removed))))
   `(diff-hl-margin-change ((t (:inherit diff-changed))))
   `(diff-hl-dired-insert ((t (:inherit diff-added))))
   `(diff-hl-dired-delete ((t (:inherit diff-removed))))
   `(diff-hl-dired-change ((t (:inherit diff-changed))))

   ;; eshell-fringe-status
   `(eshell-fringe-status-success ((t (:background ,green-1))))
   `(eshell-fringe-status-failure ((t (:background ,red-0))))

   ;; minibuffer
   `(minibuffer-prompt ((t (:background ,blue-0 :foreground ,fg :weight bold :box nil))))

   ;; ivy
   `(ivy-current-match ((t (:background ,blue-0 :foreground ,fg))))

   ;; mode-line
   `(mode-line ((t (:background ,grey-1))))
   `(mode-line-inactive ((t (:background ,grey-0 :foreground ,grey-4))))

   ;; outline
   `(outline-1 ((t (:inherit default :weight bold :extend t))))
   `(outline-2 ((t (:inherit outline-1))))
   `(outline-3 ((t (:inherit outline-1))))
   `(outline-4 ((t (:inherit outline-1))))
   `(outline-5 ((t (:inherit outline-1))))
   `(outline-6 ((t (:inherit outline-1))))
   `(outline-7 ((t (:inherit outline-1))))
   `(outline-8 ((t (:inherit outline-1))))

   ;; org
   `(org-level-1 ((t (:inherit outline-1))))
   `(org-level-2 ((t (:inherit outline-2))))
   `(org-level-3 ((t (:inherit outline-3))))
   `(org-level-4 ((t (:inherit outline-4))))
   `(org-level-5 ((t (:inherit outline-5))))
   `(org-level-6 ((t (:inherit outline-6))))
   `(org-level-7 ((t (:inherit outline-7))))
   `(org-level-8 ((t (:inherit outline-8))))
   `(org-tag ((t (:weight bold))))
   `(org-hide ((t (:foreground ,bg))))
   `(org-done ((t (:foreground ,green-2 :weight bold))))
   `(org-todo ((t (:foreground ,red-1 :weight bold))))
   `(org-block ((t (:background ,grey-0))))
   `(org-block-begin-line ((t (:background ,grey-1))))
   `(org-block-end-line ((t (:inherit org-block-begin-line))))
   `(org-tag ((t (:inherit shadow))))
   `(org-ellipsis ((t (:underline nil))))
   `(org-meta-line ((t (:inherit shadow))))
   `(org-document-info-keyword ((t (:inherit shadow))))
   `(org-special-keyword ((t (:inherit shadow))))
   `(org-indent ((t (:inherit org-hide :extend t))))

   ;; org-agenda
   `(org-agenda-structure ((t (:foreground ,blue-2 :weight bold :height 1.1))))
   `(org-agenda-date ((t (:inherit org-agenda-structure))))
   `(org-agenda-date-today ((t (:inherit org-agenda-date :underline t))))
   `(org-agenda-date-weekend ((t (:inherit org-agenda-date :foreground ,green-2))))

   ;; show-paren
   `(show-paren-match ((t (:background ,orange-0 :weight bold))))
   `(show-paren-mismatch ((t (:background ,red-0 :weight bold))))

   ;; eros
   `(eros-result-overlay-face ((t (:background ,grey-1))))

   ;; cider
   `(cider-result-overlay-face ((t (:background ,grey-1))))

   ;; treemacs
   `(treemacs-directory-face ((t (:inherit dired-directory))))
   `(treemacs-git-ignored-face ((t (:inherit shadow))))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'heumi)
;;; heumi-theme.el ends here

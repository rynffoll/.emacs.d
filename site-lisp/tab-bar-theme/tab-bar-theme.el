;;; tab-bar-theme.el --- Tab Bar Theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ruslan Kamashev

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

;; Theme for `tab-bar-mode'

;;; Code:

(require 'tab-bar)


(defgroup tab-bar-theme
  nil
  "Tab Bar Theme."
  :group 'tab-bar)


;;;###autoload
(defun tab-bar-theme--apply (&optional _theme)
  "Apply faces to current theme."
  (let* ((bg-tab-inactive (face-attribute 'mode-line-inactive :background))
         (fg-tab-inactive (face-attribute 'mode-line-inactive :foreground))
         (bg-tab-active   (face-attribute 'default :background))
         (fg-tab-active   (face-attribute 'default :foreground))
         (box-line-width  3))
    (custom-set-faces

     `(tab-bar
       ((t ( :inherit unspecified
             :background ,bg-tab-inactive
             :foreground ,fg-tab-inactive
             :box (:line-width ,box-line-width :color ,bg-tab-inactive :style nil)))))

     `(tab-bar-tab
       ((t ( :inherit unspecified
             :background ,bg-tab-active
             :foreground ,fg-tab-active
             :overline t
             :box (:line-width ,box-line-width :color ,bg-tab-active :style nil)))))

     `(tab-bar-tab-inactive
       ((t ( :inherit unspecified
             :background ,bg-tab-inactive
             :foreground ,fg-tab-inactive
             :box (:line-width ,box-line-width :color ,bg-tab-inactive :style nil)))))

     `(tab-bar-tab-ungrouped
       ((t ( :inherit unspecified
             :background ,bg-tab-inactive
             :foreground ,fg-tab-inactive
             :box (:line-width ,box-line-width :color ,bg-tab-inactive :style nil)))))

     `(tab-bar-tab-group-inactive
       ((t ( :inherit unspecified
             :background ,bg-tab-inactive
             :foreground ,fg-tab-inactive
             :weight bold
             :box (:line-width ,box-line-width :color ,bg-tab-inactive :style nil)))))

     `(tab-bar-tab-group-current
       ((t ( :inherit unspecified
             :background ,bg-tab-inactive
             :foreground ,fg-tab-active
             :weight bold
             :box (:line-width ,box-line-width :color ,bg-tab-inactive :style nil))))))))

;;;###autoload
(define-minor-mode tab-bar-theme-mode
  "Toggle `tab-bar-theme-mode'."
  :global t :group 'tab-bar-theme
  (cond
   (tab-bar-theme-mode
	(tab-bar-theme--apply)
	(add-hook 'enable-theme-functions #'tab-bar-theme--apply))
   (t
	(remove-hook 'enable-theme-functions #'tab-bar-theme--apply))))

(provide 'tab-bar-theme)
;;; tab-bar-theme.el ends here

;;; per-tab-group-theme.el --- Per Tab Group Theme  -*- lexical-binding: t; -*-

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


(defgroup per-tab-group-theme
  nil
  "Per Tab Group Theme."
  :group 'tab-bar)



(defvar per-tab-group-theme--alist nil
  "List of tab groups and their themes.")


;;;###autoload
;; FIXME: arg names
(defun per-tab-group-theme--apply (&optional _a1 _a2)
  "Apply faces to current theme."
  (let* ((tab-group (alist-get 'group (tab-bar--current-tab)))
         (theme (assoc tab-group per-tab-group-theme--alist)))

    (if theme
        (load-theme (cdr theme) :no-confirm)
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme 'modus-operandi :no-confirm))

    ;; (cond
    ;;  (theme
    ;;   (load-theme 'ef-dream :no-confirm))
    ;;  (t
    ;;   (mapc #'disable-theme custom-enabled-themes)
    ;;   (load-theme 'modus-operandi :no-confirm)))
    ))

;;;###autoload
(define-minor-mode per-tab-group-theme-mode
  "Toggle `per-tab-group-theme-mode'."
  :global t :group 'per-tab-group-theme
  (cond
   (per-tab-group-theme-mode
    (add-to-list 'tab-bar-tab-post-select-functions #'per-tab-group-theme--apply)
    (add-to-list 'tab-bar-tab-post-open-functions #'per-tab-group-theme--apply)
    )
   (t
    (setq tab-bar-tab-post-select-functions
          (delete #'per-tab-group-theme--apply tab-bar-tab-post-select-functions))
    (setq tab-bar-tab-post-open-functions
          (delete #'per-tab-group-theme--apply tab-bar-tab-post-open-functions))
    )))

(provide 'per-tab-group-theme)
;;; per-tab-group-theme.el ends here

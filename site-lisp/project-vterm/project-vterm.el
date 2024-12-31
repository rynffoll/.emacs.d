;;; project-vterm.el --- Project Vterm  -*- lexical-binding: t; -*-

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

;; Integration `vterm' and `project'

;;; Code:

(require 'project)
(require 'vterm)


(defgroup project-vterm
  nil
  "Integration vterm and project."
  :group 'project)



;; Source: https://www.reddit.com/r/emacs/comments/wu5rxi/tramp_projectel_vterm_integration_help/

;;;###autoload
(defun project-vterm ()
  "Open vterm in project root."
  (interactive)
  (defvar vterm-buffer-name)
  (let* ((default-directory (project-root (project-current t)))
         (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer      (get-buffer vterm-buffer-name)))
    (if (and vterm-buffer (not current-prefix-arg))
        (pop-to-buffer vterm-buffer
                       (bound-and-true-p display-comint-buffer-action))
      (vterm))))

(provide 'project-vterm)
;;; project-vterm.el ends here

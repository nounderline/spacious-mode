;;; spacious-mode.el --- Emacs mode that gives you space to think.

;; Author: Rafael Gutkowski <rgtk@protonmail.com>
;; URL: https://github.com/rgtk/spacious-mode
;; Version: 1.1.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.
;; If not, see http://www.gnu.org/licenses.

;;; Commentary

;; spacious-mode balances available whitespace for more aesthetic experience
;; with your Emacs.

;;; Code:

(defun spacious-mode--reset-window (&optional window)
  (set-window-margins (or window (get-buffer-window)) 0 0))

(defun spacious-mode--adjust-windows ()
  (walk-windows
   (lambda (window)
     (unless (window-minibuffer-p window)
       (let ((margin (max 0 (floor (/ (- (window-total-width window) 80) 2)))))
         (set-window-margins window margin margin))))))

(defun spacious-mode-on ()
  (spacious-mode +1))

(defun spacious-mode (&optional toggle)
  "Global mode that centers window content spreading whitespace evenly
for more aesthetic composition."
  (interactive)

  (unless (boundp 'spacious-mode)
    (setq spaious-mode nil))

  (if toggle
      (setq spacious-mode (<= toggle 0)))

  (if spacious-mode
      (progn
        (setq spacious-mode nil)

        (remove-hook 'window-configuration-change-hook 'spacious-mode--adjust-windows)
        (remove-hook 'window-setup-hook 'spacious-mode-on)
        (advice-remove 'split-window-right 'spacious-mode--reset-window)

        (walk-windows
         (lambda (window)
           (unless (window-minibuffer-p window)
             (spacious-mode--reset-window window))))

        (message "Spacious mode disabled"))

    (progn
      (setq spacious-mode t)

      (add-hook 'window-configuration-change-hook 'spacious-mode--adjust-windows)
      (add-hook 'window-setup-hook 'spacious-mode-on)
      (advice-add 'split-window-right :before 'spacious-mode--reset-window)

      (spacious-mode--adjust-windows)

      (message "Spacious mode enabled"))))

(provide 'spacious-mode)

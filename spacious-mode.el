(defun spacious-mode--reset-window (&optional window)
  (set-window-margins (or window (get-buffer-window)) 0 0))

(defun spacious-mode--adjust-windows ()
  (walk-windows
   (lambda (window)
     (unless (window-minibuffer-p window)
       (let ((margin (floor (/ (- (window-total-width window) 80) 2))))
         (set-window-margins window margin margin))))))

(defun spacious-mode-on ()
  (spacious-mode +1))

(defun spacious-mode-off (&optional any)
  (spacious-mode -1))

;; TODO walk all touched windows
(defun spacious-mode (&optional toggle)
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

    (if (window-parent)
        (message "Spaciousness can be only obtained with one window")

      (progn
        (setq spacious-mode t)

        (add-hook 'window-configuration-change-hook 'spacious-mode--adjust-windows)
        (add-hook 'window-setup-hook 'spacious-mode-on)
        (advice-add 'split-window-right :before 'spacious-mode--reset-window)

        (let* ((win (get-buffer-window))
               (content-width 80)
               (full-width (window-total-width))
               (margin (floor (/ (- full-width content-width) 2))))
          (set-window-margins win margin margin))

        (message "Spacious mode enabled")))))

(provide 'spacious-mode)

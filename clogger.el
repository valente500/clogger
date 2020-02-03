(require 'clogger-logging)

(setq hungry-mode-last-buffer nil)
; TODO: This should be a custom
(setq hungry-mode-prefix-commands
      '(digit-argument
	universal-argument))

(defun help-echo-p (key)
  (and (consp key) (equal (car key) 'help-echo)))

(defun update-window-points ()
  (when-let ((w (get-buffer-window (current-buffer))))
    (set-window-point w (point))))

(defun hungry-mode-message ()
  (format "%-31s %s"
	  (key-description (seq-remove #'help-echo-p (recent-keys)))
	  (symbol-name this-original-command)))

(defun hungry-mode-add-to-command-log ()
  (let ((inhibit-message t)
	(b (get-buffer-create "*commands*")))
    (with-current-buffer b
      (push-log (hungry-mode-message))
      (update-window-points)
      (clear-this-command-keys))))

(defun hungry-mode-callback ()
  (let ((curr-buf
	 (current-buffer))
	(old-hungry-mode
	 (and (bound-and-true-p hungry-mode-last-buffer)
	      (with-current-buffer hungry-mode-last-buffer (bound-and-true-p hungry-mode)))))
    (cond
     ((and (not old-hungry-mode) hungry-mode)
      (clear-this-command-keys))
     ((and hungry-mode (not (member this-original-command hungry-mode-prefix-commands)))
      (hungry-mode-add-to-command-log)))
    (setq hungry-mode-last-buffer curr-buf)))

(define-minor-mode hungry-mode
  "Log keypresses and commands to *commands* buffer"
  nil " Hungry" nil
  (if (bound-and-true-p hungry-mode)
      (progn
	(message "Enabled hungry-mode")
	(add-hook 'post-command-hook 'hungry-mode-callback nil))
    (progn
      (message "Disabled hungry-mode")
      (remove-hook 'post-command-hook 'hungry-mode-callback))))

(provide 'clogger)


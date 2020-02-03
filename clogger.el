(require 'clogger-logging)

; The buffer that the last command was run in.
(setq clogger-mode-last-buffer nil)

; TODO: This should be a custom
; A list of prefix commands. When a user enters any of these commands,
; they are ignored, and the keys are treated as part of the next command.
(setq clogger-mode-prefix-commands
      '(digit-argument
	universal-argument))

(defun help-echo-p (key)
  "Whether KEY is a key of <help-echo> type."
  (and (consp key) (equal (car key) 'help-echo)))

(defun update-window-points ()
  "Set the window point of all windows showing the current buffer
to the buffer's point."
  (when-let ((w (get-buffer-window (current-buffer))))
    (set-window-point w (point))))

(defun clogger-mode-message ()
  "Returns a formatted log message."
  (format "%-31s %s"
	  (key-description (seq-remove #'help-echo-p (recent-keys)))
	  (symbol-name this-original-command)))

(defun clogger-mode-add-to-command-log ()
  "Add a log message to the end of the *commands* buffer"
  (let ((inhibit-message t)
	(b (get-buffer-create "*commands*")))
    (with-current-buffer b
      (push-log (clogger-mode-message))
      (update-window-points)
      (clear-this-command-keys))))

(defun clogger-mode-callback ()
  (let ((curr-buf
	 (current-buffer))
	(old-clogger-mode
	 (and (bound-and-true-p clogger-mode-last-buffer)
	      (with-current-buffer clogger-mode-last-buffer (bound-and-true-p clogger-mode)))))
    (cond
     ((and (not old-clogger-mode) clogger-mode)
      (clear-this-command-keys))
     ((and clogger-mode (not (member this-original-command clogger-mode-prefix-commands)))
      (clogger-mode-add-to-command-log)))
    (setq clogger-mode-last-buffer curr-buf)))

(define-minor-mode clogger-mode
  "Log keypresses and commands to *commands* buffer"
  nil " Clogger" nil
  (if (bound-and-true-p clogger-mode)
      (progn
	(message "Enabled clogger-mode")
	(add-hook 'post-command-hook 'clogger-mode-callback nil))
    (progn
      (message "Disabled clogger-mode")
      (remove-hook 'post-command-hook 'clogger-mode-callback))))

(provide 'clogger)


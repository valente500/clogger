
(defun log-line-to-entry (line)
  "Parses the log line LINE and converts it to a log entry.
The log entry returned is a cons cell (MESSAGE . COUNT)"
  (if (string-match "^\\(.*\\) \\[\\([0-9]+\\) times\\]$" line)
      (cons (match-string 1 line) (string-to-number (match-string 2 line)))
    (cons line 1)))

(defun entry-to-log-line (entry)
  "Converts a log entry into a line.
The log entry ENTRY should be a cons cell (MESSAGE . COUNT)"
  (if (/= (cdr entry) 1)
      (format "%s [%d times]" (car entry) (cdr entry))
    (car entry)))

(defun remove-last-line ()
  "Removes the last line of the current buffer and returns it.
If the buffer is empty, returns nil."
  (goto-char (point-max))
  (when-let* ((last-line-point-min (re-search-backward "^." nil t))
	 (last-line (string-trim (buffer-substring last-line-point-min (point-max)))))
    (delete-region last-line-point-min (point-max))
    last-line))

(defun push-log-entry (entry)
  "Adds a log entry to the end of the current buffer.
The log entry ENTRY should be a cons cell (MESSAGE . COUNT)"
  (goto-char (point-max))
  (insert (entry-to-log-line entry) "\n"))

(defun pop-log-entry ()
  "Removes a log entry from the end of the current buffer.
The log entry returned is a cons cell (MESSAGE . COUNT)"
  (when-let ((line (remove-last-line)))
      (log-line-to-entry line)))

(defun push-log (message)
  "Adds a log message to the end of the current buffer."
  (let ((last-entry (pop-log-entry)))
    (cond
     ((and last-entry (equal (car last-entry) message))
      (push-log-entry (cons (car last-entry) (1+ (cdr last-entry)))))
     (last-entry
      (push-log-entry last-entry)
      (push-log-entry (cons message 1)))
     (t
      (push-log-entry (cons message 1))))))

(provide 'clogger-logging)



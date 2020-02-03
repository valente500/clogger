
(defun log-line-to-cell (s)
  (if (string-match "^\\(.*\\) \\[\\([0-9]+\\) times\\]$" s)
      (cons (match-string 1 s) (string-to-number (match-string 2 s)))
    (cons s 1)))

(defun cell-to-log-line (cell)
  (if (/= (cdr cell) 1)
      (format "%s [%d times]" (car cell) (cdr cell))
    (car cell)))

(defun remove-last-line ()
  (goto-char (point-max))
  (when-let* ((last-line-point-min (re-search-backward "^." nil t))
	 (last-line (string-trim (buffer-substring last-line-point-min (point-max)))))
    (delete-region last-line-point-min (point-max))
    last-line))

(defun push-log-entry (cell)
  (goto-char (point-max))
  (insert (cell-to-log-line cell) "\n"))

(defun pop-log-entry ()
  (when-let ((line (remove-last-line)))
      (log-line-to-cell line)))

(defun push-log (s)
  (let ((last-entry (pop-log-entry)))
    (cond
     ((and last-entry (equal (car last-entry) s))
      (push-log-entry (cons (car last-entry) (1+ (cdr last-entry)))))
     (last-entry
      (push-log-entry last-entry)
      (push-log-entry (cons s 1)))
     (t
      (push-log-entry (cons s 1))))))

(provide 'clogger-logging)



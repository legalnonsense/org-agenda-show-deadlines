;;; org-agenda-show-deadlines.el --- Show deadlines in agenda views  -*- lexical-binding: t; -*-
(require 'org)
(require 'org-agenda)
(require 'ts)
(require 'subr-x)
(require 'dash)

(defcustom org-agenda-show-deadlines-column 100
  "Column to align the deadlines in the agenda buffer. Will throw error if the deadline column collides with agenda text")

(defcustom org-agenda-show-deadlines-next-line-p nil
  "Non-nil inserts deadlines on the line after the entry")

(defcustom org-agenda-show-deadlines-fill-char "."
  "Fill character between entry and deadline. Use a space for no fill. \"_\" also works.")

(defcustom org-agenda-show-deadlines-date-format  "%Y-%m-%d"
  "Time format for deadlines. In the format of `format-time-string'."
  :type  'string)

(defcustom org-agenda-show-deadlines-change-function nil
  "Function called on the deadline string before it is displayed. If `nil', then do nothing. The date string is the only argument")

(defcustom org-agenda-show-deadlines-show-active-timestamps-p t
  "Show active timestamps in the agenda if no deadline is available.")

(defun org-agenda-show-deadlines--insert-deadlines ()
  "Insert deadlines and active timestamps into agenda view. Deadlines take precedence over timestamps"
  (goto-char (point-min))
  (when (org-agenda-next-item 1)
    (cl-loop with
	     padding-char = (string-to-char org-agenda-show-deadlines-fill-char)
	     do
	     (org-agenda-next-item 1)
	     (when-let*
		 ((time (or (org-agenda-with-point-at-orig-entry
			     nil (cdar (org-entry-properties (point) "DEADLINE")))
			    (when org-agenda-show-deadlines-show-active-timestamps-p
			      (funcall (lambda ()
					 (let ((timestamp (org-agenda-with-point-at-orig-entry
							   nil
							   (cdar (org-entry-properties (point) "TIMESTAMP")))))
					   (when (string-match (org-re-timestamp 'active) timestamp)
					     timestamp)))))))
		  (time (->> time
			     (ts-parse-org)
			     (ts-format org-agenda-show-deadlines-date-format)
			     ((lambda (d) (if (fboundp org-agenda-show-deadlines-change-function)
					      (funcall org-agenda-show-deadlines-change-function d)
					    d)))))
		  (padding-string (if org-agenda-show-deadlines-next-line-p
				      (concat "\n" (make-string org-agenda-show-deadlines-column
								padding-char))
				    (make-string ((lambda (col)   ;; If there's going to be a collision with agenda text,
						    (if (< col 0) ;; tell the user to fix it. 
							(signal 'user-error
								`(,(concat "The headings are too long. "
									   "Increase the value `org-agenda-show-deadlines-column' "
									   "to avoid collisions.")))
						      col))
						  (- org-agenda-show-deadlines-column (save-excursion (end-of-line)
												      (current-column))))
						 padding-char))))
	       (end-of-line)
	       (insert padding-string time))
	     until
	     (save-excursion (forward-line)  ;; `org-agenda-next-item' does not return `nil' at the last item
			     (eobp))))       ;; so need to check it manually.
  (goto-char (point-min)))



;;;###autoload
(define-minor-mode org-agenda-show-deadlines-mode
  "Show deadlines in org-agenda buffers"
  nil
  " OASD"
  nil
  (if org-agenda-show-deadlines-mode
      (progn 
	(add-hook 'org-agenda-finalize-hook #'org-agenda-show-deadlines--insert-deadlines t)
	(define-key org-agenda-mode-map (kbd "C-d") #'org-agenda-show-deadlines))
    (remove-hook 'org-agenda-finalize-hook #'org-agenda-show-deadlines--insert-deadlines)
    (define-key org-agenda-mode-map (kbd "C-d") #'delete-char)))

  (provide 'org-agenda-show-deadlines)



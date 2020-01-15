;;; org-agenda-show-deadlines.el --- Show deadlines in agenda views  -*- lexical-binding: t; -*-

(require 'ts)
(require 'subr-x)

(setq org-agenda-show-deadlines-column 100)
(defcustom org-agenda-show-deadlines-column 100
  "Column to align the deadlines in the agenda buffer. Will throw error if the deadline column collides with agenda text")

(setq org-agenda-show-deadlines-next-line-p t)
(setq org-agenda-show-deadlines-next-line-p nil)
(defcustom org-agenda-show-deadlines-next-line-p nil
  "Non-nil inserts deadlines on the line after the entry")

(setq org-agenda-show-deadlines-fill-char "_")
(setq org-agenda-show-deadlines-fill-char ".")
(setq org-agenda-show-deadlines-fill-char " ")
(defcustom org-agenda-show-deadlines-fill-char "."
  "Fill character between entry and deadline. Use a space for no fill. \"_\" also works.")

(setq org-agenda-show-deadlines-date-format "%B %d, %Y")
(defcustom org-agenda-show-deadlines-date-format  "%Y-%m-%d"
  "Time format for deadlines. In the format of `format-time-string'."
  :type  'string)

(setq org-agenda-show-deadlines-change-function 'org-agenda-show-deadlines--trim)
(setq org-agenda-show-deadlines-change-function nil)
(defcustom org-agenda-show-deadlines-final-change-function nil
  "Function called on the deadline string before it is displayed. If `nil', then do nothing. The date string is the only argument")

(defun org-agenda-show-deadlines--trim (date-string)
  (substring date-string 1 -1))

(defun org-agenda-show-deadlines--insert-deadlines ()
  (interactive)
  (goto-char (point-min))
  (cl-loop with padding-char = (string-to-char org-agenda-show-deadlines-fill-char)
	   do (org-agenda-next-item 1)
	   ;;  `org-agenda-next-item' does not return `nil' at the last item
	   ;; so need to check it manually.
	   until (save-excursion (forward-line)
				 (eobp))
	   do (when-let* ((deadline (org-agenda-with-point-at-orig-entry nil (cdar (org-entry-properties (point) "DEADLINE"))))
			  (deadline (->> deadline
					 (ts-parse-org)
					 (ts-format org-agenda-show-deadlines-date-format)
					 ((lambda (d) (if (fboundp org-agenda-show-deadlines-change-function)
							  (funcall org-agenda-show-deadlines-change-function d)
							d)))))
			  (padding-string (if org-agenda-show-deadlines-next-line-p
					      (concat "\n" (make-string org-agenda-show-deadlines-column
									padding-char))
					    (make-string ((lambda (col) ;;  Of there's going to be a collision with agenda text,
							    (if (< col 0);; tell the user to fix it. 
								(signal 'user-error
									`(,(concat "The headings are too long. "
										   "Increase the value `org-agenda-show-deadlines-column' "
										   "to avoid collisions.")))
							      col))
							  (- org-agenda-show-deadlines-column (save-excursion (end-of-line)
													      (current-column))))
							 padding-char))))
		(end-of-line)
		(insert padding-string deadline))))

;;;###autoload
(define-minor-mode org-agenda-show-deadlines-mode
  "Show deadlines in org-agenda buffers"
  nil
  " OASD"
  nil
  (if org-agenda-show-deadlines-mode
      (add-hook 'org-agenda-finalize-hook 'org-agenda-show-deadlines--insert-deadlines t)
    (remove-hook 'org-agenda-finalize-hook 'org-agenda-show-deadlines--insert-deadlines)))

(provide 'org-agenda-show-deadlines)

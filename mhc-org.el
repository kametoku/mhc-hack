;;; mhc-org.el - org-mode and mhc integration

(defun org-mhc-record (subject date category)
  (let ((sc-date (if (string-match org-date-regexp date)
		     (concat (match-string 1 date)
			     (match-string 2 date)
			     (match-string 3 date))
		   date)))
    (with-temp-buffer
      (insert "X-SC-Subject: " subject "\n"
	      "X-SC-Day: " sc-date "\n"
	      "X-SC-Category: " category "\n")
      (goto-char (point-min))
      (mhc-parse-buffer))))

(defun org-mhc-task-to-mhc-record (task date)
  "Convert a org task line to MHC schedule record."
  (if (string-match (concat "^#?\\([A-C]\\)\\([0-9]*\\) +\\("
			    org-marks-regexp "\\) +\\(.+\\)")
		    task)
      (let* ((category (match-string 1 task))
	     (priority (match-string 2 task))
	     (status (match-string 3 task))
	     (description (match-string 4 task)))
	(org-mhc-record (concat category priority " " status " "
				    description)
			    date
			    (concat
			     (cond ((string= status "X") "TaskDone")
				   ((string= status "o") "TaskInProgress")
				   ((string= status ">") "TaskDelegated")
				   (t "Task"))
			     (if (string-match "^\\(private:\\|<private\\)"
					       description)
				 " Private"))))))

(defvar org-mhc-tasks "_o>"
  "*Task status which are listed in MHC schedule.
\"_oX>\" for all tasks.")

(defvar org-mhc-notes-as-schedule nil
  "*Non-nil means treat notes of org as schedule.")

(defun org-mhc-parse-file-for-tasks (filename task-type)
  (let ((tasks (org-extract-tasks task-type))
	records)
    (while tasks
      (setq records (cons (org-mhc-task-to-mhc-record
			   (car tasks)
			   (file-name-nondirectory filename))
			  records))
      (setq tasks (cdr tasks)))
    (nreverse records)))

(defun org-mhc-parse-file-for-notes (filename)
  (save-excursion
    (goto-char (point-min))
    (let ((date (file-name-nondirectory filename))
	  records)
      (while (re-search-forward "^\\.#\\([0-9]+\\)\\s-+\\(.+\\)" nil t)
	(let* ((num (match-string 1))
	       (subject (match-string 2))
	       (note (concat "#" num " " subject))
	       (categories (concat "Note"
				   (if (string-match "^private\\>" subject)
				       " Private"))))
	  (setq records (cons (org-mhc-record note date categories)
			      records))))
      (nreverse records))))

(defun org-mhc-parse-file (filename &optional task-type)
  (or task-type (setq task-type org-mhc-tasks))
  (with-temp-buffer
    (insert-file filename)
    (append (org-mhc-parse-file-for-tasks filename task-type)
	    (if org-mhc-notes-as-schedule
		(org-mhc-parse-file-for-notes filename)))))

(defadvice mhc-slot-get-month-schedule (after add-org-tasks (key) activate)
  (if (and (consp key) (car key) (cdr key))
      (let ((entries
	     (directory-files org-directory t
			      (concat "^"
				      (regexp-quote (format "%04d.%02d."
							    (car key)
							    (cdr key)))
				      "[0-9]+$")))
	    records filename slotinfo)
	(while entries
	  (setq filename (car entries))
	  (setq records (append (org-mhc-parse-file filename)
				records))
	  (setq entries (cdr entries)))
	(setq slotinfo (mhc-slot/new key
				     (mhc-slot-mtime ad-return-value)
				     (append (mhc-slot-records ad-return-value)
					     records)))
	(setq ad-return-value slotinfo))
    ad-return-value))
  
(defun org-mhc-toggle-notes (&optional hide-private)
  "Toggle notes as schedule."
  (interactive "P")
  (setq org-mhc-notes-as-schedule (not org-mhc-notes-as-schedule))
  (cond ((mhc-summary-buffer-p)
	 (mhc-rescan-month hide-private))
	((mhc-calendar-p)
	 (mhc-calendar-rescan))
	((eq major-mode 'mhc-weekly-mode)
	 (mhc-weekly-rescan))))

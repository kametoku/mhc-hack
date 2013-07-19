;;; mhc-monthly.el --- MHC monthly calendar

;; Author:  Tokuya Kameshima
;; Created: 2007-04-11

;;; $Id: mhc-monthly.el,v 1.1 2007/04/11 04:24:09 kame Exp $

;;; derived from |Id: mhc-weekly.el,v 1.8 2005/01/25 02:53:50 kame Exp |

(require 'mhc-calendar)
(require 'mhc-weekly)

(eval-when-compile
  (require 'cl))

;;; Configration Variables:

(defcustom mhc-monthly-start-day-of-week 1
  "*Day of the week as the start of the week in monthly mode."
  :group 'mhc
  :type '(choice (const :tag "Sunday" 0)
		 (const :tag "Monday" 1)
		 (const :tag "Tuesday" 2)
		 (const :tag "Wednesday" 3)
		 (const :tag "Thursday" 4)
		 (const :tag "Friday" 5)
		 (const :tag "Saturday" 6)))

;; (defcustom mhc-monthly-start-hour 8
;;   "*The hour when a day starts."
;;   :group 'mhc
;;   :type 'integer)

;; (defcustom mhc-monthly-end-hour 20
;;   "*The hour when a day ends."
;;   :group 'mhc
;;   :type 'integer)

;; (defcustom mhc-monthly-lines-par-hour 2
;;   "*Number of lines per hour (1-6)."
;;   :group 'mhc
;;   :type 'integer)

(defcustom mhc-monthly-lines-par-day 5
  "*Number of lines per day without the date line."
  :group 'mhc
  :type 'integer)

;; (defcustom mhc-monthly-task-lines 4
;;   "*Number of lines for tasks (schedule without time)."
;;   :group 'mhc
;;   :type 'integer)

;; (defcustom mhc-monthly-hour-width 3
;;   "*Width of the hour column without the vertical line '|'."
;;   :group 'mhc
;;   :type 'integer)

(defcustom mhc-monthly-day-widths '[9 11 11 11 11 11 9]
  "*Vector of column width of a day starting from Sunday."
  :group 'mhc
  :type '(vector integer integer integer integer integer integer integer))

(defcustom mhc-monthly-header-formats '("%a")
  "*List of header string format."
  :group 'mhc
  :type '(repeat (choice string
			 function)))

(defcustom mhc-monthly-day-label-formats '("%d")
  "*List of header string format."
  :group 'mhc
  :type '(repeat (choice string
			 function)))

;; (defcustom mhc-monthly-headline-categories '("Headline" "Holiday" "Vacation")
;;   "*List of headline categories."
;;   :group 'mhc
;;   :type '(repeat string))

(defcustom mhc-monthly-show-headline-in-tasks-too nil
  "*Non-nil means to show headline schedule in task columns."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-monthly-ignore-categories nil
  "*List of categories not shown in monthly view."
  :group 'mhc
  :type '(repeat string))

(defcustom mhc-monthly-column-separator "|"
  "*A string for column separator."
  :group 'mhc
  :type 'string)

(defcustom mhc-monthly-mode-hook nil
  "*Hook called in mhc-monthly-mode."
  :group 'mhc
  :type 'hook)

(defcustom mhc-monthly-create-buffer-hook nil
  "*Hook called in mhc-monthly-create-buffer."
  :group 'mhc
  :type 'hook)

(defcustom mhc-monthly-goto-view-hook nil
  "*Hook called in mhc-monthly-goto-view."
  :group 'mhc
  :type 'hook)

;; internal variables. Don't modify.
(defvar mhc-monthly/buffer "*mhc-monthly*")
(defvar mhc-monthly-date nil)
(defvar mhc-monthly-date-begin nil)	; new in mhc-monthly
(defvar mhc-monthly-date-end nil)	; new in mhc-monthly
(defvar mhc-monthly-view-date nil)
(defvar mhc-monthly-mode-map nil)
(defvar mhc-monthly-mode-menu-spec nil)


;;; Faces:

(defface mhc-monthly-face-day-label
  '((t (:background "Gray90")))
  "Face for the day labels.")

(mhc-weekly-make-faces 'mhc-monthly-face-day-label)

(defface mhc-monthly-face-day-label-other-month
  '((t (:foreground "Gray" :background "Gray90")))
  "Face for the day labels for other months.")

(mhc-weekly-make-faces 'mhc-monthly-face-day-label-other-month
		       (face-foreground 'default))

(defface mhc-monthly-face-time
  '((t (:foreground "purple")))
  "Face for the time string")

;; (defun mhc-monthly-make-faces (base-face &optional border-color)
;;   (let ((base-name (symbol-name base-face))
;; 	(border (or border-color t)))
;;     (mapc (lambda (arg)
;; 	    (let ((face (intern (concat base-name "-" (car arg))))
;; 		  (spec (cdr arg)))
;; 	      (copy-face base-face face)
;; 	      (apply 'set-face-attribute face nil spec)))
;; 	  `(("top" :overline ,border)
;; 	    ("bottom" :underline ,border)
;; 	    ("both" :overline ,border :underline ,border)))))

;; (defface mhc-monthly-face-table-1
;;   '((t (:background "Gray90")))
;;   "Face for table 1.")

;; (defface mhc-monthly-face-table-2
;;   nil
;;   "Face for table 2.")

;; (defvar mhc-monthly-table-base-faces
;;   '(mhc-monthly-face-table-1 mhc-monthly-face-table-2))

;; (mhc-monthly-make-faces 'mhc-monthly-face-table-1)
;; (mhc-monthly-make-faces 'mhc-monthly-face-table-2)

;; (defface mhc-monthly-face-schedule-1
;;   '((t (:background "LightBlue")))
;;   "Face for schedule 1.")

;; (defface mhc-monthly-face-schedule-2
;;   '((t (:background "Khaki")))
;;   "Face for schedule 2.")

;; (defvar mhc-monthly-schedule-base-faces
;;   '(mhc-monthly-face-schedule-1 mhc-monthly-face-schedule-2))

;; (mhc-monthly-make-faces 'mhc-monthly-face-schedule-1 "blue")
;; (mhc-monthly-make-faces 'mhc-monthly-face-schedule-2 "DarkGoldenRod")

;;; Code:

;; ;; utility
;; (defun mhc-monthly-overwrite-rectangle (rectangle)
;;   "Overwrite text of RECTANGLE with upper left corner at point."
;;   (let ((lines rectangle)
;; 	(insertcolumn (current-column))
;; 	(first t))
;; ;;     (push-mark)
;;     (while lines
;;       (or first
;; 	  (progn
;; 	   (forward-line 1)
;; 	   (or (bolp) (insert ?\n))
;; 	   (move-to-column-force insertcolumn)))
;;       (setq first nil)
;;       (if (looking-at "[^\r\n]")
;; 	  (let ((beg (point)))
;; 	    (move-to-column (+ insertcolumn
;; 			       (string-width (car lines))))
;; 	    (delete-region beg (point))))
;;       (insert (car lines))
;;       (setq lines (cdr lines)))))

;; (defun mhc-monthly-join-rectangles (&rest rectangles)
;;   (apply 'mapcar* 'concat rectangles))

;; (defun mhc-monthly/face (base-face height line border)
;;   ;; height: height of the cell
;;   ;; line: line >= 1
;;   ;; border: nil, 'top, 'bottom, or t (for top-bottom)
;;   (if (null base-face)
;;       nil
;;     (if (symbolp base-face)
;; 	(setq base-face (symbol-name base-face)))
;;     (or (memq border '(nil top bottom))
;; 	(setq border t))
;;     (let ((no-border-face (intern base-face))
;; 	  (top-border-face (intern (concat base-face "-top")))
;; 	  (bottom-border-face (intern (concat base-face "-bottom")))
;; 	  (both-border-face (intern (concat base-face "-both"))))
;;       (cond ((= height 1)
;; 	     (cond ((null border) no-border-face)
;; 		   ((eq border 'top) top-border-face)
;; 		   ((eq border 'bottom) bottom-border-face)
;; 		   (t both-border-face)))
;; 	    ((and (= line 1)
;; 		  (or (eq border 'top) (eq border t)))
;; 	     top-border-face)
;; 	    ((and (= line height)
;; 		  (or (eq border 'bottom) (eq border t)))
;; 	     bottom-border-face)
;; 	    (t no-border-face)))))

;; (defun mhc-monthly/cell-rectangle (width &optional height
;; 					text fold base-face text-prop
;; 					hborder vborder)
;;   ;; hborder: nil, 'top, 'bottom, or 'top-bottom
;;   ;; vborder: nil, 'left, 'right, or 'left-right
;;   (or height (setq height 1))
;;   (or text (setq text ""))
;;   (or fold (setq text (truncate-string-to-width text width)))
;;   (let* ((line-format (cond ((null vborder) "%s")
;; 			    ((eq vborder 'left)
;; 			     (concat mhc-monthly-column-separator "%s"))	; "|%s"
;; 			    ((eq vborder 'right)
;; 			     (concat "%s" mhc-monthly-column-separator))	; "%s|"
;; 			    (t
;; 			     (concat mhc-monthly-column-separator "%s"
;; 				     mhc-monthly-column-separator)))) ; "|%s|"
;; 	 (line 1)
;; 	 rectangle)
;;     (set-text-properties 0 (length line-format) text-prop line-format)
;;     (while (<= line height)
;;       (let* ((str (truncate-string-to-width text width))
;; 	     (label (concat str
;; 			    (make-string (- width (string-width str)) ?\x20)))
;; 	     (string (format line-format label))
;; 	     (face (mhc-monthly/face base-face height line hborder)))
;; 	(if face
;; 	    (add-text-properties 0 (length string) (list 'face face) string))
;; 	(setq rectangle (cons string rectangle))
;; 	(setq text (substring text (length str)))
;; 	(setq line (1+ line))))
;;     (nreverse rectangle)))

;; (defun mhc-monthly/center-string (string width)
;;   (or string (setq string ""))
;;   (let ((spaces (- width (string-width string)))
;; 	(padding ?\x20)
;; 	left right)
;;     (if (<= spaces 0)
;; 	string
;;       (setq left (/ spaces 2))
;;       (setq right (- spaces left))
;;       (concat (make-string left padding)
;; 	      string (make-string right padding)))))


;; (defun mhc-monthly/table-base-face (hour)
;;   (let ((index (% (- hour mhc-monthly-start-hour)
;; 		  (length mhc-monthly-table-base-faces))))
;;     (nth index mhc-monthly-table-base-faces)))

;; (defun mhc-monthly/schedule-base-face (nth)
;;   (let ((index (% nth
;; 		  (length mhc-monthly-schedule-base-faces))))
;;     (nth index mhc-monthly-schedule-base-faces)))


(defun mhc-monthly/table-label-string (day width format base-face hborder
					   &optional no-date-associated)
  (let* ((date (mhc-day-date day))
	 (ww (mhc-day-day-of-week day))
	 (label (mhc-weekly/center-string
		 (cond ((stringp format)
			(mhc-date-format-time-string format date))
		       ((functionp format)
			(funcall format day)))
		 width))
	 (prop (if (not no-date-associated)
		   (list 'mhc-weekly/date-prop date)))
	 (face (or base-face
;; 		   (get-text-property (/ (length label) 2) 'face label)
		   (cond ((= ww 0)
			  'mhc-calendar-face-sunday)
;; 			 ((mhc-day-holiday day)
;; 			  (mhc-face-category-to-face "Holiday"))
			 ((= ww 6)
			  'mhc-calendar-face-saturday)
			 (t 'mhc-calendar-face-default)))))
    (if base-face
	(progn
	  (if (mhc-date= (mhc-date-now) date)
	      (setq face (mhc-face-get-today-face face)))
	  (if (mhc-day-busy-p day)
	      (setq face (mhc-face-get-busy-face face)))))
    (let ((len (length label)))
      (if (text-properties-at (/ len 2) label)
	(progn
	  (remove-text-properties 0 len '(face nil) label)
	  (setq prop (append prop (text-properties-at (/ len 2) label))))))
    (apply 'concat
	   (append (unless (= ww mhc-monthly-start-day-of-week)
		     (mhc-weekly/cell-rectangle 0 1 "" nil base-face prop nil
						'left))
		   (mhc-weekly/cell-rectangle width 1 label nil face
					      prop hborder)))))

(defun mhc-monthly/rectangle-table-header (day width &optional show-hour)
  (mapcar (lambda (format)
	    (mhc-monthly/table-label-string day width format nil nil t))
	  mhc-monthly-header-formats))

(defun mhc-monthly/rectangle-table-day-label (day width &optional show-hour)
  (let* ((date (mhc-day-date day))
	 (face (if (= (mhc-date-mm date)
		      (mhc-date-mm mhc-monthly-date))
		   'mhc-monthly-face-day-label-top
		 'mhc-monthly-face-day-label-other-month-top)))
    (mapcar (lambda (format)
	      (mhc-monthly/table-label-string day width format face nil))
	    mhc-monthly-day-label-formats)))

;; (defun mhc-monthly/rectangle-table-body-row (day width hour
;; 						&optional show-hour format
;; 						text)
;;   (or format (setq format "%02d"))
;;   (let* ((base-face (mhc-monthly/table-base-face hour))
;; ;; 	 (hours-with-top-border (list mhc-monthly-start-hour 12 18
;; ;; 				      (+ mhc-monthly-end-hour 1)))
;; 	 (hours-with-top-border (list mhc-monthly-start-hour 12 18))
;; 	 (hborder (if (memq hour hours-with-top-border) 'top))
;; 	 (date-prop (list 'mhc-weekly/date-prop (mhc-day-date day)))
;; 	 (hour-cell (mhc-monthly/cell-rectangle mhc-monthly-hour-width
;; 					       mhc-monthly-lines-par-hour
;; 					       (format format hour)
;; 					       nil base-face nil hborder))
;; 	 (day-cell (mhc-monthly/cell-rectangle width
;; 					      mhc-monthly-lines-par-hour
;; 					      text
;; 					      nil base-face date-prop
;; 					      hborder 'left)))
;;     (if show-hour
;; 	(mhc-monthly-join-rectangles hour-cell day-cell)
;;       day-cell)))

;; (defun mhc-monthly-headline (day)
;;   (catch 'found
;;     (mapc (lambda (schedule)
;; 	    (if (mhc-schedule-in-category-p schedule
;; 					    mhc-monthly-headline-categories)
;; 		(throw 'found schedule)))
;; 	  (mhc-day-schedules day))
;;     nil))

;; ;; (defun mhc-monthly-headline-subject (day)
;; ;;   (mhc-schedule-subject (mhc-monthly-headline day)))

;; (defun mhc-monthly-headline-subject (day)
;;   (let ((headline (mhc-monthly-headline day)))
;;     (if headline
;; 	(let* ((subject (copy-sequence	; XXX need to copy?
;; 			 (mhc-schedule-subject-as-string headline)))
;; 	       (prop (list 'mhc-monthly/schedule-prop headline
;; 			   'help-echo subject
;; 			   'face (mhc-face-category-to-face
;; 				  (car (mhc-schedule-categories headline))))))
;; 	  (add-text-properties 0 (length subject) prop subject)
;; 	  subject))))

(defun mhc-monthly/rectangle-table-body-schedule (day width &optional show-hour)
  (let ((line 0)
	rectangle)
    (while (< line mhc-monthly-lines-par-day)
      (setq rectangle
	    (append rectangle
		    (list
		     (mhc-monthly/table-label-string day width "" nil nil))))
      (setq line (1+ line)))
    rectangle))

;; (defun mhc-monthly/rectangle-table-body-task (day width &optional show-hour)
;;   (let ((mhc-monthly-lines-par-hour mhc-monthly-task-lines))
;;     (mhc-monthly/rectangle-table-body-row day width (+ mhc-monthly-end-hour 1)
;; 					 show-hour "")))

(defun mhc-monthly/rectangle-blank-table (day width &optional show-header)
  (let ((show-hour nil))
    (append (if show-header
		(mhc-monthly/rectangle-table-header day width show-hour))
	    (mhc-monthly/rectangle-table-day-label day width show-hour)
	    (mhc-monthly/rectangle-table-body-schedule day width show-hour))))

(defun mhc-monthly/subject (schedule)
  (let ((subject (or (mhc-schedule-subject schedule) "<<no subject>>"))
	(location (mhc-schedule-location schedule))
	(face (mhc-face-category-to-face
	       (car (mhc-schedule-categories schedule)))))
    (setq subject
	  (cond ((and mhc-default-hide-private-schedules
		      (mhc-schedule-in-category-p schedule
						  mhc-category-as-private))
		 "[secret]")
		((and location
		      (not (string= location "")))
		 (concat subject " [" location "]"))
		(t
		 subject)))
    (set-text-properties 0 (length subject) (list 'face face) subject) ;; XXX
    subject))

(defvar mhc-monthly/schedule-count nil)

(defsubst mhc-monthly/time-to-string (minutes)
  (if (null minutes)
      ""
    (let ((time-string (format "%02d%02d"
			       (/ minutes 60) (% minutes 60))))
      (set-text-properties 0 (length time-string) '(face mhc-monthly-face-time)
			   time-string)
      time-string)))

(defun mhc-monthly/insert-schedule (schedule width) ;; schedule-begin)
;;   (goto-char schedule-begin)
  (let* ((column (current-column))
	 (time-begin (mhc-schedule-time-begin schedule))
	 (time-end (mhc-schedule-time-end schedule))
	 (time-string (mhc-schedule-time-as-string schedule))
	 (time-string-short
	  (mhc-monthly/time-to-string (or time-begin time-end)))
	 (subject (mhc-monthly/subject schedule))
	 (text (if (zerop (length time-string-short))
		   subject
		 (format "%s %s" time-string-short subject)))
;; 	 (base-face (mhc-monthly/schedule-base-face mhc-monthly/schedule-count))
	 (base-face nil)
	 (prop (list 'mhc-monthly/schedule-prop schedule
		     'mhc-weekly/date-prop date
		     'help-echo (concat time-string
					(if (> (length time-string) 0)
					    " ")
					subject))))
    (mhc-weekly-overwrite-rectangle
     (mhc-weekly/cell-rectangle width 1 text t base-face prop)))
  (setq mhc-monthly/schedule-count (1+ mhc-monthly/schedule-count)))

(defun mhc-monthly/insert-schedules (schedules width)
  (let ((column (current-column))
	(mhc-monthly/schedule-count 0))
    (while (and schedules
		(> mhc-monthly/rest-lines 0))
      (let ((schedule (car schedules)))
	(if (not (mhc-schedule-in-category-p schedule
					     mhc-monthly-ignore-categories))
	    (progn
	      (mhc-monthly/insert-schedule schedule width)
	      (forward-line)
	      (move-to-column column)
	      (setq mhc-monthly/rest-lines (1- mhc-monthly/rest-lines)))))
      (setq schedules (cdr schedules)))))

;; (defun mhc-monthly/insert-tasks (schedules width)
;;   (let (;;(hour (+ mhc-monthly-end-hour 1))
;; ;;  	(headline (mhc-weekly-headline day)) ;; XXX: day...
;; 	rectangle)
;;     (while (and schedules
;; 		(> mhc-monthly/rest-lines 0))
;;       (if (and (not (mhc-schedule-in-category-p (car schedules)
;; 						mhc-monthly-ignore-categories))
;; 	       (not (or (mhc-schedule-time-begin (car schedules))
;; 			(mhc-schedule-time-end (car schedules))
;; ;; 			(and (not mhc-monthly-show-headline-in-tasks-too)
;; ;; 			     (eq headline (car schedules)))
;; 			)))
;; 	  (let* ((subject (mhc-monthly/subject (car schedules)))
;; ;; 		 (base-face (mhc-monthly/table-base-face hour))
;; 		 (base-face (get-text-property 0 'face subject)) ;; XXX
;; 		 (prop (list 'mhc-monthly/schedule-prop (car schedules)
;; 			     'mhc-weekly/date-prop date	;; FIXME date!!
;; 			     'help-echo subject)))
;; 	    (setq rectangle
;; 		  (append
;; 		   rectangle
;; 		   (mhc-weekly/cell-rectangle width 1
;; 					      subject t base-face
;; 					      prop)))
;; 	    (setq mhc-monthly/rest-lines (1- mhc-monthly/rest-lines))))
;;       (setq schedules (cdr schedules)))
;;     (mhc-weekly-overwrite-rectangle rectangle)))

(defun mhc-monthly/day-summary (schedules)
  (let ((summary ""))
    (while schedules
      (let ((schedule (car schedules)))
	(if (not (mhc-schedule-in-category-p schedule
					     mhc-monthly-ignore-categories))
	    (let* ((time-string (mhc-schedule-time-as-string schedule))
		   (subject (mhc-monthly/subject schedule))
		   (text (concat time-string
				 (if (> (length time-string) 0) " ")
				 subject)))
	      (setq summary (concat summary
				    (if (> (length summary) 0) "\n")
				    text))))
	(setq schedules (cdr schedules))))
;;     (if (> (length summary) 0)
;; 	(concat summary "\n")
;;       summary)))
    summary))

(defvar mhc-monthly/rest-lines)

;; (defun mhc-monthly/insert-time-table (day &optional show-hour)
(defun mhc-monthly/insert-time-table (day &optional show-header)
  (let* ((date (mhc-day-date day))	;; used in insert-tasks
	 (width (aref mhc-monthly-day-widths (mhc-day-day-of-week day)))
	 (schedule-column (current-column))
	 (schedules (mhc-day-schedules day))
	 (summary (mhc-monthly/day-summary schedules))
	 (prop (list 'help-echo summary))
	 (beg (point))
	 (mhc-monthly/rest-lines mhc-monthly-lines-par-day))
    (if (not (= (mhc-date-ww date) mhc-monthly-start-day-of-week))
	(setq schedule-column
	      (+ schedule-column
		 (string-width mhc-monthly-column-separator))))
    (insert-rectangle (mhc-monthly/rectangle-blank-table day width show-header))
    (save-excursion
      (goto-char beg)
      (if show-header
	  (progn
	    (forward-line (length mhc-monthly-header-formats))
	    (move-to-column schedule-column)))
      (let* ((lines (length mhc-monthly-day-label-formats)))
	(if (> (length summary) 0)
	    (while (> lines 0)
	      (add-text-properties (point) (+ (point) width) prop)
	      (forward-line)
	      (move-to-column schedule-column)
	      (setq lines (1- lines)))
	  (forward-line (length mhc-monthly-day-label-formats))
	  (move-to-column schedule-column)))
      (mhc-monthly/insert-schedules schedules width)
      (if (> (length (split-string summary "\n")) mhc-monthly-lines-par-day)
	  (let ((text "<<more>>"))
;; 	    (forward-line -1)
	    (move-to-column schedule-column)
;; 	    (set-text-properties 0 (length text) prop text)
	    (mhc-weekly-overwrite-rectangle
	     (mhc-weekly/cell-rectangle width 1 text t
					font-lock-warning-face prop))))
;;       (mhc-monthly/insert-tasks (mhc-day-schedules day) width))))
      )))

(defun mhc-monthly/number-of-tasks (day)
  (let ((schedules (mhc-day-schedules day))
	(headline (mhc-monthly-headline day))
	(num 0))
    (while schedules
      (if (or (mhc-schedule-time-begin (car schedules))
	      (mhc-schedule-time-end (car schedules)))
	  nil
	(if (and (not mhc-monthly-show-headline-in-tasks-too)
		 (eq headline (car schedules)))
	    nil
	  (setq num (+ num 1))))
      (setq schedules (cdr schedules)))
    num))

(defun mhc-monthly-make-header (date)
  (let ((header (funcall mhc-calendar-header-function date)))
    (if (string-match "^ *\\(.*\\) *$" header)
	(match-string 1 header)
      header)))

(defun mhc-monthly-insert-table (from to date)
  (let* ((days (mhc-db-scan from to))
	 (count 0))
    (setq header-line-format
	  (concat "  " (mhc-monthly-make-header date)))
    (while days
      (save-excursion
	(let ((first (< count 7)))
	  (mhc-monthly/insert-time-table (car days) first)))
      (if (= (% count 7) 6)
	  (progn
	    (goto-char (point-max))
	    (insert "\n")))
      (end-of-line)
      (setq count (1+ count))
      (setq days (cdr days)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new in mhc-monthly but could be shared
(defun mhc-monthly/start-day-of-week (date)
  (let ((ww (mhc-date-ww date)))
    (- date
       (- ww mhc-monthly-start-day-of-week)
       (if (< ww mhc-monthly-start-day-of-week)
	   7 0))))

(defun mhc-monthly/create-buffer (date)
  (set-buffer (get-buffer-create mhc-monthly/buffer))
  (setq buffer-read-only t)
  (unless (eq major-mode 'mhc-monthly-mode)
    (mhc-monthly-mode)
    (buffer-disable-undo))
  (or (mhc-date-p date) (setq date (mhc-date-now)))
  (let* ((buffer-read-only nil))
    (erase-buffer)
    (message "mhc-monthly create...")
    (setq mhc-monthly-date date)
    (setq mhc-monthly-date-begin
	  (mhc-monthly/start-day-of-week (mhc-date-mm-first date)))
    (setq mhc-monthly-date-end
	  (mhc-date+ (mhc-monthly/start-day-of-week (mhc-date-mm-last date))
		     6))
    (mhc-monthly-insert-table mhc-monthly-date-begin mhc-monthly-date-end
			      mhc-monthly-date)
    (run-hooks 'mhc-monthly-create-buffer-hook)
    (set-buffer-modified-p nil)
    (message "mhc-monthly create...done")))

(unless mhc-monthly-mode-map
  (setq mhc-monthly-mode-map (make-sparse-keymap))
  (define-key mhc-monthly-mode-map "."    'mhc-monthly-goto-today)
  (define-key mhc-monthly-mode-map "g"    'mhc-monthly-goto-day)
  (define-key mhc-monthly-mode-map "r"    'mhc-monthly-rescan)
  (define-key mhc-monthly-mode-map "R"    'mhc-monthly-reset)
  (define-key mhc-monthly-mode-map "s"    'mhc-monthly-scan)
  (define-key mhc-monthly-mode-map "E"    'mhc-monthly-edit)
  (define-key mhc-monthly-mode-map "M"    'mhc-monthly-modify)
  (define-key mhc-monthly-mode-map "D"    'mhc-monthly-delete)
  (define-key mhc-monthly-mode-map "v"    'mhc-monthly-goto-view)
  (define-key mhc-monthly-mode-map "n"    'mhc-monthly-next-week)
  (define-key mhc-monthly-mode-map "p"    'mhc-monthly-prev-week)
  (define-key mhc-monthly-mode-map "N"    'mhc-monthly-next-line)
  (define-key mhc-monthly-mode-map "P"    'mhc-monthly-prev-line)
  (define-key mhc-monthly-mode-map "j"    'mhc-monthly-next-line)
  (define-key mhc-monthly-mode-map "k"    'mhc-monthly-prev-line)
  (define-key mhc-monthly-mode-map "f"    'mhc-monthly-next-day)
  (define-key mhc-monthly-mode-map "b"    'mhc-monthly-prev-day)
  (define-key mhc-monthly-mode-map ">"    'mhc-monthly-next-month)
  (define-key mhc-monthly-mode-map "<"    'mhc-monthly-prev-month)
  (define-key mhc-monthly-mode-map "\M-}" 'mhc-monthly-next-month)
  (define-key mhc-monthly-mode-map "\M-{" 'mhc-monthly-prev-month)
  (define-key mhc-monthly-mode-map "\M-\C-n" 'mhc-monthly-next-year)
  (define-key mhc-monthly-mode-map "\M-\C-p" 'mhc-monthly-prev-year)
  (define-key mhc-monthly-mode-map "\M-<" 'mhc-monthly-beginning-of-month)
  (define-key mhc-monthly-mode-map "\M->" 'mhc-monthly-end-of-month)
  (define-key mhc-monthly-mode-map "\C-a" 'mhc-monthly-beginning-of-week)
  (define-key mhc-monthly-mode-map "\C-e" 'mhc-monthly-end-of-week)
  (define-key mhc-monthly-mode-map "^"    'mhc-monthly-enlarge-table)
  (define-key mhc-monthly-mode-map "q"    'mhc-monthly-quit)
  (define-key mhc-monthly-mode-map "Q"    'mhc-monthly-exit)
  (define-key mhc-monthly-mode-map "\C-c?" 'mhc-calendar)
  (define-key mhc-monthly-mode-map "?"    'mhc-monthly-describe-schedule))

(defun mhc-monthly-mode ()
  "MHC Monthly mode:: major mode to view monthly calendar."
  (interactive)
  (kill-all-local-variables)
  (use-local-map mhc-monthly-mode-map)
  (make-local-variable 'mhc-monthly-date)
  (make-local-variable 'mhc-monthly-date-begin)
  (make-local-variable 'mhc-monthly-date-end)
  (make-local-variable 'mhc-monthly-view-date)
  (make-local-variable 'indent-tabs-mode)
  (setq major-mode 'mhc-monthly-mode)
  (setq mode-name "mhc-monthly")
  (setq indent-tabs-mode nil)
  (setq truncate-lines t)
;;   (when (featurep 'xemacs)
;;     (easy-menu-add mhc-monthly-mode-menu))
  (run-hooks 'mhc-monthly-mode-hook))

(defun mhc-monthly (&optional date)
  "Display monthly schedule."
  (interactive)
  (setq date (or date
		 (mhc-current-date)
		 (mhc-calendar-get-date) ;; (mhc-monthly-get-date)))
		 (mhc-date-now)))
  (mhc-monthly/create-buffer date)
  (mhc-monthly/goto-date date)
;;   (if (mhc-date= date (mhc-date-now))
;;       (mhc-monthly-goto-time)))
  (if (mhc-date= date (mhc-date-now))
      (mhc-monthly-describe-schedule)))

(defun mhc-monthly-get-date ()
  (if (get-buffer mhc-monthly/buffer)
      (let (date pos)
	(set-buffer (get-buffer mhc-monthly/buffer))
	(setq pos (point))
	(while (and (null date)
		    (> pos 0))
	  (setq date (get-text-property pos 'mhc-weekly/date-prop))
	  (setq pos (1- pos)))
	date)
    (mhc-date-now)))

(defun mhc-monthly/count-lines ()
  (let* ((date (mhc-monthly-get-date))
	 (bol (save-excursion
		(beginning-of-line)
		(point)))
	 (pos (text-property-any (point-min) (point)
				 'mhc-weekly/date-prop date)))
    (if pos
	(count-lines (save-excursion
		       (goto-char pos)
		       (beginning-of-line)
		       (point))
		     bol)
      0)))

(defun mhc-monthly/goto-date (date &optional force-update)
  (unless (get-buffer mhc-monthly/buffer)
    (mhc-monthly/create-buffer date))
  (set-buffer (get-buffer mhc-monthly/buffer))
  (switch-to-buffer mhc-monthly/buffer)
  (let ((lines (mhc-monthly/count-lines)))
    (if (or force-update
	    (< date mhc-monthly-date-begin)
	    (> date mhc-monthly-date-end))
	(mhc-monthly/create-buffer date))
    (goto-char (or (text-property-any (point-min) (point-max)
				      'mhc-weekly/date-prop date)
		   (point)))
    (mhc-monthly-next-line lines date)))

(defun mhc-monthly-goto-today (&optional arg)
  (interactive "p")
  (goto-char (point-min))
  (mhc-monthly/goto-date (mhc-date-now))
;;   (mhc-monthly-goto-time))
  (mhc-monthly-describe-schedule))

(defun mhc-monthly-next-day (&optional arg)
  (interactive "p")
  (mhc-monthly/goto-date (+ (mhc-monthly-get-date) arg)))

(defun mhc-monthly-prev-day (&optional arg)
  (interactive "p")
  (mhc-monthly-next-day (- arg)))

(defun mhc-monthly-next-week (&optional arg)
  (interactive "p")
  (let ((date (mhc-monthly-get-date)))
    (goto-char (point-min))
    (mhc-monthly/goto-date (+ date (* 7 arg)))))

(defun mhc-monthly-prev-week (&optional arg)
  (interactive "p")
  (mhc-monthly-next-week (- arg)))

(defun mhc-monthly-next-month (&optional arg)
  (interactive "p")
;;   (mhc-monthly/goto-date (mhc-date-mm+ (mhc-monthly-get-date) arg)))
  (let ((date (mhc-monthly-get-date)))
    (unless (mhc-date-yymm= date mhc-monthly-date)
      (setq date (mhc-date-mm-first mhc-monthly-date)))
    (mhc-monthly/goto-date (mhc-date-mm+ date arg) t)))

(defun mhc-monthly-prev-month (&optional arg)
  (interactive "p")
  (mhc-monthly-next-month (- arg)))

(defun mhc-monthly-next-year (&optional arg)
  (interactive "p")
;;   (mhc-monthly/goto-date (mhc-date-yy+ (mhc-monthly-get-date) arg)))
  (let ((date (mhc-monthly-get-date)))
    (unless (mhc-date-yymm= date mhc-monthly-date)
      (setq date (mhc-date-mm-first mhc-monthly-date)))
    (mhc-monthly/goto-date (mhc-date-yy+ date arg) t)))

(defun mhc-monthly-prev-year (&optional arg)
  (interactive "p")
  (mhc-monthly-next-year (- arg)))

(defun mhc-monthly-next-line (&optional arg date)
  (interactive "p")
  (or date (setq date (mhc-monthly-get-date)))
  (let ((pos (point))
	eol)
    (forward-line arg)
    (beginning-of-line)
    (setq eol (save-excursion
		(end-of-line)
		(point)))
    (goto-char (or (text-property-any (point) eol 'mhc-weekly/date-prop date)
		   pos))
    ;;; FIXME
    (if (looking-at (regexp-quote mhc-monthly-column-separator))
	(goto-char (match-end 0)))
    (mhc-monthly-describe-schedule)))

(defun mhc-monthly-prev-line (&optional arg)
  (interactive "p")
  (mhc-monthly-next-line (- arg)))

;; (defun mhc-monthly-goto-time (&optional time)
;;   (or time (setq time (mhc-time-now)))
;;   (let* ((date (mhc-monthly-get-date))
;; 	 (hour (mhc-time-HH time))
;; 	 (min (mhc-time-MM time))
;; 	 (hour-regexp (concat "^" (regexp-quote (format "%02d" hour))))
;; 	 (pos (point)))
;;     (cond ((< hour mhc-monthly-start-hour)
;; 	   (mhc-monthly-beginning-of-day))
;; 	  ((> hour mhc-monthly-end-hour)
;; 	   (mhc-monthly-end-of-day))
;; 	  (t
;; 	   (goto-char (point-min))
;; 	   (if (re-search-forward hour-regexp nil t)
;; 	       (progn
;; 		 (mhc-monthly/goto-date date)
;; 		 (mhc-monthly-next-line (/ (* min mhc-monthly-lines-par-hour)
;; 					  60)))
;; 	     (goto-char pos))))
;;     (mhc-monthly-describe-schedule)))

;; (defun mhc-monthly-goto-task ()
;;   "Go to the beginning of the task line on the current day."
;;   (interactive)
;;   (mhc-monthly-goto-time (mhc-time-new mhc-monthly-end-hour 0))
;;   (mhc-monthly-next-line mhc-monthly-lines-par-hour))

(defun mhc-monthly-next-schedule (&optional arg)
  (interactive "p")
  (let ((direction (signum arg))
	(prev-prop (get-text-property (point) 'mhc-monthly/schedule-prop))
	prop found)
    (setq arg (abs arg))
    (while (> arg 0)
      (setq found nil)
      (while (and (not found)
		  (let ((pos (point)))
		    (mhc-monthly-next-line direction)
		    (not (= (point) pos))))
	(if (and (setq prop
		       (get-text-property (point) 'mhc-monthly/schedule-prop))
		 (not (eq prop prev-prop)))
	    (setq found t))
	(setq prev-prop prop))
      (setq arg (- arg 1)))))

(defun mhc-monthly-prev-schedule (&optional arg)
  (interactive "p")
  (mhc-monthly-next-schedule (- arg)))

(defun mhc-monthly-goto-day (&optional date)
  (interactive)
  (let ((default-date (get-text-property (point) 'mhc-weekly/date-prop)))
    (mhc-monthly/goto-date (if (integerp date)
			       date
			     (car (mhc-input-day "Date: " default-date))))))

(defun mhc-monthly-beginning-of-day ()
  (interactive)
  (let ((date (mhc-monthly-get-date)))
    (goto-char (point-min))
    (forward-line (length mhc-monthly-header-formats))
    (mhc-monthly-goto-day date)))

(defun mhc-monthly-end-of-day ()
  (interactive)
  (let ((date (mhc-monthly-get-date)))
    (goto-char (point-max))
    (mhc-monthly-goto-day date)))

(defun mhc-monthly-beginning-of-week ()
  (interactive)
  (let* ((ww (mhc-date-ww (mhc-monthly-get-date)))
	 (date (mhc-date- (mhc-monthly-get-date)
			  (mod (+ 7 (- ww mhc-monthly-start-day-of-week))
			       7))))
    (mhc-monthly-goto-day date)))

(defun mhc-monthly-end-of-week ()
  (interactive)
  (let* ((ww (mhc-date-ww (mhc-monthly-get-date)))
	 (date (mhc-date- (mhc-monthly-get-date)
			  (- (mod (+ 7 (- ww mhc-monthly-start-day-of-week))
				  7)
			     6))))
    (mhc-monthly-goto-day date)))

(defun mhc-monthly-beginning-of-month ()
  (interactive)
  (mhc-monthly-goto-day (mhc-date-mm-first mhc-monthly-date)))

(defun mhc-monthly-end-of-month ()
  (interactive)
  (mhc-monthly-goto-day (mhc-date-mm-last mhc-monthly-date)))

(defun mhc-monthly-enlarge-table (&optional arg)
  "Increase the lines per day."
  (interactive "p")
  (setq mhc-monthly-lines-par-day (+ mhc-monthly-lines-par-day arg))
  (if (< mhc-monthly-lines-par-day 1)
      (setq mhc-monthly-lines-par-day 1))
  (if (> mhc-monthly-lines-par-day 6)
      (setq mhc-monthly-lines-par-day 6))
  (mhc-monthly-rescan)) ;; FIXME: cursor position is not preserved properly.

(defun mhc-monthly-rescan ()
  (interactive)
  (if (get-buffer mhc-monthly/buffer)
      (let (date top)
	(set-buffer mhc-monthly/buffer)
	(setq date (mhc-monthly-get-date))
	(setq top (save-excursion
		    (move-to-window-line 0)
		    (point)))
	(mhc-monthly/create-buffer mhc-monthly-date)
;;     (mhc-monthly/goto-date pdate)))
	(goto-char top)
	(recenter 0)
	(mhc-monthly/goto-date date))))

(defun mhc-monthly-reset ()
  (interactive)
  (mhc-reset)
  (mhc-monthly-rescan))

(defun mhc-monthly-scan (&optional hide-private)
  (interactive "P")
  (let ((date (mhc-monthly-get-date)))
    (mhc-monthly-quit)
    (mhc-goto-month date hide-private)
    (goto-char (point-min))
    (if (mhc-summary-search-date date)
	(progn
	  (beginning-of-line)
	  (if (not (pos-visible-in-window-p (point)))
	      (recenter))))))

(defun mhc-monthly-quit ()
  (interactive)
  (let ((win (get-buffer-window mhc-monthly/buffer))
	(buf (get-buffer mhc-monthly/buffer)))
;;     (save-excursion
;;       (set-buffer buf)
;;       (mhc-monthly/delete-overlay))
    (when win
      (bury-buffer buf)
      (if (null (one-window-p))
	  (delete-windows-on buf)
	(set-window-buffer win (other-buffer))
	(select-window (next-window))))))

(defun mhc-monthly-describe-schedule ()
  "Describe the schedule on cursor position."
  (interactive)
;;   (let ((schedule (get-text-property (point) 'mhc-monthly/schedule-prop)))
  (let ((date (or (get-text-property (point) 'mhc-weekly/date-prop)
		  (get-text-property (point) 'mhc-weekly/date-prop)))
	(help-echo (get-text-property (point) 'help-echo)))
    (if help-echo
	(message (if (string-match "\n." help-echo)
		     "*** %s ***\n%s"
		   "%s %s")
		 (mhc-date-format-time-string "%m/%d(%a)" date)
		 help-echo)
      (if (interactive-p)
	  (message "No schedule on cursor.")
	(message "")))))

(defun mch-monthly/schedule
  (get-text-property (point) 'mhc-monthly/schedule-prop))

(defadvice mhc-current-date (after recognize-mhc-monthly-buffer activate)
  (if (and (not ad-return-value)
	   (eq major-mode 'mhc-monthly-mode))
      (setq ad-return-value (mhc-monthly-get-date))))

;; for `mhc-monthly-edit' and `mhc-monthly-modify'.
(add-hook 'mhc-draft-finish-hook
	  '(lambda ()
	     (if (eq major-mode 'mhc-monthly-mode)
		 (mhc-monthly-rescan))))

(defun mhc-monthly-edit ()
  (interactive)
  (mhc-window-push)
  (mhc-edit nil)
  (delete-other-windows))

(defun mhc-monthly-delete ()
  (interactive)
  (let* ((schedule (get-text-property (point) 'mhc-monthly/schedule-prop))
	 (filename (mhc-record-name (mhc-schedule-record schedule))))
    (if (null filename)
	(message "Nothing to do in this point.")
      (setq key (mhc-slot-directory-to-key
		 (directory-file-name (file-name-directory filename))))
      (mhc-delete-file
       (assoc filename
	      (mhc-slot-records (mhc-slot-get-month-schedule key))))
      (let ((pos (point)))
	(mhc-monthly-rescan)
	(goto-char pos)))))

(defun mhc-monthly-modify ()
  (interactive)
  (let* ((schedule (get-text-property (point) 'mhc-monthly/schedule-prop))
	 (filename (mhc-record-name (mhc-schedule-record schedule))))
    (if filename
	(mhc-modify-file filename)
      (message "Nothing to do in this point."))))

(defun mhc-monthly-goto-view ()
  (interactive)
  (let* ((schedule (get-text-property (point) 'mhc-monthly/schedule-prop))
	 (filename (mhc-record-name (mhc-schedule-record schedule))))
    (if filename
	(progn
	  (view-file-other-window filename)
	  (mhc-calendar/view-file-decode-header)
	  (set-visited-file-name nil)
	  (set-buffer-modified-p nil)
	  (setq buffer-read-only t)
;; 	  (re-search-forward "^$" nil t))
	  (run-hooks 'mhc-monthly-goto-view-hook)
	  )
      (message "Nothing to do in this point."))))

(provide 'mhc-monthly)

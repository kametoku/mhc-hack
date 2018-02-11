;;; mhc-weekly.el --- MHC weekly calendar

;; Author:  Tokuya Kameshima
;; Created: 2003-11-11

;;; $Id: mhc-weekly.el,v 1.8 2005/01/25 02:53:50 kame Exp $

;; TODO: Update layout on window resize:
;; (add-hook 'window-size-change-functions 'foo)
;; (defun foo (frame) ...)

(require 'mhc-calendar)
(eval-when-compile
  (require 'cl))

;; for new MHC.
(if (fboundp 'mhc-summary-current-date)
    (defalias 'mhc-current-date 'mhc-summary-current-date))

;;; Configration Variables:

(defcustom mhc-weekly-start-day-of-week 1
  "*Day of the week as the start of the week in weekly mode."
  :group 'mhc
  :type '(choice (const :tag "Sunday" 0)
		 (const :tag "Monday" 1)
		 (const :tag "Tuesday" 2)
		 (const :tag "Wednesday" 3)
		 (const :tag "Thursday" 4)
		 (const :tag "Friday" 5)
		 (const :tag "Saturday" 6)))

(defcustom mhc-weekly-start-hour 8
  "*The hour when a day starts."
  :group 'mhc
  :type 'integer)

(defcustom mhc-weekly-end-hour 20
  "*The hour when a day ends."
  :group 'mhc
  :type 'integer)

(defcustom mhc-weekly-lines-par-hour 2
  "*Number of lines per hour (1-6)."
  :group 'mhc
  :type 'integer)

(defcustom mhc-weekly-task-lines 4
  "*Number of lines for tasks (schedule without time)."
  :group 'mhc
  :type 'integer)

(defcustom mhc-weekly-hour-width 3
  "*Width of the hour column without the vertical line '|'."
  :group 'mhc
  :type 'integer)

(defcustom mhc-weekly-day-widths '[7 11 11 11 11 11 7]
  "*Vector of column width of a day starting from Sunday."
  :group 'mhc
  :type '(choice (vector :tag "Fixed widths"
			 (integer :tag "Sunday")
			 (integer :tag "Monday")
			 (integer :tag "Tuesday")
			 (integer :tag "Wedesday")
			 (integer :tag "Thursday")
			 (integer :tag "Friday")
			 (integer :tag "Saturday"))
		 (const :tag "Fit to the window size" fit-to-window)))

(defcustom mhc-weekly-day-widths-fit-to-window 80
  "*Non-nil means to fit the column widths to the window width.
A value of integer means the minimum window width in which fitting to
the window occurs."
  :group 'mhc
  :type '(choice (const :tag "Always" t)
		 (const :tag "No" nil)
		 (integer :tag "Minimum window width")))

(defcustom mhc-weekly-header-formats '("%m/%d" "%a"
				       mhc-weekly-headline-subject)
  "*List of header string format."
  :group 'mhc
  :type '(repeat (choice string
			 function)))

(defcustom mhc-weekly-headline-categories '("Headline" "Holiday" "Vacation")
  "*List of headline categories."
  :group 'mhc
  :type '(repeat string))

(defcustom mhc-weekly-show-headline-in-tasks-too nil
  "*Non-nil means to show headline schedule in task columns."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-weekly-column-separator "|"
  "*A string for column separator."
  :group 'mhc
  :type 'string)

(defcustom mhc-weekly-mode-hook nil
  "*Hook called in mhc-weekly-mode."
  :group 'mhc
  :type 'hook)

(defcustom mhc-weekly-create-buffer-hook nil
  "*Hook called in mhc-weekly-create-buffer."
  :group 'mhc
  :type 'hook)

(defcustom mhc-weekly-goto-view-hook nil
  "*Hook called in mhc-weekly-goto-view."
  :group 'mhc
  :type 'hook)

;; internal variables. Don't modify.
(defvar mhc-weekly/buffer "*mhc-weekly*")
(defvar mhc-weekly-date nil)
(defvar mhc-weekly-view-date nil)
(defvar mhc-weekly-mode-map nil)
(defvar mhc-weekly-mode-menu-spec nil)
(defvar mhc-weekly-day-widths-cache nil)

;;; Faces:

(defun mhc-weekly-copy-face (old-face new-face)
  (make-face new-face)
;;   (let* ((face-attr-alist (face-all-attributes old-face (selected-frame)))
;; 	 (spec (apply 'append
;; 		      (mapcar '(lambda (elem)
;; 				 (list (car elem) (cdr elem)))
;; 			      face-attr-alist))))
;;     (apply 'set-face-attribute new-face nil spec)))
  (let* ((spec (face-attr-construct old-face)))
    (apply 'set-face-attribute new-face nil spec)))

(defun mhc-weekly-make-faces (base-face &optional border-color)
  (let ((base-name (symbol-name base-face))
	(border (or border-color t)))
    (mapc (lambda (arg)
	    (let ((face (intern (concat base-name "-" (car arg))))
		  (spec (cdr arg)))
;; 	      (copy-face base-face face)
;; 	      (apply 'set-face-attribute face nil spec)))
	      (mhc-weekly-copy-face base-face face)
 	      (apply 'set-face-attribute face nil spec)))
	  `(("top" :overline ,border)
	    ("bottom" :underline ,border)
	    ("both" :overline ,border :underline ,border)))))

(defface mhc-weekly-face-table-1
  '((t (:background "Gray90")))
  "Face for table 1.")

(defface mhc-weekly-face-table-2
  nil
  "Face for table 2.")

(defvar mhc-weekly-table-base-faces
  '(mhc-weekly-face-table-1 mhc-weekly-face-table-2))

(mhc-weekly-make-faces 'mhc-weekly-face-table-1)
(mhc-weekly-make-faces 'mhc-weekly-face-table-2)

(defface mhc-weekly-face-schedule-1
  '((t (:background "LightBlue")))
  "Face for schedule 1.")

(defface mhc-weekly-face-schedule-2
  '((t (:background "Khaki")))
  "Face for schedule 2.")

(defvar mhc-weekly-schedule-base-faces
  '(mhc-weekly-face-schedule-1 mhc-weekly-face-schedule-2))

(mhc-weekly-make-faces 'mhc-weekly-face-schedule-1 "blue")
(mhc-weekly-make-faces 'mhc-weekly-face-schedule-2 "DarkGoldenRod")

;;; Code:

;; utility
(defun mhc-weekly-overwrite-rectangle (rectangle)
  "Overwrite text of RECTANGLE with upper left corner at point."
  (let ((lines rectangle)
	(insertcolumn (current-column))
	(first t))
;;     (push-mark)
    (while lines
      (or first
	  (progn
	   (forward-line 1)
	   (or (bolp) (insert ?\n))
	   (move-to-column insertcolumn t)))
      (setq first nil)
      (if (looking-at "[^\r\n]")
	  (let ((beg (point)))
	    (move-to-column (+ insertcolumn
			       (string-width (car lines))))
	    (delete-region beg (point))))
      (insert (car lines))
      (setq lines (cdr lines)))))

(defun mhc-weekly-join-rectangles (&rest rectangles)
  (apply 'mapcar* 'concat rectangles))

(defun mhc-weekly/face (base-face height line border)
  ;; height: height of the cell
  ;; line: line >= 1
  ;; border: nil, 'top, 'bottom, or t (for top-bottom)
  (if (null base-face)
      nil
    (if (symbolp base-face)
	(setq base-face (symbol-name base-face)))
    (or (memq border '(nil top bottom))
	(setq border t))
    (let ((no-border-face (intern base-face))
	  (top-border-face (intern (concat base-face "-top")))
	  (bottom-border-face (intern (concat base-face "-bottom")))
	  (both-border-face (intern (concat base-face "-both"))))
      (cond ((= height 1)
	     (cond ((null border) no-border-face)
		   ((eq border 'top) top-border-face)
		   ((eq border 'bottom) bottom-border-face)
		   (t both-border-face)))
	    ((and (= line 1)
		  (or (eq border 'top) (eq border t)))
	     top-border-face)
	    ((and (= line height)
		  (or (eq border 'bottom) (eq border t)))
	     bottom-border-face)
	    (t no-border-face)))))

(defun mhc-weekly/cell-rectangle (width &optional height
					text fold base-face text-prop
					hborder vborder)
  ;; hborder: nil, 'top, 'bottom, or 'top-bottom
  ;; vborder: nil, 'left, 'right, or 'left-right
  (or height (setq height 1))
  (or text (setq text ""))
  (or fold (setq text (truncate-string-to-width text width)))
  (let* ((line-format (cond ((null vborder) "%s")
			    ((eq vborder 'left)
			     (concat mhc-weekly-column-separator "%s"))	; "|%s"
			    ((eq vborder 'right)
			     (concat "%s" mhc-weekly-column-separator))	; "%s|"
			    (t
			     (concat mhc-weekly-column-separator "%s"
				     mhc-weekly-column-separator)))) ; "|%s|"
	 (line 1)
	 rectangle)
    (set-text-properties 0 (length line-format) text-prop line-format)
    (while (<= line height)
      (let* ((str (truncate-string-to-width text width))
	     (label (concat str
			    (make-string (- width (string-width str)) ?\x20)))
	     (string (format line-format label))
	     (face (mhc-weekly/face base-face height line hborder)))
	(if face
	    (add-text-properties 0 (length string) (list 'face face) string))
	(setq rectangle (cons string rectangle))
	(setq text (substring text (length str)))
	(setq line (1+ line))))
    (nreverse rectangle)))

(defun mhc-weekly/center-string (string width)
  (or string (setq string ""))
  (let ((spaces (- width (string-width string)))
	(padding ?\x20)
	left right)
    (if (<= spaces 0)
	string
      (setq left (/ spaces 2))
      (setq right (- spaces left))
      (concat (make-string left padding)
	      string (make-string right padding)))))


(defun mhc-weekly/table-base-face (hour)
  (let ((index (% (- hour mhc-weekly-start-hour)
		  (length mhc-weekly-table-base-faces))))
    (nth index mhc-weekly-table-base-faces)))

(defun mhc-weekly/schedule-base-face (nth)
  (let ((index (% nth
		  (length mhc-weekly-schedule-base-faces))))
    (nth index mhc-weekly-schedule-base-faces)))


(defun mhc-weekly/table-label-string (day width format
					  &optional show-hour)
  (let* ((date (mhc-day-date day))
	 (ww (mhc-day-day-of-week day))
;; 	 (label (mhc-weekly/center-string
;; 		 (mhc-date-format-time-string format date) width))
	 (label (mhc-weekly/center-string
		 (cond ((stringp format)
			(mhc-date-format-time-string format date))
		       ((functionp format)
			(funcall format day)))
		 width))
	 (prop (list 'mhc-weekly/date-prop date))
	 (face (or (get-text-property (/ (length label) 2) 'face label)
		   (cond ((= ww 0)
			  'mhc-calendar-face-sunday)
			 ((mhc-day-holiday day)
			  (mhc-face-category-to-face "Holiday"))
			 ((= ww 6)
			  'mhc-calendar-face-saturday)
			 (t 'mhc-calendar-face-default)))))
    (if (mhc-date= (mhc-date-now) date)
	(setq face (mhc-face-get-today-face face)))
    (if (mhc-day-busy-p day)
	(setq face (mhc-face-get-busy-face face)))
    (let ((len (length label)))
      (if (text-properties-at (/ len 2) label)
	(progn
	  (remove-text-properties 0 len '(face nil) label)
	  (setq prop (append prop (text-properties-at (/ len 2) label))))))
    (apply 'concat
	   (append (if show-hour
		       (mhc-weekly/cell-rectangle mhc-weekly-hour-width))
		   (mhc-weekly/cell-rectangle 0 1 "" nil nil prop nil
					      'left)
		   (mhc-weekly/cell-rectangle width 1 label nil face
					      prop)))))

(defun mhc-weekly/rectangle-table-header (day width &optional show-hour)
  (mapcar (lambda (format)
	    (mhc-weekly/table-label-string day width format show-hour))
	  mhc-weekly-header-formats))

(defun mhc-weekly/rectangle-table-body-row (day width hour
						&optional show-hour format
						text)
  (or format (setq format "%02d"))
  (let* ((base-face (mhc-weekly/table-base-face hour))
;; 	 (hours-with-top-border (list mhc-weekly-start-hour 12 18
;; 				      (+ mhc-weekly-end-hour 1)))
	 (hours-with-top-border (list mhc-weekly-start-hour 12 18))
	 (hborder (if (memq hour hours-with-top-border) 'top))
	 (date-prop (list 'mhc-weekly/date-prop (mhc-day-date day)))
	 (hour-cell (mhc-weekly/cell-rectangle mhc-weekly-hour-width
					       mhc-weekly-lines-par-hour
					       (format format hour)
					       nil base-face nil hborder))
	 (day-cell (mhc-weekly/cell-rectangle width
					      mhc-weekly-lines-par-hour
					      text
					      nil base-face date-prop
					      hborder 'left)))
    (if show-hour
	(mhc-weekly-join-rectangles hour-cell day-cell)
      day-cell)))

(defun mhc-weekly-headline (day)
  (catch 'found
    (mapc (lambda (schedule)
	    (if (mhc-schedule-in-category-p schedule
					    mhc-weekly-headline-categories)
		(throw 'found schedule)))
	  (mhc-day-schedules day))
    nil))

;; (defun mhc-weekly-headline-subject (day)
;;   (mhc-schedule-subject (mhc-weekly-headline day)))

(defun mhc-weekly-headline-subject (day)
  (let ((headline (mhc-weekly-headline day)))
    (if headline
	(let* ((subject (copy-sequence	; XXX need to copy?
			 (mhc-schedule-subject-as-string headline)))
	       (prop (list 'mhc-weekly/schedule-prop headline
			   'help-echo subject
			   'face (mhc-face-category-to-face
				  (car (mhc-schedule-categories headline))))))
	  (add-text-properties 0 (length subject) prop subject)
	  subject))))

(defun mhc-weekly/rectangle-table-body-schedule (day width &optional show-hour)
  (let ((hour mhc-weekly-start-hour)
	rectangle)
    (while (<= hour mhc-weekly-end-hour)
      (setq rectangle
	    (append rectangle
		    (mhc-weekly/rectangle-table-body-row day width hour
							 show-hour)))
      (setq hour (1+ hour)))
    rectangle))

(defun mhc-weekly/rectangle-table-body-task (day width &optional show-hour)
  (let ((mhc-weekly-lines-par-hour mhc-weekly-task-lines))
    (mhc-weekly/rectangle-table-body-row day width (+ mhc-weekly-end-hour 1)
					 show-hour "")))

(defun mhc-weekly/rectangle-blank-table (day width &optional show-hour)
    (append (mhc-weekly/rectangle-table-header day width show-hour)
	    (mhc-weekly/rectangle-table-body-schedule day width show-hour)
	    (mhc-weekly/rectangle-table-body-task day width show-hour)))

(defun mhc-weekly/subject (schedule)
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

(defvar mhc-weekly/schedule-count nil)

(defun mhc-weekly/insert-schedule (schedule width schedule-begin)
  (goto-char schedule-begin)
  (let* ((column (current-column))
	 (mhc-weekly-start-time (mhc-time-new mhc-weekly-start-hour 0))
	 (mhc-weekly-end-time (mhc-time-new mhc-weekly-end-hour 0))
	 (time-begin (mhc-schedule-time-begin schedule))
	 (time-end (mhc-schedule-time-end schedule))
	 (time-string (mhc-schedule-time-as-string schedule))
	 (subject (mhc-weekly/subject schedule))
	 (base-face (mhc-weekly/schedule-base-face mhc-weekly/schedule-count))
	 (prop (list 'mhc-weekly/schedule-prop schedule
		     'mhc-weekly/date-prop date
		     'help-echo (concat time-string
					(if (> (length time-string) 0)
					    " ")
					subject)))
	 (border (cond ((and time-begin time-end) 'top-bottom)
		       (time-begin 'top)
		       (time-end 'bottom)
		       (t nil)))
	 row-beg row-end)
    ;; TODO: adjust `time-begin' and `time-end' if any of them is nil
    ;; or is out of range of the table.
;;     (if (and (null time-begin) (null time-end))	; XXX shouldn't happen
;; 	(setq time-begin (mhc-time-new mhc-weekly-start-hour 0)))
;;     (if (null time-begin)
;; 	(setq time-begin (mhc-time- time-end 60)))
    (if (null time-end)
	(setq time-end (mhc-time+ time-begin 60)))
;;     (if (< (mhc-time-HH time-begin) mhc-weekly-start-hour)
;; 	(setq time-begin (mhc-time-new mhc-weekly-start-hour 0)))
    (setq row-beg (/ (* (mhc-time- time-begin mhc-weekly-start-time)
			mhc-weekly-lines-par-hour)
		     60))
    (setq row-end (/ (* (mhc-time- time-end mhc-weekly-start-time 1)
			mhc-weekly-lines-par-hour)
		     60))
    (forward-line row-beg)
    (move-to-column column)
    (let ((prop0 (get-text-property (point) 'mhc-weekly/schedule-prop))
	  (w width)
	  (found nil))
      (if prop0
	  (while (and (> w 3)
		      (not found))
;; 	    (forward-char)
	    (move-to-column (+ 2 (current-column)))
	    (setq w (- width (- (current-column) column)))
	    (if (eq (get-text-property (point) 'mhc-weekly/schedule-prop)
		    prop0)
		(setq found t))))
      (setq width w))
;;     (save-excursion
;;       (move-to-column (+ column width -1))
;;       (let ((prop0 (get-text-property (point) 'mhc-weekly/schedule-prop))
;; 	    (w width)
;; 	    (found nil))
;; 	(if prop0
;; 	    (while (and (> w 3)
;; 			(not found))
;; 	      (backward-char)
;; 	      (setq w (- (current-column) column -1))
;; 	      (if (eq (get-text-property (point) 'mhc-weekly/schedule-prop)
;; 		      prop0)
;; 		  (setq found t))))
;; 	(setq width w)))
    (mhc-weekly-overwrite-rectangle
     (mhc-weekly/cell-rectangle width (- row-end row-beg -1) subject t
				base-face prop border)))
  (setq mhc-weekly/schedule-count (1+ mhc-weekly/schedule-count)))

(defun mhc-weekly/insert-schedules (schedules width)
  (let ((schedule-beg (point))
	(mhc-weekly/schedule-count 0))
    (while schedules
      (let ((schedule (car schedules)))
	(if (or (mhc-schedule-time-begin schedule)
		(mhc-schedule-time-end schedule))
	    (mhc-weekly/insert-schedule schedule width schedule-beg)))
      (setq schedules (cdr schedules)))))

(defun mhc-weekly/insert-tasks (schedules width)
  (let ((hour (+ mhc-weekly-end-hour 1))
 	(headline (mhc-weekly-headline day)) ;; XXX: day...
	rectangle)
    (while schedules
      (if (not (or (mhc-schedule-time-begin (car schedules))
		   (mhc-schedule-time-end (car schedules))
		   (and (not mhc-weekly-show-headline-in-tasks-too)
			(eq headline (car schedules)))))
	  (let* ((subject (mhc-weekly/subject (car schedules)))
;; 		 (base-face (mhc-weekly/table-base-face hour))
		 (base-face (get-text-property 0 'face subject)) ;; XXX
		 (prop (list 'mhc-weekly/schedule-prop (car schedules)
			     'mhc-weekly/date-prop date	;; FIXME date!!
			     'help-echo subject)))
	    (setq rectangle
		  (append
		   rectangle
		   (mhc-weekly/cell-rectangle width 1
					      subject t base-face
					      prop)))))
      (setq schedules (cdr schedules)))
    (mhc-weekly-overwrite-rectangle rectangle)))

(defun mhc-weekly/day-widths-to-fit (whole-width)
  (let* ((width (- whole-width mhc-weekly-hour-width 8))
	 (wid (/ width 7))
	 (mod (% width 7))
	 (widths (make-vector 7 wid)))
    (while (> mod 0)
      (setq mod (1- mod))
      (aset widths mod (1+ wid)))
    widths))

(defun mhc-weekly/day-width (day-of-week)
  (unless mhc-weekly-day-widths-cache
    (setq mhc-weekly-day-widths-cache
	  (if (or (not mhc-weekly-day-widths-fit-to-window)
		  (and (integerp mhc-weekly-day-widths-fit-to-window)
		       (<= (window-width) mhc-weekly-day-widths-fit-to-window)))
	      mhc-weekly-day-widths
	    (mhc-weekly/day-widths-to-fit (window-width)))))
  (aref mhc-weekly-day-widths-cache day-of-week))

(defun mhc-weekly/insert-time-table (day &optional show-hour)
  (let ((date (mhc-day-date day))	;; used in insert-tasks
;; 	(width (aref mhc-weekly-day-widths (mhc-day-day-of-week day)))
	(width (mhc-weekly/day-width (mhc-day-day-of-week day)))
	(schedule-column (+ (current-column) (if show-hour
						 mhc-weekly-hour-width
					       0)
			    (string-width mhc-weekly-column-separator)))
	(beg (point))
	schedule-beg)
    (insert-rectangle (mhc-weekly/rectangle-blank-table day width show-hour))
    (save-excursion
      (goto-char beg)
      (forward-line (length mhc-weekly-header-formats))
      (move-to-column schedule-column)
      (save-excursion
	(mhc-weekly/insert-schedules (mhc-day-schedules day) width))
      ;;
      (forward-line (* (- mhc-weekly-end-hour mhc-weekly-start-hour -1)
		       mhc-weekly-lines-par-hour))
      (move-to-column schedule-column)
      (mhc-weekly/insert-tasks (mhc-day-schedules day) width))))

(defun mhc-weekly/number-of-tasks (day)
  (let ((schedules (mhc-day-schedules day))
	(headline (mhc-weekly-headline day))
	(num 0))
    (while schedules
      (if (or (mhc-schedule-time-begin (car schedules))
	      (mhc-schedule-time-end (car schedules)))
	  nil
	(if (and (not mhc-weekly-show-headline-in-tasks-too)
		 (eq headline (car schedules)))
	    nil
	  (setq num (+ num 1))))
      (setq schedules (cdr schedules)))
    num))

(defun mhc-weekly-make-header (date)
  (let ((header (funcall mhc-calendar-header-function date)))
    (if (string-match "^ *\\(.*\\) *$" header)
	(match-string 1 header)
      header)))

(defun mhc-weekly-insert-table (date)
  (let* ((days (mhc-db-scan date (mhc-date+ date 6)))
	 (max (apply 'max (mapcar 'mhc-weekly/number-of-tasks days)))
	 (mhc-weekly-task-lines (max max mhc-weekly-task-lines))
	 (first t))
;;     (setq header-line-format (funcall mhc-calendar-header-function date))
    (setq header-line-format
	  (concat "  "
		  (if (mhc-date-yymm= date (mhc-date+ date 6))
		      (mhc-weekly-make-header date)
		    (concat (mhc-weekly-make-header date)
			    " / "
			    (mhc-weekly-make-header (mhc-date+ date 6))))))
    (while days
      (save-excursion
	(mhc-weekly/insert-time-table (car days) first))
      (end-of-line)
      (setq first nil)
      (setq days (cdr days)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mhc-weekly/create-buffer (date)
  (set-buffer (get-buffer-create mhc-weekly/buffer))
  (setq buffer-read-only t)
  (unless (eq major-mode 'mhc-weekly-mode)
    (mhc-weekly-mode)
    (buffer-disable-undo))
  (or (mhc-date-p date) (setq date (mhc-date-now)))
  (let* ((buffer-read-only nil)
	 (ww (mhc-date-ww date)))
    (erase-buffer)
    (message "mhc-weekly create...")
    (setq mhc-weekly-date
	  (- date
	     (- ww mhc-weekly-start-day-of-week)
	     (if (< ww mhc-weekly-start-day-of-week)
		 7 0)))
    (mhc-weekly-insert-table mhc-weekly-date)
    (run-hooks 'mhc-weekly-create-buffer-hook)
    (set-buffer-modified-p nil)
    (message "mhc-weekly create...done")))

(unless mhc-weekly-mode-map
  (setq mhc-weekly-mode-map (make-sparse-keymap))
  (define-key mhc-weekly-mode-map "."    'mhc-weekly-goto-today)
  (define-key mhc-weekly-mode-map "g"    'mhc-weekly-goto-day)
  (define-key mhc-weekly-mode-map "r"    'mhc-weekly-rescan)
  (define-key mhc-weekly-mode-map "R"    'mhc-weekly-reset)
  (define-key mhc-weekly-mode-map "s"    'mhc-weekly-scan)
  (define-key mhc-weekly-mode-map "E"    'mhc-weekly-edit)
  (define-key mhc-weekly-mode-map "M"    'mhc-weekly-modify)
  (define-key mhc-weekly-mode-map "D"    'mhc-weekly-delete)
  (define-key mhc-weekly-mode-map "v"    'mhc-weekly-goto-view)
;;   (define-key mhc-weekly-mode-map "H"    'mhc-weekly-hnf-edit)
;;   (define-key mhc-weekly-mode-map "v"    'mhc-weekly-goto-view)
;;   (define-key mhc-weekly-mode-map "h"    'mhc-weekly-goto-home)
  (define-key mhc-weekly-mode-map "t"    'mhc-weekly-goto-task)
  (define-key mhc-weekly-mode-map "n"    'mhc-weekly-next-line)
  (define-key mhc-weekly-mode-map "p"    'mhc-weekly-prev-line)
  (define-key mhc-weekly-mode-map "N"    'mhc-weekly-next-schedule)
  (define-key mhc-weekly-mode-map "P"    'mhc-weekly-prev-schedule)
  (define-key mhc-weekly-mode-map "f"    'mhc-weekly-next-day)
  (define-key mhc-weekly-mode-map "b"    'mhc-weekly-prev-day)
  (define-key mhc-weekly-mode-map ">"    'mhc-weekly-next-week)
  (define-key mhc-weekly-mode-map "<"    'mhc-weekly-prev-week)
  (define-key mhc-weekly-mode-map "\M-}" 'mhc-weekly-next-month)
  (define-key mhc-weekly-mode-map "\M-{" 'mhc-weekly-prev-month)
  (define-key mhc-weekly-mode-map "\M-\C-n" 'mhc-weekly-next-year)
  (define-key mhc-weekly-mode-map "\M-\C-p" 'mhc-weekly-prev-year)
  (define-key mhc-weekly-mode-map "\M-<" 'mhc-weekly-beginning-of-day)
  (define-key mhc-weekly-mode-map "\M->" 'mhc-weekly-end-of-day)
  (define-key mhc-weekly-mode-map "\C-a" 'mhc-weekly-beginning-of-week)
  (define-key mhc-weekly-mode-map "\C-e" 'mhc-weekly-end-of-week)
  (define-key mhc-weekly-mode-map "^"    'mhc-weekly-enlarge-table)
  (define-key mhc-weekly-mode-map "q"    'mhc-weekly-quit)
  (define-key mhc-weekly-mode-map "Q"    'mhc-weekly-exit)
  (define-key mhc-weekly-mode-map "\C-c?" 'mhc-calendar)
  (define-key mhc-weekly-mode-map "?"    'mhc-weekly-describe-schedule))

(defun mhc-weekly-mode ()
  "MHC Weekly mode:: major mode to view weekly calendar."
  (interactive)
  (kill-all-local-variables)
  (use-local-map mhc-weekly-mode-map)
  (make-local-variable 'mhc-weekly-date)
  (make-local-variable 'mhc-weekly-view-date)
  (make-local-variable 'indent-tabs-mode)
  (setq major-mode 'mhc-weekly-mode)
  (setq mode-name "mhc-weekly")
  (setq indent-tabs-mode nil)
  (setq truncate-lines t)
  (require 'mhc-face)
  (mhc-face-setup)
;;   (when (featurep 'xemacs)
;;     (easy-menu-add mhc-weekly-mode-menu))
  (run-hooks 'mhc-weekly-mode-hook))

(defun mhc-weekly (&optional date)
  "Display weekly schedule."
  (interactive)
  (setq date (or date
		 (mhc-current-date)
		 (mhc-calendar-get-date) ;; (mhc-weekly-get-date)))
		 (mhc-date-now)))
  (mhc-weekly/create-buffer date)
  (mhc-weekly/goto-date date)
  (if (mhc-date= date (mhc-date-now))
      (mhc-weekly-goto-time)))

(defun mhc-weekly-get-date ()
  (if (get-buffer mhc-weekly/buffer)
      (progn
       (set-buffer (get-buffer mhc-weekly/buffer))
       (or (get-text-property (point) 'mhc-weekly/date-prop)
	   (if (eolp)
	       (+ mhc-weekly-date 6)
	     mhc-weekly-date)))
    (mhc-date-now)))

(defun mhc-weekly/goto-date (date)
  (unless (get-buffer mhc-weekly/buffer)
    (mhc-weekly/create-buffer date))
  (set-buffer (get-buffer mhc-weekly/buffer))
  (switch-to-buffer mhc-weekly/buffer)
  (let ((lines 0))
    (if (or (< date mhc-weekly-date)
	    (>= date (mhc-date+ mhc-weekly-date 7)))
	(progn
	  (setq lines
		(- (count-lines (point-min) (point)) 1)) ; preserve line no.
	  (mhc-weekly/create-buffer date)
	  (goto-char (point-min))))
    (mhc-weekly-next-line lines date)))

(defun mhc-weekly-goto-today (&optional arg)
  (interactive "p")
  (mhc-weekly/goto-date (mhc-date-now))
  (mhc-weekly-goto-time))

(defun mhc-weekly-next-day (&optional arg)
  (interactive "p")
  (mhc-weekly/goto-date (+ (mhc-weekly-get-date) arg)))

(defun mhc-weekly-prev-day (&optional arg)
  (interactive "p")
  (mhc-weekly-next-day (- arg)))

(defun mhc-weekly-next-week (&optional arg)
  (interactive "p")
  (mhc-weekly/goto-date (+ (mhc-weekly-get-date) (* 7 arg))))

(defun mhc-weekly-prev-week (&optional arg)
  (interactive "p")
  (mhc-weekly-next-week (- arg)))

(defun mhc-weekly-next-month (&optional arg)
  (interactive "p")
  (mhc-weekly/goto-date (mhc-date-mm+ (mhc-weekly-get-date) arg)))

(defun mhc-weekly-prev-month (&optional arg)
  (interactive "p")
  (mhc-weekly-next-month (- arg)))

(defun mhc-weekly-next-year (&optional arg)
  (interactive "p")
  (mhc-weekly/goto-date (mhc-date-yy+ (mhc-weekly-get-date) arg)))

(defun mhc-weekly-prev-year (&optional arg)
  (interactive "p")
  (mhc-weekly-next-year (- arg)))

(defun mhc-weekly-next-line (&optional arg date)
  (interactive "p")
  (or date (setq date (mhc-weekly-get-date)))
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
    (if (looking-at (regexp-quote mhc-weekly-column-separator))
	(goto-char (match-end 0)))
    (mhc-weekly-describe-schedule)))

(defun mhc-weekly-prev-line (&optional arg)
  (interactive "p")
  (mhc-weekly-next-line (- arg)))

(defun mhc-weekly-goto-time (&optional time)
  (or time (setq time (mhc-time-now)))
  (let* ((date (mhc-weekly-get-date))
	 (hour (mhc-time-HH time))
	 (min (mhc-time-MM time))
	 (hour-regexp (concat "^" (regexp-quote (format "%02d" hour))))
	 (pos (point)))
    (cond ((< hour mhc-weekly-start-hour)
	   (mhc-weekly-beginning-of-day))
	  ((> hour mhc-weekly-end-hour)
	   (mhc-weekly-end-of-day))
	  (t
	   (goto-char (point-min))
	   (if (re-search-forward hour-regexp nil t)
	       (progn
		 (mhc-weekly/goto-date date)
		 (mhc-weekly-next-line (/ (* min mhc-weekly-lines-par-hour)
					  60)))
	     (goto-char pos))))
    (mhc-weekly-describe-schedule)))

(defun mhc-weekly-goto-task ()
  "Go to the beginning of the task line on the current day."
  (interactive)
  (mhc-weekly-goto-time (mhc-time-new mhc-weekly-end-hour 0))
  (mhc-weekly-next-line mhc-weekly-lines-par-hour))

(defun mhc-weekly-next-schedule (&optional arg)
  (interactive "p")
  (let ((direction (signum arg))
	(prev-prop (get-text-property (point) 'mhc-weekly/schedule-prop))
	prop found)
    (setq arg (abs arg))
    (while (> arg 0)
      (setq found nil)
      (while (and (not found)
		  (let ((pos (point)))
		    (mhc-weekly-next-line direction)
		    (not (= (point) pos))))
	(if (and (setq prop
		       (get-text-property (point) 'mhc-weekly/schedule-prop))
		 (not (eq prop prev-prop)))
	    (setq found t))
	(setq prev-prop prop))
      (setq arg (- arg 1)))))

(defun mhc-weekly-prev-schedule (&optional arg)
  (interactive "p")
  (mhc-weekly-next-schedule (- arg)))

(defun mhc-weekly-goto-day (&optional date)
  (interactive)
  (let ((default-date (get-text-property (point) 'mhc-weekly/date-prop)))
    (mhc-weekly/goto-date (if (integerp date)
			      date
			    (car (mhc-input-day "Date: " default-date))))))

(defun mhc-weekly-beginning-of-day ()
  (interactive)
  (let ((date (mhc-weekly-get-date)))
    (goto-char (point-min))
    (forward-line (length mhc-weekly-header-formats))
    (mhc-weekly-goto-day date)))

(defun mhc-weekly-end-of-day ()
  (interactive)
  (let ((date (mhc-weekly-get-date)))
    (goto-char (point-max))
    (mhc-weekly-goto-day date)))

(defun mhc-weekly-beginning-of-week (&optional arg)
  (interactive "p")
  (let* ((ww (mhc-date-ww (mhc-weekly-get-date)))
	 (date (mhc-date- mhc-weekly-date
			  (if (= ww mhc-weekly-start-day-of-week)
			      (* 7 arg)
			    (* 7 (1- arg))))))
    (mhc-weekly-goto-day date)))

(defun mhc-weekly-end-of-week (&optional arg)
  (interactive "p")
  (let* ((ww (mhc-date-ww (mhc-weekly-get-date)))
	 (date (mhc-date+ mhc-weekly-date -1
			  (if (= ww (mod (1- mhc-weekly-start-day-of-week) 7))
			      (* 7 (1+ arg))
			    (* 7 arg)))))
    (mhc-weekly-goto-day date)))

(defun mhc-weekly-enlarge-table (&optional arg)
  "Increase the lines per hour."
  (interactive "p")
  (setq mhc-weekly-lines-par-hour (+ mhc-weekly-lines-par-hour arg))
  (if (< mhc-weekly-lines-par-hour 1)
      (setq mhc-weekly-lines-par-hour 1))
  (if (> mhc-weekly-lines-par-hour 6)
      (setq mhc-weekly-lines-par-hour 6))
  (mhc-weekly-rescan)) ;; FIXME: cursor position is not preserved properly.

(defun mhc-weekly-rescan ()
  (interactive)
  (if (get-buffer mhc-weekly/buffer)
      (let (pos top)
	(set-buffer mhc-weekly/buffer)
	(setq pos (point))
	(setq top (save-excursion
		    (move-to-window-line 0)
		    (point)))
	(mhc-weekly/create-buffer mhc-weekly-date)
;;     (mhc-weekly/goto-date pdate)))
	(goto-char top)
	(recenter 0)
	(goto-char pos))))

(defun mhc-weekly-reset (&optional update-widths)
  (interactive "P")
  (mhc-reset)
  (if update-widths
      (setq mhc-weekly-day-widths-cache nil))
  (mhc-weekly-rescan)
  (mhc-weekly-describe-schedule))

(defun mhc-weekly-scan (&optional hide-private)
  (interactive "P")
  (let ((date (mhc-weekly-get-date)))
    (mhc-weekly-quit)
    (mhc-goto-month date hide-private)
    (goto-char (point-min))
    (if (mhc-summary-search-date date)
	(progn
	  (beginning-of-line)
	  (if (not (pos-visible-in-window-p (point)))
	      (recenter))))))

(defun mhc-weekly-quit ()
  (interactive)
  (let ((win (get-buffer-window mhc-weekly/buffer))
	(buf (get-buffer mhc-weekly/buffer)))
;;     (save-excursion
;;       (set-buffer buf)
;;       (mhc-weekly/delete-overlay))
    (when win
      (bury-buffer buf)
      (if (null (one-window-p))
	  (delete-windows-on buf)
	(set-window-buffer win (other-buffer))
	(select-window (next-window))))))

(defun mhc-weekly-describe-schedule ()
  "Describe the schedule on cursor position."
  (interactive)
;;   (let ((schedule (get-text-property (point) 'mhc-weekly/schedule-prop)))
  (let ((date (get-text-property (point) 'mhc-weekly/date-prop))
	(help-echo (get-text-property (point) 'help-echo)))
    (if help-echo
	(message "%s %s"
		 (mhc-date-format-time-string "%m/%d(%a)" date)
		 help-echo)
      (if (interactive-p)
	  (message "No schedule on cursor.")
	(message "")))))

(defun mch-weekly/schedule
  (get-text-property (point) 'mhc-weekly/schedule-prop))

(defadvice mhc-current-date (after recognize-mhc-weekly-buffer activate)
  (if (and (not ad-return-value)
	   (eq major-mode 'mhc-weekly-mode))
      (setq ad-return-value (mhc-weekly-get-date))))

;; for `mhc-weekly-edit' and `mhc-weekly-modify'.
(add-hook 'mhc-draft-finish-hook
	  '(lambda ()
	     (if (eq major-mode 'mhc-weekly-mode)
		 (mhc-weekly-rescan))))

(defun mhc-weekly-edit ()
  (interactive)
  (mhc-window-push)
  (mhc-edit nil)
  (delete-other-windows))

(defun mhc-weekly-delete ()
  (interactive)
  (let* ((schedule (get-text-property (point) 'mhc-weekly/schedule-prop))
	 (filename (mhc-record-name (mhc-schedule-record schedule))))
    (if (null filename)
	(message "Nothing to do in this point.")
      (setq key (mhc-slot-directory-to-key
		 (directory-file-name (file-name-directory filename))))
      (mhc-delete-file
       (assoc filename
	      (mhc-slot-records (mhc-slot-get-month-schedule key))))
      (let ((pos (point)))
	(mhc-weekly-rescan)
	(goto-char pos)))))

(defun mhc-weekly-modify ()
  (interactive)
  (let* ((schedule (get-text-property (point) 'mhc-weekly/schedule-prop))
	 (filename (mhc-record-name (mhc-schedule-record schedule))))
    (if filename
	(mhc-modify-file filename)
      (message "Nothing to do in this point."))))

(defun mhc-weekly-goto-view ()
  (interactive)
  (let* ((schedule (get-text-property (point) 'mhc-weekly/schedule-prop))
	 (filename (mhc-record-name (mhc-schedule-record schedule))))
    (if filename
	(progn
	  (view-file-other-window filename)
	  (mhc-calendar/view-file-decode-header)
	  (set-visited-file-name nil)
	  (set-buffer-modified-p nil)
	  (setq buffer-read-only t)
;; 	  (re-search-forward "^$" nil t))
	  (run-hooks 'mhc-weekly-goto-view-hook)
	  )
      (message "Nothing to do in this point."))))

(provide 'mhc-weekly)

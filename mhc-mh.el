;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Author:  Tokuya Kameshima
;; Created: 2002/11/28

;;; $Id: mhc-mh.el,v 1.2 2003/12/24 02:13:29 kame Exp $

;;; Commentary:

;; This file is a part of MHC, includes MUA backend methods for MH-E
;; (EMH).

;;; Code:

(require 'mh-e)
(require 'mhc-vars)
(require 'mhc-mime)

;; Setup function:

;;;###autoload
(defun mhc-mh-setup ()
  (require 'mhc)
  (setq mhc-mailer-package 'mh)
  (setq mh-good-msg-regexp  "^ *[0-9]+[^0-9D^]")
  (mhc-setup)
  (autoload 'mhc-mode "mhc" nil t)
  (add-hook 'mh-folder-mode-hook 'mhc-mode)
  (add-hook 'mh-quit-hook 'mhc-exit))


;; Backend methods:


(defun mhc-mh-summary-filename ()
  "Return FILENAME on current line."
  (let ((msg (mh-get-msg-num t))
	(folder mh-current-folder))
    (cond
     ((>= msg 100000)
      (setq folder (concat mhc-base-folder "/intersect"))
      (setq msg (- msg 100000))))
    (mh-msg-filename msg folder)))


(defun mhc-mh-summary-display-article ()
  "Display the article on the current."
  (mh-show))


(defun mhc-mh-mime-get-raw-buffer ()
  (let* ((folder (buffer-name (current-buffer)))
	 (prefix (if (memq 'emh features)
		     "article-"
		   "show-"))
	 (buf-name (concat prefix folder)))
    (or (get-buffer buf-name)
	(progn (mh-show)
	       (get-buffer buf-name)))))

(defun mhc-mh-mime-get-mime-structure ()
  (mime-open-entity 'buffer (mhc-mh-mime-get-raw-buffer)))

(defun mhc-mh-get-import-buffer (get-original)
  "Return buffer visiting import article.  If GET-ORIGINAL,
return it without MIME decode."
  (let* ((folder (buffer-name (current-buffer)))
	 (buf-name (concat "show-" folder)))
    (or (get-buffer buf-name)
	(progn (mh-show)
	       (get-buffer buf-name)))))  


(defun mhc-mh-highlight-message (for-draft)
  (if (fboundp 'emh-highlight-header)
      (progn
	(goto-char (point-min))
	(if (re-search-forward "^--text follows this line--" nil t)
	    (progn
	      (narrow-to-region (point-min) (match-beginning 0))
	      (emh-highlight-header)
	      (widen))))))

;; mhc-tmp-schedule is already bound.
(defun mhc-mh-insert-summary-contents (inserter)
  (let ((today (mhc-current-date-month))
	(date (mhc-day-date mhc-tmp-dayinfo))
	head path)
    (setq path (mhc-record-name (mhc-schedule-record mhc-tmp-schedule))
	  head
	  (cond
	   ((or (not path) (equal path mhc-schedule-file))
	    (if mhc-tmp-schedule
		"100000"
	      "------"))
	   ((string-match "/intersect/" path)
	    (format "1%05d"
		    (string-to-number (file-name-nondirectory path))))
	   ;; This month
	   ((mhc-date-yymm= today date)
	    (format "%d"
		    (string-to-number (file-name-nondirectory path))))
	   ;; Previous month
	   ((mhc-date-yymm= (mhc-date-mm- today 1) date)
	    (format "3%05d"
		    (string-to-number (file-name-nondirectory path))))
	   ;; Next month
	   ((mhc-date-yymm= (mhc-date-mm+ today 1) date)
	    (format "4%05d"
		    (string-to-number (file-name-nondirectory path)))))
	  head (concat head (if path "*| " " | ")))
    (put-text-property 0 (length head) 'invisible t head)
    (insert head)
    (funcall inserter)
    (insert "\n")))

(defadvice mh-show-msg (around mh-showm-msg-around (msg) activate)
  (if (not msg)
      (setq msg (mh-get-msg-num t)))
  (cond
   ((= msg 100000)
    (if (get-buffer mh-show-buffer)
	(delete-windows-on mh-show-buffer)))
   ((> msg 100000)
    (let ((folder (concat mhc-base-folder "/intersect"))
	  (curbuf (current-buffer)))
      (cond ((not (get-buffer folder))
	     (mh-make-folder folder))
	    ((not (eq (current-buffer) (get-buffer folder)))
	     (switch-to-buffer folder)))
      (setq msg (- msg 100000))
      (get-buffer-create mh-current-folder)
      ad-do-it
      (switch-to-buffer curbuf)))
   (t
    ad-do-it)))


(defvar mhc-mh-exit-buffer nil)
(make-variable-buffer-local 'mhc-mh-exit-buffer)

(defun mhc-mh-summary-buffer-p ()
  "Return non-nil if the current buffer is mhc summary buffer."
  mhc-mh-exit-buffer)

(defadvice mh-quit (around mh-quit-around activate)
  (if (mhc-mh-summary-buffer-p)
      (mhc-mh-summary-exit)
    ad-do-it))

(defun mhc-mh-summary-exit ()
  (let ((buffer mhc-mh-exit-buffer))
    (kill-buffer (current-buffer))
    (when (and buffer
	       (buffer-live-p buffer))
      (if (get-buffer-window buffer)
	  (unless (eq (current-buffer) buffer)
	    (delete-window)))
      (switch-to-buffer buffer)
      (if (eq (with-current-buffer buffer major-mode)
	      'mh-folder-mode)
	  (delete-other-windows)))))

(defun mhc-mh-summary-mode-setup (date)
  (let ((original mhc-mh-exit-buffer))
    (mh-folder-mode) ; buffer local variables are killed.
    (setq mhc-mh-exit-buffer original)
    ))

(defun mhc-mh-generate-summary-buffer (date)
  (let ((original (and (or (eq major-mode 'mh-summary-mode)
			   (eq major-mode 'mh-folder-mode))
		       (or mhc-mh-exit-buffer (current-buffer)))))
    (switch-to-buffer
     (set-buffer
      (mhc-get-buffer-create
       (mhc-date-format date "%s/%02d/%02d" mhc-base-folder yy mm))))
    (and original
	 (setq mhc-mh-exit-buffer original))
    (setq inhibit-read-only t
	  buffer-read-only nil
	  selective-display t
	  selective-display-ellipses nil
	  indent-tabs-mode nil)
    (widen)
    (delete-region (point-min) (point-max))
    (delete-other-windows)))


(defadvice mh-next-undeleted-msg
  (around mhc-mh-next-undeleted-msg activate)
  (let ((cur (point)))
    ad-do-it
    (when (and (mhc-mh-summary-buffer-p)
	       (= (point) cur))
      (mhc-goto-next-month 1)
      (goto-char (point-min))
      (mh-first-msg))))

(defadvice mh-previous-undeleted-msg
  (around mhc-mh-previous-undeleted-msg activate)
  (let ((cur (point)))
    ad-do-it
    (when (and (mhc-mh-summary-buffer-p)
	       (= (point) cur))
      (mhc-goto-prev-month 1)
      (goto-char (point-max))
      (mh-last-msg))))


(provide 'mhc-mh)
(put 'mhc-mh 'summary-filename 'mhc-mh-summary-filename)
(put 'mhc-mh 'summary-display-article 'mhc-mh-summary-display-article)
(put 'mhc-mh 'generate-summary-buffer 'mhc-mh-generate-summary-buffer)
(put 'mhc-mh 'insert-summary-contents 'mhc-mh-insert-summary-contents)
(put 'mhc-mh 'summary-mode-setup 'mhc-mh-summary-mode-setup)
;; (put 'mhc-mh 'get-import-buffer 'mhc-mime-get-import-buffer)
(put 'mhc-mh 'get-import-buffer 'mhc-mh-get-import-buffer)
(put 'mhc-mh 'mime-get-raw-buffer 'mhc-mh-mime-get-raw-buffer)
(put 'mhc-mh 'mime-get-mime-structure 'mhc-mh-mime-get-mime-structure)
(put 'mhc-mh 'highlight-message 'mhc-mh-highlight-message)
(put 'mhc-mh 'draft-setup-new 'mhc-mime-draft-setup-new)
(put 'mhc-mh 'draft-reedit-buffer 'mhc-mime-draft-reedit-buffer)
(put 'mhc-mh 'draft-reedit-file 'mhc-mime-draft-reedit-file)
(put 'mhc-mh 'draft-translate 'mhc-mime-draft-translate)
(put 'mhc-mh 'eword-decode-string 'mhc-mime-eword-decode-string)
(put 'mhc-mh 'decode-header 'mhc-mime-decode-header)

;;; Copyright Notice:

;; Copyright (C) 2002 Tokuya Kameshima. All rights reserved.
;; Copyright (C) 1999, 2000 Yoshinari Nomura. All rights reserved.
;; Copyright (C) 2000 MHC developing team. All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS''
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
;; THE TEAM OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mhc-mh.el ends here.

;;; -*- Emacs-Lisp -*-
;;; comment/uncomment region for emacs.
;;; comment.el rev.0.0
;;; (c ) 1992 by Hirose Yuuji.(yuuji@ae.keio.ac.jp)
;;; Last modified Sat Jan 29 16:55:22 1994 on gloria

(provide 'comment)

(defvar current-comment-prefix "> "
  "default prefix string")

(defun cite-region nil
  (save-excursion
    (if (< (point) (mark)) (exchange-point-and-mark))
    (if (bolp)
	(forward-line -1))
    (if (string= string "") (setq string current-comment-prefix)
      (setq current-comment-prefix string))
    (save-restriction 
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (while (re-search-forward "^" nil t)
	(message "%s" string)
	(replace-match string))
      ))
)

(defun comment-region (string &optional once)
  "Inserts STRING at the beginning of every line in the region.
Called interactively, STRING defaults to comment-start (or '> ' if
none is defined) unless a prefix argument is given, in which case it
prompts for a string.  Optional second argument ONCE is only for
compatibility for uncomment-region.  It has no means now."
  (interactive
   (list (if current-prefix-arg
	     (read-string 
	      (concat "String to insert"
		      (format "(default \"%s\")" current-comment-prefix
			      " ")
		      ": "))
	   current-comment-prefix
	   )))
  (if (not (stringp string)) (setq string current-comment-prefix))
  (cite-region)
)


(defun uncomment-region (string &optional once)
  "Deletes STRING from the beginning of every line in the region.
Called interactively, STRING defaults to comment-start (or '> ' if
none is defined) unless a prefix argument is given, in which case it
prompts for a string.  Optional second argument ONCE restricts
deletion to first occurance of STRING on each line."
  (interactive
   (list (if current-prefix-arg
	     (read-string 
	      (concat "String to delete"
		      (format "(default \"%s\")" current-comment-prefix
			      " ")
		      ": "))
	   current-comment-prefix
	   )))
  (if (not (stringp string)) (setq string current-comment-prefix))
  (save-excursion
    (if (< (point) (mark)) (exchange-point-and-mark))
;    (if (bolp)
;         (forward-line -1))
    (save-restriction 
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (while (re-search-forward (concat "^" string) nil t)
	(replace-match "")
	(if once (end-of-line)))
      ))
)

(defun cite-file (filename)
  "insert the file with citation string."
  (interactive "FCite-file: ")
  (let*
      ((string
	(read-string
	 (format "Citation string (default \"%s\"): " current-comment-prefix)
	 ))
       (ins-tail (car (cdr (insert-file-contents filename)))))
    (save-excursion
      (push-mark (+ (point) ins-tail))
      (cite-region)))
)

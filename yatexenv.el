;;; -*- Emacs-Lisp -*-
;;; YaTeX environment-specific functions.
;;; yatexenv.el
;;; (c ) 1994 by HIROSE Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Tue Sep 20 01:35:46 1994 on figaro
;;; $Id$

;;;
;; Functions for tabular environment
;;;

;; Showing the matching column of tabular environment.
(defun YaTeX-array-what-column ()
  "Show matching columne title of array environment.
When calling from a program, make sure to be in array/tabular environment."
  (let ((p (point)) beg eot bor (nlptn "\\\\\\\\") (andptn "[^\\]&") (n 0)
	(firsterr "This line might be the first row."))
    (save-excursion
      (YaTeX-beginning-of-environment)
      (search-forward "{" p) (up-list 1)
      (search-forward "{" p) (up-list 1)
      ;;(re-search-forward andptn p)
      (while (progn (search-forward "&" p)
		    (equal (char-after (1- (match-beginning 0))) ?\\ )))
      (setq beg (1- (point)))		;beg is the point of the first &
      (or (re-search-forward nlptn p t)
	  (error firsterr))
      (setq eot (point))		;eot is the point of the first \\
      (goto-char p)
      (or (re-search-backward nlptn beg t)
	  (error firsterr))
      (setq bor (point))		;bor is the beginning of this row.
      (while (< (1- (point)) p)
	(if (equal (following-char) ?&)
	    (forward-char 1)
	  (re-search-forward andptn nil 1))
	(setq n (1+ n)))		;Check current column number
      (goto-char p)
      (cond				;Start searching \multicolumn{N}
       ((> n 1)
	(re-search-backward andptn)	;Sure to find!
	(while (re-search-backward "\\\\multicolumn{\\([0-9]+\\)}" bor t)
	  (setq n (+ n (string-to-int
			(buffer-substring (match-beginning 1)
					  (match-end 1)))
		     -1)))))
      (message "%s" n)
      (goto-char (1- beg))
      (cond
       ((= n 1) (message "Here is the FIRST column!"))
       (t (while (> n 1)
	    (or (re-search-forward andptn p nil)
		(error "This column exceeds the limit."))
	    (setq n (1- n)))
	  (skip-chars-forward "\\s ")
	  (message
	   "Here is the column of: %s"
	   (buffer-substring
	    (point)
	    (progn
	      (re-search-forward (concat andptn "\\|" nlptn) eot)
	      (goto-char (match-beginning 0))
	      (if (looking-at andptn)
		  (forward-char 1))
	      (skip-chars-backward "\\s ")
	      (point))))))))
)

;;;###autoload
(defun YaTeX-what-column ()
  "Show which kind of column the current position is belonging to."
  (interactive)
  (cond
   ((YaTeX-quick-in-environment-p '("tabular" "tabular*" "array" "array*"))
    (YaTeX-array-what-column))
   (t (message "Not in array/tabuar environment.")))
)

(defun YaTeX-tabular-parse-format (&optional tabular*)
  "Parse `tabular' format.
Return the list of (No.ofCols PointEndofFormat)"
  (let ((p (point)) elt boform eoform (cols 0))
    (save-excursion
      (if (null (YaTeX-beginning-of-environment t))
	  (error "Beginning of tabular not found."))
      (skip-chars-forward "^{")
      (forward-list 1)
      (if tabular*
	  (progn (skip-chars-forward "^{")
		 (forward-list 1)))
      (skip-chars-forward "^{" p)
      (if (/= (following-char) ?\{) (error "Tabular format not found."))
      (setq boform (1+ (point))
	    eoform (progn (forward-list 1) (1- (point))))
      (if (> eoform p) (error "Non-terminated tabular format."))
      (goto-char boform)
      (while (< (point) eoform)
	(setq elt (following-char))
	(cond
	 ((string-match (char-to-string elt) "clr") ;normal indicators.
	  (setq cols (1+ cols))
	  (forward-char 1))
	 ((equal elt ?|)		;vertical
	  (forward-char 1))
	 ((string-match (char-to-string elt) "p@") ;p or @ expression
	  (setq cols (+ (if (eq elt ?p) 1 0) cols))
	  (skip-chars-forward "^{" p)
	  (forward-list 1))))
      (list cols (1+ eoform))))
)
;; Insert &
(defun YaTeX-intelligent-newline-tabular (&optional tabular*)
  "Parse current tabular format and insert that many `&'s."
  (let*((p (point)) (format (YaTeX-tabular-parse-format tabular*))
	(cols (car format)) (beg (car (cdr format)))
	space hline)
    (setq hline (search-backward "\\hline" beg t))
    (goto-char p)
    (setq space (if (search-backward "\t&" beg t) "\t" " "))
    (goto-char p)
    (YaTeX-indent-line)
    (setq p (point))
    (while (> (1- cols) 0)
      (insert "&" space)
      (setq cols (1- cols)))
    (insert "\\\\")
    (if hline (insert " \\hline"))
    (goto-char p))
)

(defun YaTeX-intelligent-newline-tabular* ()
  "Parse current tabular* format and insert that many `&'s."
  (YaTeX-intelligent-newline-tabular t)
)

(fset 'YaTeX-intelligent-newline-array 'YaTeX-intelligent-newline-tabular)

;;;
;; Functions for tabbing environment
;;;
(defun YaTeX-intelligent-newline-tabbing ()
  "Check the number of \\= in the first line and insert that many \\>."
  (let ((p (point)) begenv tabcount)
    (save-excursion
      (YaTeX-beginning-of-environment)
      (setq begenv (point-end-of-line))
      (if (YaTeX-search-active-forward "\\\\" YaTeX-comment-prefix p t)
	  (progn
	    (setq tabcount 0)
	    (while (> (point) begenv)
	      (if (search-backward "\\=" begenv 1)
		  (setq tabcount (1+ tabcount)))))))
    (YaTeX-indent-line)
    (if tabcount
	(progn
	  (save-excursion
	    (while (> tabcount 0)
	      (insert "\\>\t")
	      (setq tabcount (1- tabcount))))
	  (forward-char 2))
      (insert "\\=")))
)

;;;
;; Functions for itemize/enumerate/list environments
;;;

(defun YaTeX-indent-for-item ()
  (let (col (p (point)) begenv)
    (save-excursion
      (YaTeX-beginning-of-environment t)
      (setq begenv (point-end-of-line))
      (goto-char p)
      (if (YaTeX-search-active-backward "\\item" YaTeX-comment-prefix begenv t)
	  (setq col (current-column))))
    (if col (indent-to col) (YaTeX-indent-line)))
)

(defvar YaTeX-item-for-insert "\\item ")
(defun YaTeX-intelligent-newline-itemize ()
  "Insert '\\item '."
  (YaTeX-indent-for-item)
  (insert YaTeX-item-for-insert)
)
(fset 'YaTeX-intelligent-newline-enumerate 'YaTeX-intelligent-newline-itemize)
(fset 'YaTeX-intelligent-newline-list 'YaTeX-intelligent-newline-itemize)

(defun YaTeX-intelligent-newline-description ()
  (YaTeX-indent-for-item)
  (insert "\\item[] ")
  (forward-char -2)
)


;;;
;; Intelligent newline
;;;
;;;###autoload
(defun YaTeX-intelligent-newline (arg)
  "Insert newline and environment-specific entry.
`\\item'	for some itemizing environment,
`\\> \\> \\'	for tabbing environemnt,
`& & \\ \hline'	for tabular environment."
  (interactive "P")
  (let*((env (YaTeX-inner-environment))
	func)
    (if arg (setq env (YaTeX-read-environment "For what environment? ")))
    (setq func (intern-soft (concat "YaTeX-intelligent-newline-" env)))
    (end-of-line)
    (newline)
    (if (and env func (fboundp func))
	(funcall func)))
)

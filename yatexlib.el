;;; -*- Emacs-Lisp -*-
;;; YaTeX library of general functions.
;;; yatexlib.el
;;; (c )1994 by HIROSE Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Thu Nov 24 02:20:45 1994 on VFR
;;; $Id$

;;;###autoload
(defun YaTeX-search-active-forward (string cmntrx &optional bound err cnt func)
  "Search STRING which is not commented out by CMNTRX.
Optional arguments after BOUND, ERR, CNT are passed literally to search-forward
or search-backward.
Optional sixth argument FUNC changes search-function."
  (let ((sfunc (or func 'search-forward)) found md)
    (while (and (prog1
		    (setq found (funcall sfunc string bound err cnt))
		  (setq md (match-data)))
		(or
		 (YaTeX-in-verb-p (match-beginning 0))
		 (save-excursion
		   (beginning-of-line)
		   (re-search-forward cmntrx (match-beginning 0) t)))))
    (store-match-data md)
    found)
)

(defun YaTeX-re-search-active-forward (regexp cmntrx &optional bound err cnt)
  "Search REGEXP backward which is not commented out by regexp CMNTRX.
See also YaTeX-search-active-forward."
  (YaTeX-search-active-forward regexp cmntrx bound err cnt 're-search-forward)
)
(defun YaTeX-search-active-backward (string cmntrx &optional bound err cnt)
  "Search STRING backward which is not commented out by regexp CMNTRX.
See also YaTeX-search-active-forward."
  (YaTeX-search-active-forward string cmntrx bound err cnt 'search-backward)
)
(defun YaTeX-re-search-active-backward (regexp cmntrx &optional bound err cnt)
  "Search REGEXP backward which is not commented out by regexp CMNTRX.
See also YaTeX-search-active-forward."
  (YaTeX-search-active-forward regexp cmntrx bound err cnt 're-search-backward)
)


;;;###autoload
(defun YaTeX-switch-to-buffer (file &optional setbuf)
  "Switch to buffer if buffer exists, find file if not.
Optional second arg SETBUF t make use set-buffer instead of switch-to-buffer."
  (interactive "Fswitch to file: ")
  (let (buf)
    (if (setq buf (get-buffer (file-name-nondirectory file)))
	(progn
	  (funcall (if setbuf 'set-buffer 'switch-to-buffer)
		   (file-name-nondirectory file))
	  buf)
      (if (file-exists-p file)
	  (funcall
	   (if setbuf 'find-file-noselect 'find-file)
	   file)
	(message "%s was not found in this directory." file)
	nil)))
)

;;;###autoload
(defun YaTeX-switch-to-buffer-other-window (file)
  "Switch to buffer if buffer exists, find file if not."
  (interactive "Fswitch to file: ")
  (if (get-buffer (file-name-nondirectory file))
      (progn (switch-to-buffer-other-window file) t)
    (if (file-exists-p file)
	(progn (find-file-other-window file) t)
      (message "%s was not found in this directory." file)
      nil))
)

(defun YaTeX-replace-format-sub (string format repl)
  (let ((beg (or (string-match (concat "^\\(%" format "\\)") string)
		 (string-match (concat "[^%]\\(%" format "\\)") string)))
	(len (length format)))
    (if (null beg) string ;no conversion
      (concat
       (substring string 0 (match-beginning 1)) repl
       (substring string (match-end 1)))))
)

;;;###autoload
(defun YaTeX-replace-format (string format repl)
  "In STRING, replace first appearance of FORMAT to REPL as if
function `format' does.  FORMAT does not contain `%'"
  (let ((ans string))
    (while (not (string=
		 ans (setq string (YaTeX-replace-format-sub ans format repl))))
      (setq ans string))
    string)
)

;;;###autoload
(defun YaTeX-replace-format-args (string &rest args)
  "Translate the argument mark #1, #2, ... #n in the STRING into the
corresponding real arguments ARGS."
  (let ((argp 1))
    (while args
      (setq string
	    (YaTeX-replace-format string (int-to-string argp) (car args)))
      (setq args (cdr args) argp (1+ argp))))
  string
)

;;;###autoload
(defun rindex (string char)
  (let ((pos (1- (length string)))(index -1))
    (while (>= pos 0)
      (cond
       ((= (aref string pos) char)
	(setq index pos) (setq pos -1))
       (t (setq pos (1- pos))))
      )
    index)
)

;;;###autoload
(defun YaTeX-showup-buffer (buffer &optional func select)
  "Make BUFFER show up in certain window (but current window)
that gives the maximum value by the FUNC.  FUNC should take an argument
of its window object.  Non-nil for optional third argument SELECT selects
that window.  This function never selects minibuffer window."
  (or (and (if YaTeX-emacs-19
	       (get-buffer-window buffer t)
	     (get-buffer-window buffer))
	   (progn
	     (if select
		 (cond
		  (YaTeX-emacs-19
		   (let ((frame (window-frame (get-buffer-window buffer t))))
		     (select-frame frame)
		     (focus-frame frame)
		     (set-mouse-position frame 0 0)
		     (raise-frame frame)
		     (select-window (get-buffer-window buffer))
		     (if (and (featurep 'windows)
			      (win:frame-window frame))
			 (win:adjust-window))))
		  (t
		   (select-window (get-buffer-window buffer)))))
	     t))
      (let ((window (selected-window))
	    (wlist (YaTeX-window-list)) win w (x 0))
	(cond
	 ((> (length wlist) 2)
	  (if func
	      (while wlist
		(setq w (car wlist))
		(if (and (not (eq window w))
			 (> (funcall func w) x))
		    (setq win w x (funcall func w)))
		(setq wlist (cdr wlist)))
	    (setq win (get-lru-window)))
	  (select-window win)
	  (switch-to-buffer buffer)
	  (or select (select-window window)))
	 ((= (length wlist) 2)
	  ;(other-window 1);This does not work properly on Emacs-19
	  (select-window (get-lru-window))
	  (switch-to-buffer buffer)
	  (or select (select-window window)))
	 (t				;if one-window
	  (cond
	   ((and YaTeX-emacs-19 (get-buffer-window buffer t))
	    nil)			;if found in other frame
	   (YaTeX-default-pop-window-height
	    (split-window
	     (selected-window)
	     (max
	      (min
	       (- (screen-height)
		  (if (numberp YaTeX-default-pop-window-height)
		      (+ YaTeX-default-pop-window-height 2)
		    (/ (* (screen-height)
			  (string-to-int YaTeX-default-pop-window-height))
		       100)))
	       (- (screen-height) window-min-height 1))
	      window-min-height))
	    (pop-to-buffer buffer)
	    (or select (select-window window)))
	   (t nil)))
	 )))
)

;;;###autoload
(defun YaTeX-window-list ()
  (let*((curw (selected-window)) (win curw) (wlist (list curw)))
    (while (not (eq curw (setq win (next-window win))))
      (or (eq win (minibuffer-window))
	  (setq wlist (cons win wlist))))
    wlist)
)

;;;###autoload
(defun substitute-all-key-definition (olddef newdef keymap)
  "Replace recursively OLDDEF with NEWDEF for any keys in KEYMAP now
defined as OLDDEF. In other words, OLDDEF is replaced with NEWDEF
where ever it appears."
  (mapcar
   (function (lambda (key) (define-key keymap key newdef)))
   (where-is-internal olddef keymap))
)

;;;###autoload
(defun YaTeX-match-string (n &optional m)
  "Return (buffer-substring (match-beginning n) (match-beginning m))."
  (if (match-beginning n)
      (buffer-substring (match-beginning n)
			(match-end (or m n))))
)

;;;###autoload
(defun YaTeX-minibuffer-complete ()
  "Complete in minibuffer.
If the symbol 'delim is bound and is string, its value is assumed to be
the character class of delimiters.  Completion will be performed on
the last field separated by those delimiters."
  (interactive)
  (let (beg word compl (md (match-data)))
    (setq beg (if (and (boundp 'delim) (stringp delim))
		  (save-excursion
		    (skip-chars-backward (concat "^" delim))
		    (point))
		(point-min))
	  word (buffer-substring beg (point-max))
	  compl (try-completion word minibuffer-completion-table))
    (cond
     ((eq compl t)
      (let ((p (point)) (max (point-max)))
	(goto-char max)
	(insert " [Sole completion]")
	(goto-char p)
	(sit-for 1)
	(delete-region max (point-max))
	(goto-char p)))
     ((eq compl nil)
      (ding)
      (save-excursion
	(let (p)
	  (goto-char (setq p (point-max)))
	  (insert " [No match]")
	  (goto-char p)
	  (sit-for 2)
	  (delete-region p (point-max)))))
     ((string= compl word)
      (with-output-to-temp-buffer "*Completions*"
	(display-completion-list
	 (all-completions word minibuffer-completion-table))))
     (t (delete-region beg (point-max))
	(insert compl))
     )
    (store-match-data md))
)


(provide 'yatexlib)

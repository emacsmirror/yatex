;;; -*- Emacs-Lisp -*-
;;; YaTeX library of general functions.
;;; yatexlib.el
;;; (c )1994-1995 by HIROSE Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Fri Apr 28 16:17:44 1995 on VFR
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
  (if (bufferp file) (setq file (buffer-file-name file)))
  (let (buf (hilit-auto-highlight (not setbuf)))
    (cond
     ((setq buf (get-file-buffer file))
      (funcall (if setbuf 'set-buffer 'switch-to-buffer)
	       (get-file-buffer file))
      buf)
     ((or YaTeX-create-file-prefix-g (file-exists-p file))
      (or ;find-file returns nil but set current-buffer...
       (if setbuf (set-buffer (find-file-noselect file))
	 (find-file file))
       (current-buffer)))
     (t (message "%s was not found in this directory." file)
	nil)))
)

;;;###autoload
(defun YaTeX-switch-to-buffer-other-window (file)
  "Switch to buffer if buffer exists, find file if not."
  (interactive "Fswitch to file: ")
  (if (bufferp file) (setq file (buffer-file-name file)))
  (cond
   ((get-file-buffer file)
    (switch-to-buffer-other-window (get-file-buffer file))
    t)
   ((or YaTeX-create-file-prefix-g (file-exists-p file))
    (find-file-other-window file) t)
   (t (message "%s was not found in this directory." file)
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
  (or (and (if (and YaTeX-emacs-19 select)
	       (get-buffer-window buffer t)
	     (get-buffer-window buffer))
	   (progn
	     (if select
		 (goto-buffer-window buffer))
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
	    (split-window-calculate-height YaTeX-default-pop-window-height)
	    (pop-to-buffer buffer)
	    (or select (select-window window)))
	   (t nil)))
	 )))
)

;;;###autoload
(defun split-window-calculate-height (height)
  "Split current window wight specified HEIGHT.
If HEIGHT is number, make new window that has HEIGHT lines.
If HEIGHT is string, make new window that occupy HEIGT % of screen height.
Otherwise split window conventionally."
  (if (one-window-p)
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
	window-min-height)))
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
the last field separated by those delimiters.
  If the symbol 'quick is bound and is 't, when the try-completion results
in t, exit minibuffer immediately."
  (interactive)
  (let ((md (match-data)) beg word compl
	(quick (and (boundp 'quick) (eq quick t)))
	(displist			;function to display completion-list
	 (function
	  (lambda ()
	    (with-output-to-temp-buffer "*Completions*"
	      (display-completion-list
	       (all-completions word minibuffer-completion-table)))))))
    (setq beg (if (and (boundp 'delim) (stringp delim))
		  (save-excursion
		    (skip-chars-backward (concat "^" delim))
		    (point))
		(point-min))
	  word (buffer-substring beg (point-max))
	  compl (try-completion word minibuffer-completion-table))
    (cond
     ((eq compl t)
      (if quick (exit-minibuffer)
	(let ((p (point)) (max (point-max)))
	  (unwind-protect
	      (progn
		(goto-char max)
		(insert " [Sole completion]")
		(goto-char p)
		(sit-for 1))
	    (delete-region max (point-max))
	    (goto-char p)))))
     ((eq compl nil)
      (ding)
      (save-excursion
	(let (p)
	  (unwind-protect
	      (progn
		(goto-char (setq p (point-max)))
		(insert " [No match]")
		(goto-char p)
		(sit-for 2))
	    (delete-region p (point-max))))))
     ((string= compl word)
      (funcall displist))
     (t (delete-region beg (point-max))
	(insert compl)
	(if quick
	    (if (eq (try-completion compl minibuffer-completion-table) t)
		(exit-minibuffer)
	      (funcall displist)))))
    (store-match-data md))
)

(defun YaTeX-minibuffer-quick-complete ()
  "Set 'quick to 't and call YaTeX-minibuffer-complete.
See documentation of YaTeX-minibuffer-complete."
  (interactive)
  (let ((quick t))
    (self-insert-command 1)
    (YaTeX-minibuffer-complete)))

(defun foreach-buffers (pattern job)
  "For each buffer which matches with PATTERN, do JOB."
  (let ((list (buffer-list)))
    (save-excursion
      (while list
	(set-buffer (car list))
	(if (or (and (stringp pattern)
		     (buffer-file-name)
		     (string-match pattern (buffer-file-name)))
		(and (symbolp pattern) major-mode (eq major-mode pattern)))
	    (eval job))
	(setq list (cdr list)))))
)

(defun goto-buffer-window (buffer)
  "Select window which is bound to BUFFER.
If no such window exist, switch to buffer BUFFER."
  (interactive "BGoto buffer: ")
  (if (stringp buffer)
      (setq buffer (or (get-file-buffer buffer) (get-buffer buffer))))
  (if (get-buffer buffer)
      (cond
       ((get-buffer-window buffer)
	(select-window (get-buffer-window buffer)))
       ((and YaTeX-emacs-19 (get-buffer-window buffer t))
	(let*((win (get-buffer-window buffer t))
	      (frame (window-frame win)))
	  (select-frame frame)
	  (raise-frame frame)
	  (focus-frame frame)
	  (select-window win)
	  (set-mouse-position frame 0 0)
	  (and (featurep 'windows) (fboundp 'win:adjust-window)
	       (win:adjust-window))))
       ((and (featurep 'windows) (fboundp 'win:get-buffer-window)
	     (let ((w (win:get-buffer-window buffer)))
	       (and w (win:switch-window w))))
	(select-window (get-buffer-window buffer)))
       (t (switch-to-buffer buffer))))
)

;; Here starts the functions which support gmhist-vs-Emacs19 compatible
;; reading with history.
;;;###autoload
(defun completing-read-with-history
  (prompt table &optional predicate must-match initial hsym)
  "Completing read with general history: gmhist, Emacs-19."
  (let ((minibuffer-history
	 (or (symbol-value hsym)
	     (and (boundp 'minibuffer-history) minibuffer-history)))
	(minibuffer-history-symbol (or hsym 'minibuffer-history)))
    (prog1
	(if (fboundp 'completing-read-with-history-in)
	    (completing-read-with-history-in
	     minibuffer-history-symbol prompt table predicate must-match initial)
	  (completing-read prompt table predicate must-match initial))
      (if (and YaTeX-emacs-19 hsym) (set hsym minibuffer-history)))))

;;;###autoload
(defun read-from-minibuffer-with-history (prompt &optional init map read hsym)
  "Read from minibuffer with general history: gmhist, Emacs-19."
  (cond
   (YaTeX-emacs-19
    (read-from-minibuffer prompt init map read hsym))
   (t
    (let ((minibuffer-history-symbol hsym))
      (read-from-minibuffer prompt init map read)))))

;;;###autoload
(defun read-string-with-history (prompt &optional init hsym)
  "Read string with history: gmhist(Emacs-18) and Emacs-19."
  (cond
   (YaTeX-emacs-19
    (read-from-minibuffer prompt init minibuffer-local-map nil hsym))
   ((featurep 'gmhist-mh)
    (read-with-history-in hsym prompt init))
   (t (read-string prompt init))))

;;;
;; Interface function for windows.el
;;;
;;;###autoload
(defun YaTeX-switch-to-window ()
  "Switch to windows.el's window decided by last pressed key."
  (interactive)
  (or (featurep 'windows) (error "Why don't you use `windows.el'?"))
  (win-switch-to-window 1 (- last-command-char win:base-key)))

(provide 'yatexlib)

;;; -*- Emacs-Lisp -*-
;;; YaTeX and yahtml common libraries, general functions and definitions
;;; yatexlib.el
;;; (c )1994-2000 by HIROSE Yuuji.[yuuji@yatex.org]
;;; Last modified Sun Apr  9 12:36:25 2000 on firestorm
;;; $Id$

;; General variables
(defvar YaTeX-dos (memq system-type '(ms-dos windows-nt OS/2)))
(defvar YaTeX-emacs-19 (>= (string-to-int emacs-version) 19))
(defvar YaTeX-emacs-20 (>= (string-to-int emacs-version) 20))
(defvar YaTeX-user-completion-table
  (if YaTeX-dos "~/_yatexrc" "~/.yatexrc")
  "*Default filename in which user completion table is saved.")

(defvar YaTeX-japan (or (boundp 'NEMACS) (boundp 'MULE) YaTeX-emacs-20)
  "Whether yatex mode is running on Japanese environment or not.")

;; autoload from yahtml.el
(autoload 'yahtml-inner-environment-but "yahtml" "yahtml internal func." t)

(defvar YaTeX-kanji-code-alist
  (cond
   ((boundp '*junet*)
    (list (cons
	   1
	   (if YaTeX-dos (if (boundp '*sjis-dos*) *sjis-dos* *sjis*dos)
	     *sjis*))
	  '(2 . *junet*) '(3 . *euc-japan*)))
   (YaTeX-emacs-20
    ;;(cdr-safe(assq 'coding-system (assoc "Japanese" language-info-alist)))
    (list (cons
	   1 (cond (YaTeX-dos 'shift_jis-dos)
		   ((member 'shift_jis (coding-system-list)) 'shift_jis-unix)
		   (t 'sjis)))
	  '(2 . iso-2022-jp-unix)
	  '(3 . euc-jp-unix))))
  "Kanji-code expression translation table.")
(defvar YaTeX-inhibit-prefix-letter nil
  "*T for changing key definitions from [prefix] Letter to [prefix] C-Letter.")

(defvar YaTeX-no-begend-shortcut nil
  "*T for disabling shortcut of begin-type completion, [prefix] b d, etc.")

(defvar YaTeX-default-pop-window-height 10
  "Default typesetting buffer height.
If integer, sets the window-height of typesetting buffer.
If string, sets the percentage of it.
If nil, use default pop-to-buffer.")

(defvar YaTeX-create-file-prefix-g nil
  "*Non-nil creates new file when [prefix] g on \\include{foo}.")

(defvar YaTeX-nervous t
  "*If you are nervous about maintenance of yatexrc, set this value to T.
And you will have the local dictionary.")

;----------- work variables ----------------------------------------
(defvar YaTeX-typesetting-mode-map nil
  "Keymap used in YaTeX typesetting buffer")

(if YaTeX-typesetting-mode-map nil
  (setq YaTeX-typesetting-mode-map (make-keymap))
  ;(suppress-keymap YaTeX-typesetting-mode-map t)
  (define-key YaTeX-typesetting-mode-map " " 'YaTeX-jump-error-line)
  (define-key YaTeX-typesetting-mode-map "\C-m" 'YaTeX-send-string)
  (define-key YaTeX-typesetting-mode-map "1" 'delete-other-windows)
  (define-key YaTeX-typesetting-mode-map "0" 'delete-window)
  (define-key YaTeX-typesetting-mode-map "q" 'delete-window))

(defvar YaTeX-parent-file nil
  "*Main LaTeX source file name used when %#! expression doesn't exist.")
(make-variable-buffer-local 'YaTeX-parent-file)

;---------- Define default key bindings on YaTeX mode map ----------
;;;###autoload
(defun YaTeX-define-key (key binding &optional map)
  "Define key on YaTeX-prefix-map."
  (if YaTeX-inhibit-prefix-letter
      (let ((c (aref key 0)))
	(cond
	 ((and (>= c ?a) (<= c ?z)) (aset key 0 (1+ (- c ?a))))
	 ((and (>= c ?A) (<= c ?Z) (numberp YaTeX-inhibit-prefix-letter))
	  (aset key 0 (1+ (- c ?A))))
	 (t nil))))
  (define-key (or map YaTeX-prefix-map) key binding))

;;;###autoload
(defun YaTeX-local-table-symbol (symbol)
  "Return the lisp symbol which keeps local completion table of SYMBOL."
  (intern (concat "YaTeX$"
		  default-directory
		  (symbol-name symbol))))

;;;###autoload
(defun YaTeX-sync-local-table (symbol)
  "Synchronize local variable SYMBOL.
Copy its corresponding directory dependent completion table to SYMBOL."
  (if (boundp (YaTeX-local-table-symbol symbol))
      (set symbol (symbol-value (YaTeX-local-table-symbol symbol)))))

(defvar YaTeX-user-table-is-read nil
  "Flag that means whether user completion table has been read or not.")
;;;###autoload
(defun YaTeX-read-user-completion-table (&optional forcetoread)
  "Append user completion table of LaTeX macros"
  (let*((user-table (expand-file-name YaTeX-user-completion-table))
	(local-table (expand-file-name (file-name-nondirectory user-table)))
	var localvar localbuf (curbuf (current-buffer)) sexp)
    (if YaTeX-user-table-is-read nil
      (message "Loading user completion table")
      (if (file-exists-p user-table) (load-file user-table)
	(message "Welcome to the field of YaTeX.  I'm glad to see you!")))
    (setq YaTeX-user-table-is-read t)
    (cond
     ((file-exists-p local-table)
      (set-buffer (setq localbuf (find-file-noselect local-table)))
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "(setq \\([^ ]+\\)" nil t)
	(setq var (intern (buffer-substring
			   (match-beginning 1) (match-end 1)))
	      localvar (YaTeX-local-table-symbol var))
	(goto-char (match-beginning 0))
	(setq sexp (buffer-substring (point)
				     (progn (forward-sexp) (point))))
	(set-buffer curbuf)
	(or (assq var (buffer-local-variables)) (make-local-variable var))
	(eval (read sexp))
	(or (and (boundp localvar)
		 (symbol-value localvar)
		 (not forcetoread))
	    (set localvar (symbol-value var)))
	(set-buffer localbuf))
      (kill-buffer localbuf)))
    (set-buffer curbuf)))

;;;###autoload
(defun YaTeX-reload-dictionary ()
  "Reload local dictionary.
Use this function after editing ./.yatexrc."
  (interactive)
  (let ((YaTeX-user-table-is-read nil))
    (YaTeX-read-user-completion-table t)))

;;;###autoload
(defun YaTeX-lookup-table (word type)
  "Lookup WORD in completion table whose type is TYPE.
This function refers the symbol tmp-TYPE-table, user-TYPE-table, TYPE-table.
Typically, TYPE is one of 'env, 'section, 'fontsize, 'singlecmd."
  (if (symbolp type) (setq type (symbol-name type)))
  (or (assoc word (symbol-value (intern (concat "tmp-" type "-table"))))
      (assoc word (symbol-value (intern (concat "user-" type "-table"))))
      (assoc word (symbol-value (intern (concat type "-table"))))))

;;;###autoload
(defun YaTeX-update-table (vallist default-table user-table local-table)
  "Update completion table if the car of VALLIST is not in current tables.
Second argument DEFAULT-TABLE is the quoted symbol of default completion
table, third argument USER-TABLE is user table which will be saved in
YaTeX-user-completion-table, fourth argument LOCAL-TABLE should have the
completion which is valid during current Emacs's session.  If you
want to make LOCAL-TABLE valid longer span (but restrict in this directory)
create the file in current directory which has the same name with
YaTeX-user-completion-table."
  (let ((car-v (car vallist)) key answer
	(file (file-name-nondirectory YaTeX-user-completion-table)))
    (cond
     ((assoc car-v (symbol-value default-table))
      nil) ;Nothing to do
     ((setq key (assoc car-v (symbol-value user-table)))
      (if (equal (cdr vallist) (cdr key)) nil
	;; if association hits, but contents differ.
	(message
	 "%s's attributes turned into %s" (car vallist) (cdr vallist))
	(set user-table (delq key (symbol-value user-table)))
	(set user-table (cons vallist (symbol-value user-table)))
	(YaTeX-update-dictionary
	 YaTeX-user-completion-table user-table "user")))
     ((setq key (assoc car-v (symbol-value local-table)))
      (if (equal (cdr vallist) (cdr key)) nil
	(message
	 "%s's attributes turned into %s" (car vallist) (cdr vallist))
	(set local-table (delq key (symbol-value local-table)))
	(set local-table (cons vallist (symbol-value local-table)))
	(set (YaTeX-local-table-symbol local-table) (symbol-value local-table))
	(YaTeX-update-dictionary file local-table)))
     ;; All of above cases, there are some completion in tables.
     ;; Then update tables.
     (t
      (if (not YaTeX-nervous)
	  (setq answer "u")
	(message
	 (cond
	  (YaTeX-japan
	   "`%s'‚Ì“o˜^æ: U)ƒ†[ƒUŽ«‘ L)ƒ[ƒJƒ‹Ž«‘ N)ƒƒ‚ƒŠ D)‚µ‚È‚¢")
	  (t
	   "Register `%s' into: U)serDic L)ocalDic N)one D)iscard"))
	 (if (> (length car-v) 23)
	     (concat (substring car-v 0 10) "..." (substring car-v -9))
	   car-v))
	(setq answer (char-to-string (read-char))))
      (cond
       ((string-match answer "uy")
	(set user-table (cons vallist (symbol-value user-table)))
	(YaTeX-update-dictionary YaTeX-user-completion-table user-table "user")
	)
       ((string-match answer "tl")
	(set local-table (cons vallist (symbol-value local-table)))
	(set (YaTeX-local-table-symbol local-table) (symbol-value local-table))
	(YaTeX-update-dictionary file local-table))
       ((string-match answer "d") nil)	;discard it
       (t (set default-table
	       (cons vallist (symbol-value default-table)))))))))

;;;###autoload
(defun YaTeX-cplread-with-learning
  (prom default-table user-table local-table
	&optional pred reqmatch init hsym)
  "Completing read with learning.
Do a completing read with prompt PROM.  Completion table is what
DEFAULT-TABLE, USER-TABLE, LOCAL table are appended in reverse order.
Note that these tables are passed by the symbol.
Optional arguments PRED, REQMATH and INIT are passed to completing-read
as its arguments PREDICATE, REQUIRE-MATCH and INITIAL-INPUT respectively.
If optional 8th argument HSYM, history symbol, is passed, use it as
history list variable."
  (YaTeX-sync-local-table local-table)
  (let*((table (append (symbol-value local-table)
		       (symbol-value user-table)
		       (symbol-value default-table)))
	(word (completing-read-with-history
	       prom table pred reqmatch init hsym)))
    (if (and (string< "" word) (not (assoc word table)))
	(YaTeX-update-table (list word) default-table user-table local-table))
    word))

;;;###autoload
(defun YaTeX-update-dictionary (file symbol &optional type)
  (let ((local-table-buf (find-file-noselect file))
	(name (symbol-name symbol))
	(value (symbol-value symbol)))
    (save-excursion
      (message "Updating %s dictionary..." (or type "local"))
      (set-buffer local-table-buf)
      (goto-char (point-max))
      (search-backward (concat "(setq " name) nil t)
      (delete-region (point) (progn (forward-sexp) (point)))
      (delete-blank-lines)
      (insert "(setq " name " '(\n")
      (mapcar '(lambda (s)
		 (insert (format "%s\n" (prin1-to-string s))))
	      value)
      (insert "))\n\n")
      (delete-blank-lines)
      (basic-save-buffer)
      (kill-buffer local-table-buf)
      (message "Updating %s dictionary...Done" (or type "local")))))

;;;###autoload
(defun YaTeX-define-begend-key-normal (key env &optional map)
  "Define short cut YaTeX-make-begin-end key."
  (YaTeX-define-key
   key
   (list 'lambda '(arg) '(interactive "P")
	 (list 'YaTeX-insert-begin-end env 'arg))
   map))

;;;###autoload
(defun YaTeX-define-begend-region-key (key env &optional map)
  "Define short cut YaTeX-make-begin-end-region key."
  (YaTeX-define-key key (list 'lambda nil '(interactive)
			      (list 'YaTeX-insert-begin-end env t)) map))

;;;###autoload
(defun YaTeX-define-begend-key (key env &optional map)
  "Define short cut key for begin type completion both for normal
and region mode.  To customize YaTeX, user should use this function."
  (YaTeX-define-begend-key-normal key env map)
  (if YaTeX-inhibit-prefix-letter nil
    (YaTeX-define-begend-region-key
     (concat (upcase (substring key 0 1)) (substring key 1)) env)))

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
		 (and (eq major-mode 'yatex-mode)
		      (YaTeX-in-verb-p (match-beginning 0)))
		 (save-excursion
		   (beginning-of-line)
		   (re-search-forward cmntrx (match-beginning 0) t)))))
    (store-match-data md)
    found))

(defun YaTeX-re-search-active-forward (regexp cmntrx &optional bound err cnt)
  "Search REGEXP backward which is not commented out by regexp CMNTRX.
See also YaTeX-search-active-forward."
  (YaTeX-search-active-forward regexp cmntrx bound err cnt 're-search-forward))

(defun YaTeX-search-active-backward (string cmntrx &optional bound err cnt)
  "Search STRING backward which is not commented out by regexp CMNTRX.
See also YaTeX-search-active-forward."
  (YaTeX-search-active-forward string cmntrx bound err cnt 'search-backward))

(defun YaTeX-re-search-active-backward (regexp cmntrx &optional bound err cnt)
  "Search REGEXP backward which is not commented out by regexp CMNTRX.
See also YaTeX-search-active-forward."
  (YaTeX-search-active-forward
   regexp cmntrx bound err cnt 're-search-backward))

;;;###autoload
(defun YaTeX-switch-to-buffer (file &optional setbuf)
  "Switch to buffer if buffer exists, find file if not.
Optional second arg SETBUF t make use set-buffer instead of switch-to-buffer."
  (interactive "Fswitch to file: ")
  (if (bufferp file)
      (setq file (buffer-file-name file))
    (and (string-match "^[^/].*/" file)
	 (eq major-mode 'yatex-mode)
	 YaTeX-search-file-from-top-directory
	 (save-excursion
	   (YaTeX-visit-main t)
	   (setq file (expand-file-name file)))))
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
	nil))))

;;;###autoload
(defun YaTeX-switch-to-buffer-other-window (file)
  "Switch to buffer if buffer exists, find file if not."
  (interactive "Fswitch to file: ")
  (and (eq major-mode 'yatex-mode)
       (stringp file)
       (string-match "^[^/].*/" file)
       YaTeX-search-file-from-top-directory
       (save-excursion
	 (YaTeX-visit-main t)
	 (setq file (expand-file-name file))))
  (if (bufferp file) (setq file (buffer-file-name file)))
  (cond
   ((get-file-buffer file)
    (switch-to-buffer-other-window (get-file-buffer file))
    t)
   ((or YaTeX-create-file-prefix-g (file-exists-p file))
    (find-file-other-window file) t)
   (t (message "%s was not found in this directory." file)
      nil)))

(defun YaTeX-get-file-buffer (file)
  "Return the FILE's buffer.
Base directory is that of main file or current directory."
  (let (dir main (cdir default-directory))
    (or (and (eq major-mode 'yatex-mode)
	     YaTeX-search-file-from-top-directory
	     (save-excursion
	       (YaTeX-visit-main t)
	       (get-file-buffer file)))
	(get-file-buffer file))))

(defun YaTeX-replace-format-sub (string format repl)
  (let ((beg (or (string-match (concat "^\\(%" format "\\)") string)
		 (string-match (concat "[^%]\\(%" format "\\)") string)))
	(len (length format)))
    (if (null beg) string ;no conversion
      (concat
       (substring string 0 (match-beginning 1)) (or repl "")
       (substring string (match-end 1))))))

;;;###autoload
(defun YaTeX-replace-format (string format repl)
  "In STRING, replace first appearance of FORMAT to REPL as if
function `format' does.  FORMAT does not contain `%'"
  (let ((ans string))
    (while (not (string=
		 ans (setq string (YaTeX-replace-format-sub ans format repl))))
      (setq ans string))
    string))

;;;###autoload
(defun YaTeX-replace-formats (string replace-list)
  (let ((list replace-list))
    (while list
      (setq string (YaTeX-replace-format
		    string (car (car list)) (cdr (car list)))
	    list (cdr list)))
    string))

;;;###autoload
(defun YaTeX-replace-format-args (string &rest args)
  "Translate the argument mark #1, #2, ... #n in the STRING into the
corresponding real arguments ARGS."
  (let ((argp 1))
    (while args
      (setq string
	    (YaTeX-replace-format string (int-to-string argp) (car args)))
      (setq args (cdr args) argp (1+ argp))))
  string)

;;;###autoload
(defun rindex (string char)
  (let ((pos (1- (length string)))(index -1))
    (while (>= pos 0)
      (cond
       ((= (aref string pos) char)
	(setq index pos) (setq pos -1))
       (t (setq pos (1- pos))))
      )
    index))

;;;###autoload
(defun point-beginning-of-line ()
  (save-excursion (beginning-of-line)(point)))

;;;###autoload
(defun point-end-of-line ()
  (save-excursion (end-of-line)(point)))


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
	    ;;(pop-to-buffer buffer)	;damn! emacs-19.30
	    (select-window (next-window nil 1))
	    (switch-to-buffer (get-buffer-create buffer))
	    (or select (select-window window)))
	   (t nil)))
	 ))))

(cond
 ((fboundp 'screen-height)
  (fset 'YaTeX-screen-height 'screen-height)
  (fset 'YaTeX-screen-width 'screen-width))
 ((fboundp 'frame-height)
  (fset 'YaTeX-screen-height 'frame-height)
  (fset 'YaTeX-screen-width 'frame-width))
 (t (error "I don't know how to run windows.el on this Emacs...")))

;;;###autoload
(defun split-window-calculate-height (height)
  "Split current window wight specified HEIGHT.
If HEIGHT is number, make a new window that has HEIGHT lines.
If HEIGHT is string, make a new window that occupies HEIGT % of screen height.
Otherwise split window conventionally."
  (if (one-window-p t)
      (split-window
       (selected-window)
       (max
	(min
	 (- (YaTeX-screen-height)
	    (if (numberp height)
		(+ height 2)
	      (/ (* (YaTeX-screen-height)
		    (string-to-int height))
		 100)))
	 (- (YaTeX-screen-height) window-min-height 1))
	window-min-height))))

;;;###autoload
(defun YaTeX-window-list ()
  (let*((curw (selected-window)) (win curw) (wlist (list curw)))
    (while (not (eq curw (setq win (next-window win))))
      (or (eq win (minibuffer-window))
	  (setq wlist (cons win wlist))))
    wlist))

;;;###autoload
(defun substitute-all-key-definition (olddef newdef keymap)
  "Replace recursively OLDDEF with NEWDEF for any keys in KEYMAP now
defined as OLDDEF. In other words, OLDDEF is replaced with NEWDEF
where ever it appears."
  (if YaTeX-emacs-19
      (substitute-key-definition olddef newdef keymap global-map)
    (mapcar
     (function (lambda (key) (define-key keymap key newdef)))
     (where-is-internal olddef keymap))))

;;;###autoload
(defun YaTeX-match-string (n &optional m)
  "Return (buffer-substring (match-beginning n) (match-beginning m))."
  (if (match-beginning n)
      (buffer-substring (match-beginning n)
			(match-end (or m n)))))

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
    (store-match-data md)))

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
	(setq list (cdr list))))))

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
       (t (switch-to-buffer buffer)))))

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

;;;###autoload
(fset 'YaTeX-rassoc
      (if (and nil (fboundp 'rassoc) (subrp (symbol-function 'rassoc)))
	  (symbol-function 'rassoc)
	(function
	 (lambda (key list)
	   (let ((l list))
	     (catch 'found
	       (while l
		 (if (equal key (cdr (car l)))
		     (throw 'found (car l)))
		 (setq l (cdr l)))))))))

(defun YaTeX-insert-file-contents (file visit &optional beg end)
  (cond
   ((string< "19" emacs-version)
    (insert-file-contents file visit beg end))
   ((string-match "unix" (symbol-name system-type))
    (let ((default-process-coding-system
	    (and (boundp '*noconv*) (list *noconv*)))
	  file-coding-system (and (boundp '*noconv*) *noconv*)
	  kanji-fileio-code
	  (default-process-kanji-code 0))
      (call-process shell-file-name file (current-buffer) nil
		    (or (and (boundp 'shell-command-option)
			     shell-command-option)
			"-c")
		    (format "head -c %d | tail -c +%d" end beg))))
    (t (insert-file-contents file))))

(defun YaTeX-split-string (str &optional sep null)
  "Split string STR by every occurrence of SEP(regexp).
If the optional second argument SEP is nil, it defaults to \"[ \f\t\n\r\v]+\".
Do not include null string by default.  Non-nil for optional third argument
NULL includes null string in a list."
  (let ((sep (or sep "[ \f\t\n\r\v]+"))
	list m)
    (while str
      (if (setq m (string-match sep str))
	  (progn
	    (if (or (> m 0) null)
		(setq list (cons (substring str 0 m) list)))
	    (setq str (substring str (match-end 0))))
	(if (or null (string< "" str))
	    (setq list (cons str list)))
	(setq str nil)))
    (nreverse list)))

;;;
;; Interface function for windows.el
;;;
;;;###autoload
(defun YaTeX-switch-to-window ()
  "Switch to windows.el's window decided by last pressed key."
  (interactive)
  (or (featurep 'windows) (error "Why don't you use `windows.el'?"))
  (win-switch-to-window 1 (- last-command-char win:base-key)))

;;;###autoload
(defun YaTeX-reindent (col)
  "Remove current indentation and reindento to COL column."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (/= col (current-column))
	(progn
	  (delete-region (point) (progn (beginning-of-line) (point)))
	  (indent-to col))))
  (skip-chars-forward " \t" (point-end-of-line)))

(defun YaTeX-inner-environment (&optional quick)
  "Return current inner-most environment.
Non-nil for optional argument QUICK restricts search bound to most
recent sectioning command.  Matching point is stored to property 'point
of 'YaTeX-inner-environment, which can be referred by
 (get 'YaTeX-inner-environment 'point)."
  (let*((nest 0)
	(beg (YaTeX-replace-format-args
	      (regexp-quote YaTeX-struct-begin)
	      ;YaTeX-struct-begin		;=== TENTATIVE!! ==
	      YaTeX-struct-name-regexp
	      (if (eq major-mode 'yahtml-mode) "\\s *.*" "")
	      ""))
	(end (YaTeX-replace-format-args
	      (regexp-quote YaTeX-struct-end)
	      YaTeX-struct-name-regexp "" ""))
	(begend (concat "\\(" beg "\\)\\|\\(" end "\\)"))
	bound m0
	(htmlp (eq major-mode 'yahtml-mode))
	(open
	 (concat "^" (or (cdr (assq major-mode '((yahtml-mode . "<")))) "{")))
	(close
	 (concat "^"
		 (or (cdr(assq major-mode '((yahtml-mode . "\n\t >")))) "}"))))
    (save-excursion
      (if quick
	  (setq bound
		(save-excursion
		  (if htmlp 
		      ;;(re-search-backward YaTeX-sectioning-regexp nil 1)
		      (goto-char (point-min)) ;Is this enough? 97/6/26
		    (YaTeX-re-search-active-backward
		     (concat YaTeX-ec-regexp
			     "\\(" YaTeX-sectioning-regexp "\\)\\*?{")
		     YaTeX-comment-prefix nil 1))
		  (or (bobp) (end-of-line))
		  (point))))
      (if (catch 'begin
	    (if (and (numberp bound) (< (point) bound)) (throw 'begin nil))
	    (while (YaTeX-re-search-active-backward
		    begend YaTeX-comment-prefix bound t)
	      (setq m0 (match-beginning 0))
	      (if (looking-at end) ;;(match-beginning 2)
		  (setq nest (1+ nest))
		(setq nest (1- nest)))
	      (if (< nest 0)
		  (progn
		    (put 'YaTeX-inner-environment 'point m0)
		    (goto-char m0)
		    (put 'YaTeX-inner-environment 'indent (current-column))
		    (throw 'begin t)))))
	  (buffer-substring
	   (progn (skip-chars-forward open) (1+ (point)))
	   (progn (skip-chars-forward close) (point)))))))

(defun YaTeX-end-environment ()
  "Close opening environment"
  (interactive)
  (let ((env (YaTeX-inner-environment)))
    (if (not env) (error "No premature environment")
      (save-excursion
	(if (YaTeX-search-active-forward
	     (YaTeX-replace-format-args YaTeX-struct-end env "" "")
	     YaTeX-comment-prefix nil t)
	    (if (y-or-n-p
		 (concat "Environment `" env
			 "' may be already closed. Force close?"))
		nil
	      (error "end environment aborted."))))
      (message "")			;Erase (y or n) message.
      (YaTeX-insert-struc 'end env)
      (save-excursion
	(goto-char (or (get 'YaTeX-inner-environment 'point) (match-end 0)))
	(if (pos-visible-in-window-p)
	    (sit-for (if YaTeX-dos 2 1))
	  (message "Matches with %s at line %d"
		   (YaTeX-replace-format-args YaTeX-struct-begin env "" "")
		   (count-lines (point-min) (point))))))))

(defun YaTeX-beginning-of-environment (&optional limit-search-bound end)
  "Goto the beginning of the current environment.
Optional argument LIMIT-SEARCH-BOUND non-nil limits the search bound to
most recent sectioning command.  Non-nil for optional third argument END
goes to end of environment."
  (interactive)
  (let ((op (point)))
    (if (YaTeX-inner-environment limit-search-bound)
	(progn
	  (goto-char (get 'YaTeX-inner-environment 'point))
	  (and end (YaTeX-goto-corresponding-environment))
	  (if (interactive-p) (push-mark op))
	  t))))

(defun YaTeX-end-of-environment (&optional limit-search-bound)
  "Goto the end of the current environment.
Optional argument LIMIT-SEARCH-BOUND non-nil limits the search bound
to most recent sectioning command."
  (interactive)
  (YaTeX-beginning-of-environment limit-search-bound t))

(defun YaTeX-mark-environment ()
  "Mark current position and move point to end of environment."
  (interactive)
  (let ((curp (point)))
    (if (and (YaTeX-on-begin-end-p) (match-beginning 1)) ;if on \\begin
	(forward-line 1)
      (beginning-of-line))
    (if (not (YaTeX-end-of-environment))   ;arg1 turns to match-beginning 1
	(progn
	  (goto-char curp)
	  (error "Cannot found the end of current environment."))
      (YaTeX-goto-corresponding-environment)
      (beginning-of-line)		;for confirmation
      (if (< curp (point))
	  (progn
	    (message "Mark this environment?(y or n): ")
	    (if (= (read-char) ?y) nil
	      (goto-char curp)
	      (error "Abort.  Please call again at more proper position."))))
      (set-mark-command nil)
      (YaTeX-goto-corresponding-environment)
      (end-of-line)
      (if (eobp) nil (forward-char 1)))))


;;;VER2
(defun YaTeX-insert-struc (what env)
  (cond
   ((eq what 'begin)
    (insert (YaTeX-replace-format-args
	     YaTeX-struct-begin env (YaTeX-addin env))))
   ((eq what 'end)
    (insert (YaTeX-replace-format-args YaTeX-struct-end env)))
   (t nil)))

;;; Function for menu support
(defun YaTeX-define-menu (keymap bindlist)
  "Define KEYMAP(symbol)'s menu-bindings according to BINDLIST.
KEYMAP should be a quoted symbol of newly allocated keymap.
BINDLIST consists of binding list.  Each element is as follows.

 '(menusymbol DOC_String . contents)

CONTENTS is one of lambda-form, interactive function, or other keymap.
See yatex19.el for example."
  (cond
   ((featurep 'xemacs)
    (let (name)
      (if (keymapp (symbol-value keymap))
	  (progn
	    (setq name (keymap-name (symbol-value keymap)))
	    (set keymap nil))
	(setq name (car (symbol-value keymap)))
	(set keymap (cdr (symbol-value keymap))))
      (mapcar
       (function
	(lambda (bind)
	  (setq bind (cdr bind))
	   (if (eq (car-safe (cdr bind)) 'lambda)
	       (setcar (cdr bind) 'progn))
	   (if (stringp (car-safe (cdr bind)))
	       (set keymap
		    (cons (cdr bind) (symbol-value keymap)))
	     (set keymap
		  (cons (vector (car bind) (cdr bind) t)
			(symbol-value keymap))))))
       bindlist)
      (set keymap (cons name (symbol-value keymap)))))
   (t
    (mapcar
     (function
      (lambda (bind)
	(define-key (symbol-value keymap) (vector (car bind)) (cdr bind))))
     bindlist))))


;;;
;; Functions for the Installation time
;;;

(defun bcf-and-exit ()
  "Byte compile rest of argument and kill-emacs."
  (if command-line-args-left
      (let ((load-path (cons "." load-path)))
	(and (fboundp 'set-language-environment)
	     (featurep 'mule)
	     (set-language-environment "Japanese"))
	(mapcar 'byte-compile-file command-line-args-left)
	(kill-emacs))))

(provide 'yatexlib)

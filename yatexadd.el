;;; -*- Emacs-Lisp -*-
;;; YaTeX add-in functions.
;;; yatexadd.el rev.9
;;; (c )1991-1994 by HIROSE Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Mon Jun 27 17:00:19 1994 on figaro
;;; $Id$

;;;
;;Sample functions for LaTeX environment.
;;;
(defvar YaTeX:tabular-default-rule
  "@{\\vrule width 1pt\\ }c|c|c@{\\ \\vrule width 1pt}"
  "*Your favorite default rule format."
)
(defvar YaTeX:tabular-thick-vrule "\\vrule width %s"
  "*Vertical thick line format (without @{}).  %s'll be replaced by its width."
)
(defvar YaTeX:tabular-thick-hrule "\\noalign{\\hrule height %s}"
  "*Horizontal thick line format.  %s will be replaced by its width."
)
(defun YaTeX:tabular ()
  "YaTeX add-in function for tabular environment.
Notice that this function refers the let-variable `env' in
YaTeX-make-begin-end."
  (let ((width "") bars (rule "") (and "") (j 1) loc ans (hline "\\hline"))
    (if (string= env "tabular*")
	(setq width (concat "{" (read-string "Width: ") "}")))
    (setq loc (YaTeX:read-position "tb")
	  bars (string-to-int
		(read-string "Number of columns(0 for default format): " "3")))
    (if (<= bars 0)
	(setq				;if 0, simple format
	 rule YaTeX:tabular-default-rule
	 and "& &")
      (while (< j bars)			;repeat bars-1 times
	(setq rule (concat rule "c|")
	      and (concat and "& ")
	      j (1+ j)))
      (setq rule (concat rule "c"))
      (message "(N)ormal-frame or (T)hick frame? [nt]")
      (setq ans (read-char))
      (cond
       ((or (equal ans ?t) (equal ans ?T))
	(setq ans (read-string "Rule width: " "1pt")
	      rule (concat
		    "@{" (format YaTeX:tabular-thick-vrule ans) "}"
		    rule
		    "@{\\ " (format YaTeX:tabular-thick-vrule ans) "}")
	      hline (format YaTeX:tabular-thick-hrule ans)))
       (t (setq rule (concat "|" rule "|")
		hline "\\hline"))))

    (setq rule (read-string "rule format: " rule))

    (message "Dont forget to remove null line at the end of tabular.")
    (format "%s%s{%s}%s"
	    width loc rule
	    (if (and (boundp 'region-mode) region-mode)
		""		;do nothing in region-mode
	      (format "\n%s\n%s \\\\ \\hline\n%s\n\\\\ %s"
		      hline and and hline))))
)
(fset 'YaTeX:tabular* 'YaTeX:tabular)
(defun YaTeX:array ()
  (concat (YaTeX:read-position "tb")
	  "{" (read-string "Column format: ") "}")
)

(defun YaTeX:read-oneof (oneof)
  (let ((pos "") loc (guide ""))
    (and (boundp 'name) name (setq guide (format "%s " name)))
    (while (not (string-match
		 (setq loc (read-key-sequence
			    (format "%s position (`%s') [%s]: "
				    guide oneof pos)));name is in YaTeX-addin
		 "\r\^g\n"))
      (cond
       ((string-match loc oneof)
	(if (not (string-match loc pos))
	    (setq pos (concat pos loc))))
       ((and (string-match loc "\C-h\C-?") (> (length pos) 0))
	(setq pos (substring pos 0 (1- (length pos)))))
       (t
	(ding)
	(message "Please input one of `%s'." oneof)
	(sit-for 3))))
    (message "")
    pos)
)

(defun YaTeX:read-position (oneof)
  "Read a LaTeX (optional) position format such as `[htbp]'."
  (let ((pos (YaTeX:read-oneof oneof)))
    (if (string= pos "")  "" (concat "[" pos "]")))
)

(defun YaTeX:table ()
  "YaTeX add-in function for table environment."
  (YaTeX:read-position "htbp")
)

(defun YaTeX:description ()
  "Truly poor service:-)"
  (setq single-command "item[]")
  ""
)

(defun YaTeX:itemize ()
  "It's also poor service."
  (setq single-command "item")
  ""
)

(fset 'YaTeX:enumerate 'YaTeX:itemize)

(defun YaTeX:picture ()
  "Ask the size of coordinates of picture environment."
  (concat (YaTeX:read-coordinates "Picture size")
	  (YaTeX:read-coordinates "Initial position"))
)

(defun YaTeX:equation ()
  (if (fboundp 'YaTeX-toggle-math-mode)
      (YaTeX-toggle-math-mode t))		;force math-mode ON.
)
(fset 'YaTeX:eqnarray 'YaTeX:equation)
(fset 'YaTeX:displaymath 'YaTeX:equation)

(defun YaTeX:list ()
  "%\n{} %default label\n{} %formatting parameter"
)

(defun YaTeX:minipage ()
  (concat (YaTeX:read-position "cbt")
	  "{" (read-string "Width: ") "}")
)

;;;
;;Sample functions for section-type command.
;;;
(defun YaTeX:multiput ()
  (concat (YaTeX:read-coordinates "Pos")
	  (YaTeX:read-coordinates "Step")
	  "{" (read-string "How many times: ") "}")
)

(defun YaTeX:put ()
  (YaTeX:read-coordinates "Pos")
)

(defun YaTeX:makebox ()
  (cond
   ((YaTeX-in-environment-p "picture")
    (concat (YaTeX:read-coordinates "Dimension")
	    (YaTeX:read-position "lrtb")))
   (t
    (let ((width (read-string "Width: ")))
      (if (string< "" width)
	  (progn
	    (or (equal (aref width 0) ?\[)
		(setq width (concat "[" width "]")))
	    (concat width (YaTeX:read-position "lr")))))))
)

(defun YaTeX:framebox ()
  (if (YaTeX-quick-in-environment-p "picture")
      (YaTeX:makebox))
)

(defun YaTeX:dashbox ()
  (concat "{" (read-string "Dash dimension: ") "}"
	  (YaTeX:read-coordinates "Dimension"))
)

(defun YaTeX:left ()
  (let (c)
    (while (not (string-match
		 (progn (message "Which parenthesis? One of [{(|)}]: ")
			(setq c (regexp-quote (char-to-string (read-char)))))
		 "[{(|)}]")))
    (setq single-command "right")
    (cond
     ((string-match c "[(|)]") c)
     (t (concat "\\" c))))
)
(fset 'YaTeX:right 'YaTeX:left)

(defun YaTeX:read-coordinates (&optional mes varX varY)
  (concat
   "("
   (read-string (format "%s %s: " (or mes "Dimension") (or varX "X")))
   ","
   (read-string (format "%s %s: " (or mes "Dimension") (or varY "Y")))
   ")")
)

;;;
;;Sample functions for maketitle-type command.
;;;
(defun YaTeX:sum ()
  "Read range of summation."
  (YaTeX:check-completion-type 'maketitle)
  (concat (YaTeX:read-boundary "_") (YaTeX:read-boundary "^"))
)

(fset 'YaTeX:int 'YaTeX:sum)

(defun YaTeX:lim ()
  "Insert limit notation of \\lim."
  (YaTeX:check-completion-type 'maketitle)
  (let ((var (read-string "Variable: ")) limit)
    (if (string= "" var) ""
      (setq limit (read-string "Limit ($ means infinity): "))
      (if (string= "$" limit) (setq limit "\\infty"))
      (concat "_{" var " \\rightarrow " limit "}")))
)

(defun YaTeX:gcd ()
  "Add-in function for \\gcd(m,n)."
  (YaTeX:check-completion-type 'maketitle)
  (YaTeX:read-coordinates "\\gcd" "(?,)" "(,?)")
)

(defun YaTeX:read-boundary (ULchar)
  "Read boundary usage by _ or ^.  _ or ^ is indicated by argument ULchar."
  (let ((bndry (read-string (concat ULchar "{???} ($ for infinity): "))))
    (if (string= bndry "") ""
      (if (string= bndry "$") (setq bndry "\\infty"))
      (concat ULchar "{" bndry "}")))
)

(defun YaTeX:verb ()
  "Enclose \\verb's contents with the same characters."
  (let ((quote-char (read-string "Quoting char: " "|"))
	(contents (read-string "Quoted contents: ")))
    (concat quote-char contents quote-char))
)
(fset 'YaTeX:verb* 'YaTeX:verb)

;;;
;;Subroutine
;;;

(defun YaTeX:check-completion-type (type)
  "Check valid completion type."
  (if (not (eq type YaTeX-current-completion-type))
      (error "This should be completed with %s-type completion." type))
)


;;;
;;;		[[Add-in functions for reading section arguments]]
;;;
;; All of add-in functions for reading sections arguments should
;; take an argument ARGP that specify the argument position.
;; If argument position is out of range, nil should be returned,
;; else nil should NOT be returned.

;;
; Label selection
;;
(defvar YaTeX-label-menu-other
  (if YaTeX-japan "':他のバッファのラベル\n" "':LABEL IN OTHER BUFFER.\n"))
(defvar YaTeX-label-menu-repeat
  (if YaTeX-japan ".:直前の\\refと同じ\n" "/:REPEAT LAST \ref{}\n"))
(defvar YaTeX-label-menu-any
  (if YaTeX-japan "*:任意の文字列\n" "*:ANY STRING.\n"))
(defvar YaTeX-label-buffer "*Label completions*")
(defvar YaTeX-label-guide-msg "Select label and hit RETURN.")
(defvar YaTeX-label-select-map nil
  "Key map used in label selection buffer.")
(defun YaTeX::label-setup-key-map ()
  (if YaTeX-label-select-map nil
    (message "Setting up label selection mode map...")
    (setq YaTeX-label-select-map (copy-keymap global-map))
    (suppress-keymap YaTeX-label-select-map)
    (substitute-all-key-definition
     'previous-line 'YaTeX::label-previous YaTeX-label-select-map)
    (substitute-all-key-definition
     'next-line 'YaTeX::label-next YaTeX-label-select-map)
    (define-key YaTeX-label-select-map "\C-n"	'YaTeX::label-next)
    (define-key YaTeX-label-select-map "\C-p"	'YaTeX::label-previous)
    (define-key YaTeX-label-select-map "<"	'beginning-of-buffer)
    (define-key YaTeX-label-select-map ">"	'end-of-buffer)
    (define-key YaTeX-label-select-map "\C-m"	'exit-recursive-edit)
    (define-key YaTeX-label-select-map "\C-j"	'exit-recursive-edit)
    (define-key YaTeX-label-select-map " "	'exit-recursive-edit)
    (define-key YaTeX-label-select-map "\C-g"	'abort-recursive-edit)
    (define-key YaTeX-label-select-map "/"	'isearch-forward)
    (define-key YaTeX-label-select-map "?"	'isearch-backward)
    (define-key YaTeX-label-select-map "'"	'YaTeX::label-search-tag)
    (define-key YaTeX-label-select-map "."	'YaTeX::label-search-tag)
    (define-key YaTeX-label-select-map "*"	'YaTeX::label-search-tag)
    (message "Setting up label selection mode map...Done")
    (let ((key ?A))
      (while (<= key ?Z)
	(define-key YaTeX-label-select-map (char-to-string key)
	  'YaTeX::label-search-tag)
	(define-key YaTeX-label-select-map (char-to-string (+ key (- ?a ?A)))
	  'YaTeX::label-search-tag)
	(setq key (1+ key)))))
)
(defun YaTeX::label-next ()
  (interactive) (forward-line 1) (message YaTeX-label-guide-msg))
(defun YaTeX::label-previous ()
  (interactive) (forward-line -1) (message YaTeX-label-guide-msg))
(defun YaTeX::label-search-tag ()
  (interactive)
  (let ((case-fold-search t) (tag (regexp-quote (this-command-keys))))
    (cond
     ((save-excursion
	(forward-char 1)
	(re-search-forward (concat "^" tag) nil t))
      (goto-char (match-beginning 0)))
     ((save-excursion
	(goto-char (point-min))
	(re-search-forward (concat "^" tag) nil t))
      (goto-char (match-beginning 0))))
    (message YaTeX-label-guide-msg))
)
(defun YaTeX::ref (argp)
  (cond
   ((= argp 1)
    (save-excursion
      (let ((lnum 0) e0 m1 e1 label label-list (buf (current-buffer))
	    (p (point)) initl line)
	(goto-char (point-min))
	(message "Collecting labels...")
	(save-window-excursion
	  (YaTeX-showup-buffer
	   YaTeX-label-buffer (function (lambda (x) (window-width x))))
	  (with-output-to-temp-buffer YaTeX-label-buffer
	    (while (re-search-forward "\\label{\\([^}]+\\)}" nil t)
	      (setq e0 (match-end 0) m1 (match-beginning 1) e1 (match-end 1))
	      (if (search-backward
		   YaTeX-comment-prefix (point-beginning-of-line) t) nil
		(setq label (buffer-substring m1 e1)
		      label-list (cons label label-list))
		(or initl
		    (if (< p (point)) (setq initl lnum)))
		(beginning-of-line)
		(skip-chars-forward " \t\n" nil)
		(princ (format "%c:{%s}\t<<%s>>\n" (+ (% lnum 26) ?A) label
			       (buffer-substring (point) (point-end-of-line))))
		(setq lnum (1+ lnum))
		(message "Collecting \\label{}... %d" lnum))
	      (goto-char e0))
	    (princ YaTeX-label-menu-other)
	    (princ YaTeX-label-menu-repeat)
	    (princ YaTeX-label-menu-any)
	    );with
	  (goto-char p)
	  (message "Collecting labels...Done")
	  (pop-to-buffer YaTeX-label-buffer)
	  (YaTeX::label-setup-key-map)
	  (setq truncate-lines t)
	  (setq buffer-read-only t)
	  (use-local-map YaTeX-label-select-map)
	  (message YaTeX-label-guide-msg)
	  (goto-line (or initl lnum))	;goto recently defined label line
	  (unwind-protect
	      (progn
		(recursive-edit)
		(set-buffer (get-buffer YaTeX-label-buffer)) ;assertion
		(beginning-of-line)
		(setq line (count-lines (point-min)(point)))
		(cond
		 ((= line lnum) (setq label (YaTeX-label-other)))
		 ((= line (1+ lnum))
		  (save-excursion
		    (switch-to-buffer buf)
		    (goto-char p)
		    (if (re-search-backward "\\\\ref{\\([^}]+\\)}" nil t)
			(setq label (buffer-substring
				     (match-beginning 1) (match-end 1)))
		      (setq label ""))))
		 ((>= line (+ lnum 2))
		  (setq label (read-string "\\ref{???}: ")))
		 (t (setq label (nth (- lnum line 1) label-list)))))
	    (bury-buffer YaTeX-label-buffer)))
	label
	))
    ))
)
(fset 'YaTeX::pageref 'YaTeX::ref)

(defun YaTeX-label-other ()
  (let ((lbuf "*YaTeX mode buffers*") (blist (buffer-list)) (lnum -1) buf rv
	(ff "**find-file**"))
    (YaTeX-showup-buffer
     lbuf (function (lambda (x) 1)))	;;Select next window surely.
    (with-output-to-temp-buffer lbuf
      (while blist
	(if (and (buffer-file-name (setq buf (car blist)))
		 (progn (set-buffer buf) (eq major-mode 'yatex-mode)))
	    (princ
	     (format "%c:{%s}\n" (+ (% (setq lnum (1+ lnum)) 26) ?A)
		     (buffer-name buf))))
	(setq blist (cdr blist)))
      (princ (format "':{%s}" ff)))
    (pop-to-buffer lbuf)
    (YaTeX::label-setup-key-map)
    (setq buffer-read-only t)
    (use-local-map YaTeX-label-select-map)
    (message YaTeX-label-guide-msg)
    (unwind-protect
	(progn
	  (recursive-edit)
	  (set-buffer lbuf)
	  (beginning-of-line)
	  (setq rv
		(if (re-search-forward "{\\([^\\}]+\\)}" (point-end-of-line) t)
		    (buffer-substring (match-beginning 1) (match-end 1)) nil)))
      (kill-buffer lbuf))
    (cond
     ((null rv) "")
     ((string= rv ff)
      (call-interactively 'find-file)
      (YaTeX::ref argp))
     (t
      (set-buffer rv)
      (YaTeX::ref argp)))
    )
)

;;
; completion for the arguments of \newcommand
;;
(defun YaTeX::newcommand (&optional argp)
  (cond
   ((= argp 1)
    (let ((command (read-string "Define newcommand: " "\\")))
      (put 'YaTeX::newcommand 'command (substring command 1))
      command))
   ((= argp 2)
    (let ((argc
	   (string-to-int (read-string "Number of arguments(Default 0): ")))
	  (def (read-string "Definition: "))
	  (command (get 'YaTeX::newcommand 'command)))
      ;;!!! It's illegal to insert string in the add-in function !!!
      (if (> argc 0) (insert (format "[%d]" argc)))
      (if (and (stringp command)
	       (string< "" command)
	       (y-or-n-p "Update user completion table?"))
	  (cond
	   ((= argc 0)
	    (YaTeX-update-table
	     (list command)
	     'singlecmd-table 'user-singlecmd-table 'tmp-singlecmd-table))
	   ((= argc 1)
	    (YaTeX-update-table
	     (list command)
	     'section-table 'user-section-table 'tmp-section-table))
	   (t (YaTeX-update-table
	       (list command argc)
	       'section-table 'user-section-table 'tmp-section-table))))
      (message "")
      def				;return command name
      ))
   (t ""))
)

;;
; completion for the arguments of \pagestyle
;;
(defun YaTeX::pagestyle (&optional argp)
  "Read the pagestyle with completion."
  (completing-read
   "Page style: "
   '(("plain") ("empty") ("headings") ("myheadings") ("normal") nil))
)
;;
; completion for the arguments of \pagenumbering
;;
(defun YaTeX::pagenumbering (&optional argp)
  "Read the numbering style."
  (completing-read
   "Page numbering style: "
   '(("arabic") ("Alpha") ("alpha") ("Roman") ("roman")))
)

;;
; Length
;;
(defvar YaTeX:style-parameters-default
  '(("\\arraycolsep")
    ("\\arrayrulewidth")
    ("\\baselineskip")
    ("\\columnsep")
    ("\\columnseprule")
    ("\\doublerulesep")
    ("\\evensidemargin")
    ("\\footheight")
    ("\\footskip")
    ("\\headheight")
    ("\\headsep")
    ("\\itemindent")
    ("\\itemsep")
    ("\\labelsep")
    ("\\labelwidth")
    ("\\leftmargin")
    ("\\linewidth")
    ("\\listparindent")
    ("\\marginparsep")
    ("\\marginparwidth")
    ("\\mathindent")
    ("\\oddsidemargin")
    ("\\parindent")
    ("\\parsep")
    ("\\parskip")
    ("\\partopsep")
    ("\\rightmargin")
    ("\\tabcolsep")
    ("\\textheight")
    ("\\textwidth")
    ("\\topmargin")
    ("\\topsep")
    ("\\topskip")
    )
  "Alist of LaTeX style parameters.")
(defvar YaTeX:style-parameters-private nil
  "*User definable alist of style parameters.")
(defvar YaTeX:style-parameters-private nil
  "Holds the union of LaTeX style parameters.")
(setq YaTeX:style-parameters
      (append YaTeX:style-parameters-private YaTeX:style-parameters-default))

(defvar YaTeX:length-history nil "Holds history of length.")
(defun YaTeX::setlength (&optional argp)
  "YaTeX add-in function for arguments of \\setlength."
  (cond
   ((equal 1 argp)
    (completing-read "Length variable: " YaTeX:style-parameters nil nil "\\"))
   ((equal 2 argp)
    (let ((minibuffer-history-symbol 'YaTeX:length-history))
      (read-string "Length: "))))
)
(fset 'YaTeX::addtolength 'YaTeX::setlength)

(defun YaTeX::settowidth (&optional argp)
  "YaTeX add-in function for arguments of \\settowidth."
  (cond
   ((equal 1 argp)
    (completing-read "Length variable: " YaTeX:style-parameters nil nil "\\"))
   ((equal 2 argp)
    (read-string "Text: ")))
)
(defun YaTeX::newlength (&optional argp)
  "YaTeX add-in function for arguments of \\newlength"
  (cond
   ((equal argp 1)
    (let ((length (read-string "Length variable: " "\\")))
      (or (assoc length YaTeX:style-parameters-private)
	  (setq YaTeX:style-parameters-private
		(cons (list length) YaTeX:style-parameters-private)
		YaTeX:style-parameters
		(cons (list length) YaTeX:style-parameters)))
      length)))
)

;; \multicolumn's arguments
(defun YaTeX::multicolumn (&optional argp)
  "YaTeX add-in function for arguments of \\multicolumn."
  (cond
   ((equal 1 argp)
    (read-string "Number of columns: "))
   ((equal 2 argp)
    (let (c)
      (while (not (string-match
		   (progn (message "Format(one of l,r,c): ")
			  (setq c (char-to-string (read-char))))
		   "lrc")))
      c))
   ((equal 3 argp)
    (read-string "Item: ")))
)

;;; -------------------- End of yatexadd --------------------
(provide 'yatexadd)

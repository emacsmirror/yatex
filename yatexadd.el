;;; -*- Emacs-Lisp -*-
;;; YaTeX add-in functions.
;;; yatexadd.el rev.13
;;; (c )1991-1997 by HIROSE Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Fri Jan 24 18:00:45 1997 on supra
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
    (setq single-command "hline")

    (format "%s%s{%s}" width loc rule))
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
  (setq env-name "tabular")
  (YaTeX:read-position "htbp")
)

(fset 'YaTeX:figure 'YaTeX:table)
(fset 'YaTeX:figure* 'YaTeX:table)


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
  (YaTeX-jmode-off)
  (if (fboundp 'YaTeX-toggle-math-mode)
      (YaTeX-toggle-math-mode t))		;force math-mode ON.
)
(mapcar '(lambda (f) (fset f 'YaTeX:equation))
	'(YaTeX:eqnarray YaTeX:eqnarray* YaTeX:align YaTeX:align*
	  YaTeX:split YaTeX:multline YaTeX:multline* YaTeX:gather YaTeX:gather*
	  YaTeX:aligned* YaTeX:gathered YaTeX:gathered*
	  YaTeX:alignat YaTeX:alignat* YaTeX:xalignat YaTeX:xalignat*
	  YaTeX:xxalignat YaTeX:xxalignat*))

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

(defvar YaTeX-minibuffer-quick-map nil)
(if YaTeX-minibuffer-quick-map nil
  (setq YaTeX-minibuffer-quick-map
	(copy-keymap minibuffer-local-completion-map))
  (let ((ch (1+ ? )))
    (while (< ch 127)
      (define-key YaTeX-minibuffer-quick-map (char-to-string ch)
	'YaTeX-minibuffer-quick-complete)
      (setq ch (1+ ch)))))

(defvar YaTeX:left-right-delimiters
   '(("(" . ")") (")" . "(") ("[" . "]") ("]" . "[")
     ("\\{" . "\\}") ("\\}" . "\\{") ("|") ("\\|")
     ("\\lfloor" . "\\rfloor") ("\\lceil" . "\\rceil")
     ("\\langle" . "\\rangle") ("/") (".")
     ("\\rfloor" . "\\rfloor") ("\\rceil" . "\\lceil")
     ("\\rangle" . "\\langle") ("\\backslash")
     ("\\uparrow") ("\\downarrow") ("\\updownarrow") ("\\Updownarrow"))
   "TeX math delimiter, which can be completed after \\right or \\left.")

(defvar YaTeX:left-right-default nil "Default string of YaTeX:right.")

(defun YaTeX:left ()
  (let ((minibuffer-completion-table YaTeX:left-right-delimiters)
	delimiter (leftp (string= single-command "left")))
    (setq delimiter
	  (read-from-minibuffer
	   (format "Delimiter%s: "
		   (if YaTeX:left-right-default
		       (format "(default=`%s')" YaTeX:left-right-default)
		     "(SPC for menu)"))
	   nil YaTeX-minibuffer-quick-map))
    (if (string= "" delimiter) (setq delimiter YaTeX:left-right-default))
    (setq single-command (if leftp "right" "left")
	  YaTeX:left-right-default
	  (or (cdr (assoc delimiter YaTeX:left-right-delimiters)) delimiter))
    delimiter))

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

(defun YaTeX:footnotemark ()
  (setq section-name "footnotetext")
  nil
)

(defun YaTeX:cite ()
  (let ((comment (read-string "Comment for citation: ")))
    (if (string= comment "") ""
      (concat "[" comment "]")))
)

(defun YaTeX:bibitem ()
  (let ((label (read-string "Citation label: ")))
    (if (string= label "") ""
      (concat "[" label "]")))
)

(defun YaTeX:item ()
  (YaTeX-indent-line)
  (setq section-name "label")
  " ")
(fset 'YaTeX:item\[\] 'YaTeX:item)
(fset 'YaTeX:subitem 'YaTeX:item)
(fset 'YaTeX:subsubitem 'YaTeX:item)

(defun YaTeX:linebreak ()
  (let (obl)
    (message "Break strength 0,1,2,3,4 (default: 4): ")
    (setq obl (char-to-string (read-char)))
    (if (string-match "[0-4]" obl)
	(concat "[" obl "]")
      ""))
)
(fset 'YaTeX:pagebreak 'YaTeX:linebreak)

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
(defun YaTeX::ref (argp &optional labelcmd refcmd)
  (cond
   ((= argp 1)
    (let ((lnum 0) e0 label label-list (buf (current-buffer))
	  (labelcmd (or labelcmd "label")) (refcmd (or refcmd "ref"))
	  (p (point)) initl line cf)
      (message "Collecting labels...")
      (save-window-excursion
	(YaTeX-showup-buffer
	 YaTeX-label-buffer (function (lambda (x) (window-width x))))
	(if (fboundp 'select-frame) (setq cf (selected-frame)))
	(if (eq (window-buffer (minibuffer-window)) buf)
	    (progn
	      (other-window 1)
	      (setq buf (current-buffer))
	      (set-buffer buf)
	      ;(message "cb=%s" buf)(sit-for 3)
	      ))
	(save-excursion
	  (goto-char (point-min))
	  (with-output-to-temp-buffer YaTeX-label-buffer
	    (princ (format "=== LABELS in [%s] ===\n" (buffer-name buf)))
	    (while (YaTeX-re-search-active-forward
		    (concat "\\\\" labelcmd "\\b")
		    (regexp-quote YaTeX-comment-prefix) nil t)
	      (goto-char (match-beginning 0))
	      (skip-chars-forward "^{")
	      (setq label
		    (buffer-substring
		     (1+ (point))
		     (prog2 (forward-list 1) (setq e0 (1- (point)))))
		    label-list (cons label label-list))
	      (or initl
		  (if (< p (point)) (setq initl lnum)))
	      (beginning-of-line)
	      (skip-chars-forward " \t\n" nil)
	      (princ (format "%c:{%s}\t<<%s>>\n" (+ (% lnum 26) ?A) label
			     (buffer-substring (point) (point-end-of-line))))
	      (setq lnum (1+ lnum))
	      (message "Collecting \\%s{}... %d" labelcmd lnum)
	      (goto-char e0))
	    (princ YaTeX-label-menu-other)
	    (princ YaTeX-label-menu-repeat)
	    (princ YaTeX-label-menu-any)
	    );with
	  (goto-char p)
	  (or initl (setq initl lnum))
	  (message "Collecting %s...Done" labelcmd)
	  (if (fboundp 'select-frame) (select-frame cf))
	  (YaTeX-showup-buffer YaTeX-label-buffer nil t)
	  (YaTeX::label-setup-key-map)
	  (setq truncate-lines t)
	  (setq buffer-read-only t)
	  (use-local-map YaTeX-label-select-map)
	  (message YaTeX-label-guide-msg)
	  (goto-line (1+ initl)) ;goto recently defined label line
	  (unwind-protect
	      (progn
		(recursive-edit)
		(set-buffer (get-buffer YaTeX-label-buffer)) ;assertion
		(beginning-of-line)
		(setq line (1- (count-lines (point-min)(point))))
		(cond
		 ((= line -1) (setq label ""))
		 ((= line lnum) (setq label (YaTeX-label-other)))
		 ((= line (1+ lnum))
		  (save-excursion
		    (switch-to-buffer buf)
		    (goto-char p)
		    (if (re-search-backward
			 (concat "\\\\" refcmd "{\\([^}]+\\)}") nil t)
			(setq label (YaTeX-match-string 1))
		      (setq label ""))))
		 ((>= line (+ lnum 2))
		  (setq label (read-string (format "\\%s{???}: " refcmd))))
		 (t (setq label (nth (- lnum line 1) label-list)))))
	    (bury-buffer YaTeX-label-buffer)))
	label
	))
    ))
)
(fset 'YaTeX::pageref 'YaTeX::ref)
(defun YaTeX::cite (argp)
  (cond
   ((eq argp 1)
    (YaTeX::ref argp "bibitem\\(\\[.*\\]\\)?" "cite"))
   (t nil)))

(defun YaTeX-yatex-buffer-list ()
  (save-excursion
    (delq nil (mapcar (function (lambda (buf)
				  (set-buffer buf)
				  (if (eq major-mode 'yatex-mode) buf)))
		      (buffer-list))))
)

(defun YaTeX-select-other-yatex-buffer ()
  "Select buffer from all yatex-mode's buffers interactivelly."
  (interactive)
  (let ((lbuf "*YaTeX mode buffers*") (blist (YaTeX-yatex-buffer-list))
	(lnum -1) buf rv
	(ff "**find-file**"))
    (YaTeX-showup-buffer
     lbuf (function (lambda (x) 1)))	;;Select next window surely.
    (with-output-to-temp-buffer lbuf
      (while blist
	(princ
	 (format "%c:{%s}\n" (+ (% (setq lnum (1+ lnum)) 26) ?A)
		 (buffer-name (car blist))))
	(setq blist (cdr blist)))
      (princ (format "':{%s}" ff)))
    (YaTeX-showup-buffer lbuf nil t)
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
    (if (string= rv ff)
	(progn
	  (call-interactively 'find-file)
	  (current-buffer))
      rv))
)

(defun YaTeX-label-other ()
  (let ((rv (YaTeX-select-other-yatex-buffer)))
    (cond
     ((null rv) "")
     (t
      (set-buffer rv)
      (YaTeX::ref argp labelcmd refcmd)))
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
	       (y-or-n-p "Update dictionary?"))
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
(fset 'YaTeX::thispagestyle 'YaTeX::pagestyle)

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
(defvar YaTeX:style-parameters-local nil
  "*User definable alist of local style parameters.")

(defvar YaTeX:length-history nil "Holds history of length.")
(put 'YaTeX:length-history 'no-default t)
(defun YaTeX::setlength (&optional argp)
  "YaTeX add-in function for arguments of \\setlength."
  (cond
   ((equal 1 argp)
    ;;(completing-read "Length variable: " YaTeX:style-parameters nil nil "\\")
    (YaTeX-cplread-with-learning
     "Length variable: "
     'YaTeX:style-parameters-default
     'YaTeX:style-parameters-private
     'YaTeX:style-parameters-local
     nil nil "\\")
    )
   ((equal 2 argp)
    (read-string-with-history "Length: " nil 'YaTeX:length-history)))
)
(fset 'YaTeX::addtolength 'YaTeX::setlength)

(defun YaTeX::settowidth (&optional argp)
  "YaTeX add-in function for arguments of \\settowidth."
  (cond
   ((equal 1 argp)
    (YaTeX-cplread-with-learning
     "Length variable: "
     'YaTeX:style-parameters-default
     'YaTeX:style-parameters-private
     'YaTeX:style-parameters-local
     nil nil "\\"))
   ((equal 2 argp)
    (read-string "Text: ")))
)
(defun YaTeX::newlength (&optional argp)
  "YaTeX add-in function for arguments of \\newlength"
  (cond
   ((equal argp 1)
    (let ((length (read-string "Length variable: " "\\")))
      (if (string< "" length)
	  (YaTeX-update-table
	   (list length)
	   'YaTeX:style-parameters-default
	   'YaTeX:style-parameters-private
	   'YaTeX:style-parameters-local))
      length)
    ))
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

(defvar YaTeX:documentstyles-default
  '(("article") ("jarticle") ("j-article")
    ("book") ("jbook") ("j-book")
    ("report") ("jreport") ("j-report")
    ("letter") ("ascjletter"))
  "List of LaTeX documentstyles.")
(defvar YaTeX:documentstyles-private nil
  "*User defined list of LaTeX documentstyles.")
(defvar YaTeX:documentstyles-local nil
  "*User defined list of local LaTeX documentstyles.")
(defvar YaTeX:documentstyle-options-default
  '(("a4j") ("a5j") ("b4j") ("b5j")
    ("twocolumn") ("jtwocolumn") ("epsf") ("epsfig") ("epsbox") ("nfig"))
  "List of LaTeX documentstyle options.")
(defvar YaTeX:documentstyle-options-private nil
  "*User defined list of LaTeX documentstyle options.")
(defvar YaTeX:documentstyle-options-local nil
  "List of LaTeX local documentstyle options.")

(defvar YaTeX-minibuffer-completion-map nil
  "Minibuffer completion key map that allows comma completion.")
(if YaTeX-minibuffer-completion-map nil
  (setq YaTeX-minibuffer-completion-map
	(copy-keymap minibuffer-local-completion-map))
  (define-key YaTeX-minibuffer-completion-map " "
    'YaTeX-minibuffer-complete)
  (define-key YaTeX-minibuffer-completion-map "\t"
    'YaTeX-minibuffer-complete))

(defun YaTeX:documentstyle ()
  (let*((delim ",")
	(dt (append YaTeX:documentstyle-options-local
		    YaTeX:documentstyle-options-private
		    YaTeX:documentstyle-options-default))
	(minibuffer-completion-table dt)
	(opt (read-from-minibuffer
	      "Style options ([opt1,opt2,...]): "
	      nil YaTeX-minibuffer-completion-map nil))
	(substr opt) o)
    (if (string< "" opt)
	(progn
	  (while substr
	    (setq o (substring substr 0 (string-match delim substr)))
	    (or (assoc o dt)
		(YaTeX-update-table
		 (list o)
		 'YaTeX:documentstyle-options-default
		 'YaTeX:documentstyle-options-private
		 'YaTeX:documentstyle-options-local))
	    (setq substr
		  (if (string-match delim substr)
		      (substring substr (1+ (string-match delim substr))))))
	  (concat "[" opt "]"))
      "")))

(defun YaTeX::documentstyle (&optional argp)
  "YaTeX add-in function for arguments of \\documentstyle."
  (cond
   ((equal argp 1)
    (setq env-name "document")
    (let ((sname
	   (YaTeX-cplread-with-learning
	    (format "Documentstyle (default %s): "
		    YaTeX-default-document-style)
	    'YaTeX:documentstyles-default
	    'YaTeX:documentstyles-private
	    'YaTeX:documentstyles-local)))
      (if (string= "" sname) (setq sname YaTeX-default-document-style))
      (setq YaTeX-default-document-style sname))))
)

;;; -------------------- LaTeX2e stuff --------------------
(defvar YaTeX:documentclass-options-default
  '(("a4paper") ("a5paper") ("b5paper") ("10pt") ("11pt") ("12pt")
    ("latterpaper") ("legalpaper") ("executivepaper") ("landscape")
    ("oneside") ("twoside") ("draft") ("final") ("leqno") ("fleqn") ("openbib")
    ("clock")				;for slides class only
    )
    "Default options list for documentclass")
(defvar YaTeX:documentclass-options-private nil
  "*User defined options list for documentclass")
(defvar YaTeX:documentclass-options-local nil
  "*User defined options list for local documentclass")

(defun YaTeX:documentclass ()
  (let*((delim ",")
	(dt (append YaTeX:documentclass-options-local
		    YaTeX:documentclass-options-private
		    YaTeX:documentclass-options-default))
	(minibuffer-completion-table dt)
	(opt (read-from-minibuffer
	      "Documentclass options ([opt1,opt2,...]): "
	      nil YaTeX-minibuffer-completion-map nil))
	(substr opt) o)
    (if (string< "" opt)
	(progn
	  (while substr
	    (setq o (substring substr 0 (string-match delim substr)))
	    (or (assoc o dt)
		(YaTeX-update-table
		 (list o)
		 'YaTeX:documentclass-options-default
		 'YaTeX:documentclass-options-private
		 'YaTeX:documentclass-options-local))
	    (setq substr
		  (if (string-match delim substr)
		      (substring substr (1+ (string-match delim substr))))))
	  (concat "[" opt "]"))
      "")))

(defvar YaTeX:documentclasses-default
  '(("article") ("jarticle") ("report") ("jreport") ("book") ("jbook")
    ("j-article") ("j-report") ("j-book")
    ("letter") ("slides") ("ltxdoc") ("ltxguide") ("ltnews") ("proc"))
  "Default documentclass alist")
(defvar YaTeX:documentclasses-private nil
  "*User defined documentclass alist")
(defvar YaTeX:documentclasses-local nil
  "*User defined local documentclass alist")
(defvar YaTeX-default-documentclass (if YaTeX-japan "jarticle" "article")
  "*Default documentclass")

(defun YaTeX::documentclass (&optional argp)
  (cond
   ((equal argp 1)
    (setq env-name "document")
    (let ((sname
	   (YaTeX-cplread-with-learning
	    (format "Documentclass (default %s): " YaTeX-default-documentclass)
	    'YaTeX:documentclasses-default
	    'YaTeX:documentclasses-private
	    'YaTeX:documentclasses-local)))
      (if (string= "" sname) (setq sname YaTeX-default-documentclass))
      (setq YaTeX-default-documentclass sname)))))

;;; -------------------- End of yatexadd --------------------
(provide 'yatexadd)

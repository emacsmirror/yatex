;;; -*- Emacs-Lisp -*-
;;; YaTeX add-in functions.
;;; yatexadd.el rev.8
;;; (c )1991-1994 by HIROSE Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Sun May 15 18:00:12 1994 on 98fa
;;; $Id$

(provide 'yatexadd)

;;;
;;Sample functions for LaTeX environment.
;;;
(defvar YaTeX:tabular-default-rule
  "@{\\vrule width 1pt\\ }c|c|c@{\\ \\vrule width 1pt}"
  "*Your favorite default rule format."
)
(defun YaTeX:tabular ()
  "YaTeX add-in function for tabular environment.
Notice that this function refers the let-variable `env' in
YaTeX-make-begin-end."
  (let ((width "") bars (rule "") (j 0) loc)
    (if (string= env "tabular*")
	(setq width (concat "{" (read-string "Width: ") "}")))
    (setq loc (YaTeX:read-position "tb")
	  bars (string-to-int (read-string "Number of `|': ")))
    (if (> bars 0)
	(while (< j bars) (setq rule (concat rule "|")) (setq j (1+ j)))
      (setq rule YaTeX:tabular-default-rule))
    (setq rule (read-string "rule format: " rule))

    (message "")
    (format "%s%s{%s}" width loc rule))
)
(fset 'YaTeX:tabular* 'YaTeX:tabular)
(defun YaTeX:array ()
  (concat (YaTeX:read-position "tb")
	  "{" (read-string "Column format: ") "}")
)

(defun YaTeX:read-position (oneof)
  (let ((pos "") loc)
    (while (not (string-match
		 (setq loc (read-key-sequence
			    (format "Position (`%s') [%s]: " oneof pos)))
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
    (if (string= pos "") ""
      (concat "[" pos "]")))
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
  (concat (YaTeX:read-coordinates "Dimension")
	  (YaTeX:read-position "lrtb"))
)

(defun YaTeX:framebox ()
  (if (YaTeX-quick-in-environment-p "picture")
      (YaTeX:makebox))
)

(defun YaTeX:dashbox ()
  (concat "{" (read-string "Dash dimension: ") "}"
	  (YaTeX:read-coordinates "Dimension"))
)

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
  (let ((case-fold-search t))
    (cond
     ((save-excursion
	(forward-char 1)
	(re-search-forward (concat "^" (this-command-keys)) nil t))
      (goto-char (match-beginning 0)))
     ((save-excursion
	(goto-char (point-min))
	(re-search-forward (concat "^" (this-command-keys)) nil t))
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
		 ((>= line (1+ lnum ))
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

;;;
;; global subroutines
;;;
(defun substitute-all-key-definition (olddef newdef keymap)
  "Replace recursively OLDDEF with NEWDEF for any keys in KEYMAP now
defined as OLDDEF. In other words, OLDDEF is replaced with NEWDEF
where ever it appears."
  (if (arrayp keymap)
      (let ((len (length keymap))
	    (i 0))
	(while (< i len)
	  (let ((map (aref keymap i)))
	    (cond
	     ((arrayp map) (substitute-key-definition olddef newdef map))
	     ((equal map olddef)
	      (aset keymap i newdef)))
	    (setq i (1+ i)))))
    (while keymap
      (if (equal (cdr-safe (car-safe keymap)) olddef)
	  (setcdr (car keymap) newdef))
      (setq keymap (cdr keymap)))))

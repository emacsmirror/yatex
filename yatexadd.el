;;; -*- Emacs-Lisp -*-
;;; YaTeX add-in functions.
;;; yatexadd.el rev.3
;;; (c)1991-1993 by HIROSE Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Tue May  4 21:50:37 1993 on figaro
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
  "YaTeX add-in function for tabular environment."
  (let (bars (rule "") (j 0) (loc (YaTeX:read-position "tb")))
    (setq bars (string-to-int (read-string "Number of `|': ")))
    (if (> bars 0)
	(while (< j bars) (setq rule (concat rule "|")) (setq j (1+ j)))
      (setq rule YaTeX:tabular-default-rule))
    (setq rule (read-string "rule format: " rule))

    (message "")
    (format "%s{%s}" loc rule))
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
  (let ((bndry (read-string (concat ULchar "{...}: "))))
    (if (string= bndry "") ""
      (concat ULchar "{" bndry "}")))
)

(defun YaTeX:check-completion-type (type)
  "Check valid completion type."
  (if (not (eq type YaTeX-current-completion-type))
      (error "This should be completed with %s-type completion." type))
)

;;; -*- Emacs-Lisp -*-
;;; YaTeX add in functions.
;;; yatexadd.el rev.2
;;; (c)1991-1993 by HIROSE Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Fri Feb  5 09:36:06 1993 on gloria

(provide 'yatexadd)

;;;
;;Sample functions for LaTeX environment.
;;;
(defvar YaTeX:tabular-default-rule
  "@{\\vrule width 1pt\\ }c|c|c@{\\ \\vrule width 1pt}"
  "*Your favorite default rule format."
)
(defun YaTeX:tabular ()
  "YaTeX add in function for tabular environment."
  (let (bars (rule "") (j 0) loc)
    (setq bars (string-to-int (read-string "Number of `|': ")))
    (if (> bars 0)
	(while (< j bars) (setq rule (concat rule "|")) (setq j (1+ j)))
      (setq rule YaTeX:tabular-default-rule))
    (setq rule (read-string "rule format: " rule))

    (insert (format "{%s}" rule))
    (message ""))
)

(defun YaTeX:table ()
  (let ((pos ""))
    (message "Position []:")
    (while (not (string-match
		 (setq loc (read-key-sequence (format "Position [%s]: " pos)))
		 "\r\^g\n"))
      (cond
       ((string-match loc "htbp")
	(if (not (string-match loc pos))
	    (setq pos (concat pos loc))))
       ((and (string-match loc "\C-h\C-?") (> (length pos) 0))
	(setq pos (substring pos 0 (1- (length pos)))))
       (t
	(ding)
	(message "Please input one of `htbp'.")
	(sit-for 3))))
    (if (string= pos "") nil
      (insert "[" pos "]")))
)

(defun YaTeX:description ()
  "Truly poor service:-)"
  (setq single-command "item[]")
)

(defun YaTeX:itemize ()
  "It's also poor service."
  (setq single-command "item")
)

(fset 'YaTeX:enumerate 'YaTeX:itemize)

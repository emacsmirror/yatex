;;; -*- Emacs-Lisp -*-
;;; YaTeX interface for math-mode.
;;; yatexmth.el rev.0
;;; (C)1993 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Tue Aug  3 23:37:07 1993 on 98fa
;;; $Id$

(setq
 YaTeX-math-key-alist-default
 '(
   ;frequently used
   ("||"	"|"		("||"		"Åa"))
   ("sum"	"sum"		("\\-+\n >\n/-+" "É∞"))
   ("sigma"	"sum"		("\\-+\n >\n/-+" "É∞"))
   ("integral"	"int"		" /\\\n \\\n\\/")
   ("ointegral"	"oint"		" /\\\n(\\)\n\\/")
   ("A"		"forall"	"|_|\nV")
   ("E"		"exists"	"-+\n-+\n-+")
   ("!"		"neg"		"--+\n  |")
   ("oo"	"infty"		("oo"		"Åá"))
   ("\\"	"backslash"	("\\"		"Å_"))

   ;;binary operators
   ("+-"	"pm"		("+\n-" "Å}"))
   ("-+"	"mp"		"-\n+")
   ("x"		"times"		("x" "Å~"))
   ("/"		"div"		(",\n-\n'" "ÅÄ"))
   ("*"		"ast"		"*")
   ("#"		"star"		("_/\\_\n\\  /\n//\\\\" "Åö"))
   ("o"		"circ"		"o")
   ("o*"	"bullet"	" _\n(*)\n ~")
   ("."		"cdot"		".")
   ("cap"	"cap"		"/-\\\n| |")
   ("cup"	"cup"		"| |\n\\-/")
   ("u+"	"uplus"		"|+|\n\\-/")
   ("|~|"	"sqcap"		"|~|")
   ("|_|"	"sqcup"		"|_|")
   ("v"		"vee"		"v")
   ("^"		"wedge"		"^")
   ("\\\\"	"setminus"	"\\")
   (")("	"wr"		" )\n(")
   ("<>"	"diamond"	"<>")
   ("/\-"	"bigtriangleup"	("/\\\n~~" "Å¢"))
   ("-\\/"	"bigtriangledown" ("__\n\\/" "Å§"))
   ("<|"	"triangleleft"	"<|")
   ("|>"	"triangleright"	"|>")
   ("<||"	"lhd"		"/|\n\\|")
   ("||>"	"rhd"		"|\\\n|/")
   ("<|-"	"unlhd"		"<|\n~~")
   ("|>-"	"unrhd"		"|>\n~~")
   ("o+"	"oplus"		" _\n(+)\n ~")
   ("o-"	"ominus"	" _\n(-)\n ~")
   ("ox"	"otimes"	" _\n(x)\n ~")
   ("o/"	"oslash"	" _\n(/)\n ~")
   ("o."	"odot"		"(.)")
   ("O"		"bigcirc"	"O")
   ("t"		"dagger"	"+\n|")
   ("tt"	"ddagger"	"+\n+\n|")
   ("II"	"amalg"		"II")
   ;	:
   ;;relational operators
   ("<"		"leq"		("<\n-"		"ÅÖ"))
   (">"		"geq"		(">\n-"		"ÅÜ"))
   ("-="	"equiv"		"=\n-")
   ("=-"	"equiv"		"=\n-")
   ("---"	"equiv"		"=\n-")
   ("("		"subset"	" _\n(\n ~")
   ("(-"	"subseteq"	" _\n(_\n~")
   (")"		"supset"	"_\n )\n~")
   (")-"	"supseteq"	"_\n_)\n~")
   ("["		"sqsubset"	"[")
   ("[-"	"sqsubseteq"	"[\n~")
   ("]"		"sqsupset"	"]")
   ("]-"	"sqsupseteq"	"]\n~")
   ("{"		"in"		"(-")
   ("}"		"ni"		"-)")
   ("|-"	"vdash"		"|-")
   ("-|"	"dashv"		"-|")
   ("~"		"sim"		"~(tild)")
   ("~-"	"simeq"		"~\n-")
   ("asymp"	"asymp"		"v\n^")
   ("~~"	"approx"	"~\n~")
   ("~="	"cong"		"~\n=")
   ("=/"	"neq"		("=/="		"ÅÇ"))
   (".="	"doteq"		".\n=")
   ("o<"	"propto"	"o<")
   ("|="	"models"	"|=")
   ("_|_"	"perp"		"_|_")
   ("|"		"mid"		"|")
   ("||"	"parallel"	"||")
   ("bowtie"	"bowtie"	"|><|(wide)")
   ("|><|"	"join"		"|><|")
   ("\\_/"	"smile"		"\\_/")
   ("/~\\"	"frown"		"/~~\\")
   ("-<"	"prec"		("-<"		"Ç≠"))
   ("-<="	"preceq"	("-<\n-"	"Ç≠\n="))
   ("<<"	"ll"		("<<"		"Ås"))
   ;	:
   ;;arrows
   ("<-"	"leftarrow"	("<-"		"Å©"))
   ("<--"	"longleftarrow"	("<-"		"Å©--"))
   ("<="	"Leftarrow"	"<=")
   ("<=="	"Longleftarrow"	"<==")
   ("->"	"rightarrow"	("->"		"Å®"))
   ("-->"	"longrightarrow" ("-->"		"--Å®"))
   ("==>"	"Longrightarrow" "==>")
   ("<->"	"leftrightarrow" ("<->"		"Å©Å®"))
   ("<-->"	"longleftrightarrow" ("<---->"	"Å©--Å®"))
   ("<=>"	"leftrightarrow" "<=>")
   ("<==>"	"Longleftrightarrow" "<==>")
   ("^|"	"uparrow"	("^\n|" "Å™"))
   ("^||"	"Uparrow"	"/\\\n||")
   ("\C-n"	"downarrow"	("|\nv" "Å´"))
   ("^|"	"uparrow"	("^\n|" "Å™"))
   ("|->"	"mapsto"	("|->"		"|Å®"))
   ("<-)"	"hookleftarrow"	("   ,\n<--+"	"   ÅR\n<--/"))
   ("/-"	"leftharpoonup"	"/\n~~~")
   ("\\-"	"leftharpoondown" "__\n\\")
   ("-/"	"rightharpoondown"  "__\n/")
   ("-\\"	"rightharpoonup" "~~\n\\")
   ;other marks
   ("Z"		"aleph"		"|\\|")
   ("|\\|"	"aleph"		"|\\|")
   ("h-"	"hbar"		"_\nh")
   ("i"		"imath"		"i")
   ("j"		"jmath"		"j")
   ("l"		"ell"		"l")
   ("wp"	"wp"		"???")
   ("R"		"Re"		")R")
   ("Im"	"Im"		"???")
   ("mho"	"mho"		"~|_|~")
   ("'"		"prime"		"'")
   ("0"		"emptyset"	"0")
   ("nabla"	"nabla"		"___\n\\\\/")
   ("\\/"	"surd"		"-\\/")
   ("surd"	"surd"		"-\\/")
   ("top"	"top"		"T")
   ("bot"	"bot"		"_|_")
   ("b"		"flat"		"b")
   ("LT"	"natural"	"|\nLT\n |")
   ("6"		"partial"	" -+\n+-+\n+-+")
   ("partial"	"partial"	" -+\n+-+\n+-+")
   ("round"	"partial"	" -+\n+-+\n+-+")
   ("[]"	"box"		"[]")
   ("Diamond"	"Diamond"	"/\\\n\\/")
   ("3"		"triangle"	"/\\\n~~")
   ("C"		"clubsuit"	" o\no+o\n |")
   ("D"		"diamondsuit"	"/\\\n\\/")
   ("H"		"heartsuit"	"<^^>\n \\/")
   ("S"		"spadesuit"	" /\\\n<++>\n /\\")

   ))

(defvar YaTeX-math-key-alist-private nil
  "*User definable key vs LaTeX-math-command alist.")

(defvar YaTeX-math-quit-with-strict-match nil
  "*T for quitting completion as soon as strict-match is found.")
(setq YaTeX-math-key-alist
      (append YaTeX-math-key-alist-private YaTeX-math-key-alist-default))

(setq YaTeX-math-key-array
  (let ((array (make-vector (length YaTeX-math-key-alist) ""))
	(list YaTeX-math-key-alist) (i 0))
    (while list
      (aset array i (car (car list)))
      (setq i (1+ i) list (cdr list)))
    array))

(defvar YaTeX-ec "\\" "Escape character of mark-up language.")
(setq YaTeX-math-indicator
  "KEY\tLaTeX sequence\t\tsign")

(defvar YaTeX-math-need-image t
  "*T for displaying pseudo image momentarily.")
(defvar YaTeX-math-max-key 8)
(defvar YaTeX-math-max-seq
  (* 8 (1+ (/ (length "\\longleftrightarrow") 8))))
(defvar YaTeX-math-max-sign 5)
(defvar YaTeX-math-sign-width
  (+ YaTeX-math-max-key YaTeX-math-max-seq YaTeX-math-max-sign))
(defvar YaTeX-math-display-width
  (* 8 (1+ (/ YaTeX-math-sign-width 8))))
(defvar YaTeX-math-menu-map nil
  "Keymap used in YaTeX mathematical sign menu mode."
)
(if YaTeX-math-menu-map nil
  (setq YaTeX-math-menu-map (make-sparse-keymap))
  (define-key YaTeX-math-menu-map "n"	'next-line)
  (define-key YaTeX-math-menu-map "p"	'previous-line)
  (define-key YaTeX-math-menu-map "f"	'YaTeX-math-forward)
  (define-key YaTeX-math-menu-map "b"	'YaTeX-math-backward)
  (define-key YaTeX-math-menu-map "v"	'scroll-up)
  (define-key YaTeX-math-menu-map " "	'scroll-up)
  (define-key YaTeX-math-menu-map "c"	'scroll-up)
  (define-key YaTeX-math-menu-map "V"	'scroll-down)
  (define-key YaTeX-math-menu-map "r"	'scroll-down)
  (define-key YaTeX-math-menu-map "\^h"	'scroll-down)
  (define-key YaTeX-math-menu-map "<"	'beginning-of-buffer)
  (define-key YaTeX-math-menu-map ">"	'end-of-buffer)
  (define-key YaTeX-math-menu-map "\^m"	'exit-recursive-edit)
  (define-key YaTeX-math-menu-map "q"	'abort-recursive-edit))

(defmacro YaTeX-math-japanese-sign (list)
  (list 'nth 1 list))

(defvar YaTeX-math-cmd-regexp (concat (regexp-quote YaTeX-ec) "[A-z]"))

(defun YaTeX-math-forward (arg)
  (interactive "p")
  (re-search-forward YaTeX-math-cmd-regexp nil t arg))

(defun YaTeX-math-backward (arg)
  (interactive "p")
  (re-search-backward YaTeX-math-cmd-regexp nil t arg))

(defun YaTeX-math-get-sign (list)
  (let ((sign (car (cdr (cdr list)))))
    (if (listp sign)
	(setq sign (cond
		    (YaTeX-japan (YaTeX-math-japanese-sign sign))
		    (t (car sign)))))
    sign)
)

(defun YaTeX-math-display-list (list cols)
  (goto-char (point-max))
  (if (= cols 0) (if (not (eolp)) (newline 1))
    (forward-line -1)
    (while (looking-at "[ \t\n]") (forward-line -1)))
  (end-of-line)
  (let ((indent (* YaTeX-math-display-width cols)) sign str to)
    (indent-to indent)
    (insert (car list))
    (indent-to (setq indent (+ indent YaTeX-math-max-key)))
    (insert "\\" (car (cdr list)))
    (setq indent (+ indent YaTeX-math-max-seq))
    (setq sign (YaTeX-math-get-sign list))
    (while (not (string= "" sign))
      (setq to (string-match "\n" sign)
	    str (if to (substring sign 0 to) sign))
      (end-of-line)
      (indent-to indent)
      (insert str)
      (cond ((eobp) (newline 1))
	    ((> cols 0) (forward-line 1)))
      (setq sign (if to (substring sign (1+ to)) "")))))

(defvar YaTeX-math-menu-buffer "*math-mode-signs*")

(defun YaTeX-math-show-menu (match-str)
  (save-window-excursion
    (pop-to-buffer YaTeX-math-menu-buffer)
    (let ((maxcols (max 1 (/ (screen-width) YaTeX-math-sign-width)))
	  (case-fold-search nil)
	  (cols 0) (list YaTeX-math-key-alist) command)
      (erase-buffer)
      (insert YaTeX-math-indicator "\t")
      (insert YaTeX-math-indicator)
      (newline 1)
      (insert-char ?- (1- (screen-width)))
      (newline 1)
      (while list
	(if (string-match match-str (car (car list)))
	    (progn (YaTeX-math-display-list (car list) cols)
		   (setq cols (% (1+ cols) maxcols))))
	(setq list (cdr list)))
      (goto-char (point-min))
      (use-local-map YaTeX-math-menu-map)
      (unwind-protect
	  (recursive-edit)
	(skip-chars-backward "^ \t\n")
	(setq command
	      (if (re-search-forward YaTeX-math-cmd-regexp nil t)
		  (buffer-substring
		   (match-beginning 0)
		   (prog2 (skip-chars-forward "^ \t\n") (point)))
		nil))
	(kill-buffer YaTeX-math-menu-buffer))
      command))
)

;
(defun YaTeX-math-show-image (image &optional exit-char)
  "Momentarily display IMAGE at the beginning of the next line;
erase it on the next keystroke.  The window is recentered if necessary
to make the whole string visible.  If the window isn't large enough,
at least you get to read the beginning."
  (let ((buffer-read-only nil)
	(modified (buffer-modified-p))
	(name buffer-file-name)
	insert-start
	insert-end)
    (unwind-protect
	(progn
	  (save-excursion
	    ;; defeat file locking... don't try this at home, kids!
	    (setq buffer-file-name nil)
	    (forward-line 1)
	    (setq insert-start (point))
	    (if (eobp) (newline))
	    (insert image)
	    (setq insert-end (point)))
	  ; make sure the whole string is visible
	  (if (not (pos-visible-in-window-p insert-end))
	      (recenter (max 0
			     (- (window-height)
				(count-lines insert-start insert-end)
				2))))
	  (let ((char (read-char)))
	    (or (eq char exit-char)
		(setq unread-command-char char))))
      (if insert-end
	  (save-excursion
	    (delete-region insert-start insert-end)))
      (setq buffer-file-name name)
      (set-buffer-modified-p modified))))

(defun YaTeX-math-insert-sequence ()
  (interactive)
  (let ((key "") regkey str  last-char list i
	(case-fold-search nil) match
	(n (length YaTeX-math-key-array)) (beg (point)) result)
    (setq result
	  (catch 'complete
	    (while t
	      (setq last-char (read-char)
		    key (concat key (char-to-string last-char))
		    regkey (concat "^" (regexp-quote key)) i 0)
	      (cond
	       ((string= key YaTeX-math-invoke-key)	;;invoke key itself
		(throw 'complete 'escape))
	       ((string-match "[\C-g\C-c]" key) (throw 'complete 'abort))
	       ((string-match "[\n\r]" key) (throw 'complete 'menu)))
	      (if
		  (catch 'found
		    ;;(1)input string strictly matches with alist
		    (setq single-command (car (cdr match))
			  ;;remember previous match
			  match (assoc key YaTeX-math-key-alist))
		    ;;(2)search partial match into alist
		    (while (< i n)
		      (if (string-match
			   regkey (aref YaTeX-math-key-array i))
			  (progn
			    (or match
				(setq match (nth i YaTeX-math-key-alist)))
			    (throw 'found t)))
		      (setq i (1+ i)))) 		;catch 'found
		  nil ;;if any match, continue reading
		;;else reading of sequence has been done.
		(message "complete.")
		(throw 'complete t)
		)
	      (if match
		  (progn (delete-region beg (point))
			 (insert YaTeX-ec (car (cdr match)))
			 (if YaTeX-math-need-image
			     (YaTeX-math-show-image
			      (concat (YaTeX-math-get-sign match) "\n")))
			 )
		nil)
	      )))
    (cond
     ((eq result t)
      (setq YaTeX-current-completion-type 'maketitle)
      (if t nil
	(delete-region beg (point))
	(setq single-command (car (cdr match)))
	;;(recursive-edit)
	(insert YaTeX-ec single-command)
	)
      (sit-for 1)
      (setq unread-command-char last-char)
      (insert (YaTeX-addin single-command)))
     ((eq result 'abort)
      (delete-region beg (point))
      (message "Abort."))
     ((eq result 'escape)
      (delete-region beg (point))
      (insert YaTeX-math-invoke-key))
     ((eq result 'menu)
      (delete-region beg (point))
      (setq key (concat "^" (regexp-quote (substring key 0 -1))))
      (insert (YaTeX-math-show-menu key)))))
)
;;
(provide 'yatexmth)

;;; -*- Emacs-Lisp -*-
;;; YaTeX interface for math-mode.
;;; yatexmth.el rev.2
;;; (c )1993-1994 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Sat Apr 23 02:26:57 1994 on pajero
;;; $Id$

;;; [Customization guide]
;;;
;;;	  By default,  you can use two  completion  groups in YaTeX math
;;;	mode, `,' for mathematical signs and `/' for greek letters.  But
;;;	you  can add other completion groups   by defining the  alist of
;;;	`prefix  key'    vs   `completion   list'    into  the  variable
;;;	YaTeX-math-key-list-private.  If  you wish  to    accomplish the
;;;	completion as follows(prefix key is `;'):
;;;
;;;		KEY		COMPLETION
;;;		s		\sin
;;;		S		\arcsin
;;;		c		\cos
;;;		C		\arccos
;;;			:
;;;		T		\arctan
;;;		l		\log
;;;		hs		\sinh
;;;
;;;	Typing `s' after `;' makes `\sin', `hs' after `;' makes `\sinh'
;;;	and so on.  First, you have to define list of key-completion
;;;	pairs.  Variable name(YaTeX-math-funcs-list) is arbitrary.
;;;
;;;		(setq YaTeX-math-funcs-list
;;;		      '(("s"	"sin")
;;;			("S"	"arcsin")
;;;				:
;;;			("hs"	"sinh")))
;;;
;;;	Next, define the list of prefix-key vs completion list(defined
;;;	above) into the variable YaTeX-math-key-list-private.
;;;
;;;		(setq YaTeX-math-key-list-private
;;;		      '((";" . YaTeX-math-funcs-list)
;;;			("'" . Other-List-if-any)))
;;;
;;;	  Put these expressions into your ~/.emacs, and you can use this
;;;	completion in the YaTeX-math-mode.
;;;
;;;	  And you can add your favorite completion sequences to the
;;;	default completion list invoked with `,', by defining those lists
;;;	into variable YaTeX-math-sign-alist-private.

;;; �y�C���[�W�⊮�̒ǉ����@�z
;;;
;;;	  �W���̃C���[�W�⊮�ł́A�u,�v�Ŏn�܂鐔���L���⊮�ƁA�u/�v�Ŏn
;;;	�܂�M���V�������⊮���g�p�\�ł����A����ȊO�̕����Ŏn�܂�⊮
;;;	�V���[�Y����`���邱�Ƃ��ł��܂��B�Ⴆ�΁A�u;�v�Ŏn�܂鎟�̂悤
;;;	�ȕ⊮�V���[�Y���`����ꍇ���l���Ă݂܂��B
;;;
;;;		�⊮�L�[	�⊮����
;;;		s		\sin
;;;		S		\arcsin
;;;		c		\cos
;;;		C		\arccos
;;;			:
;;;		T		\arctan
;;;		l		\log
;;;		hs		\sinh
;;;
;;;	�u;�v�̂��Ƃ� s �������� \sin ���Ahs �������� \sinh �����͂����
;;;	���B���̂悤�ȕ⊮���X�g�̓o�^�͈ȉ��̂悤�ɂ��܂�(�ϐ����͔C��)�B
;;;
;;;		(setq YaTeX-math-funcs-list
;;;		      '(("s"	"sin")
;;;			("S"	"arcsin")
;;;				:
;;;			("hs"	"sinh")))
;;;
;;;	����ɁA�u;�v�����������ɃC���[�W�⊮���n�܂�悤���̕ϐ��ɁA�N���L�[
;;;	�Ə�Œ�`�����⊮�p�ϐ��̓o�^���s�Ȃ��܂��B
;;;
;;;		(setq YaTeX-math-key-list-private
;;;		      '((";" . YaTeX-math-funcs-list)
;;;			("'" . �ق��ɒ�`�������V���[�Y������΁c)))
;;;
;;;	������ ~/.emacs �ɏ����Ă����΁A�쒹�� math-mode �Ŏ�����p��
;;;	�C���[�W�⊮�����p�ł��܂��B

(defvar YaTeX-jisold
  (and (boundp 'dos-machine-type)
       (eq dos-machine-type 'pc98)))

(defmacro YaTeX-setq-math-sym (sym old new)
  (list 'setq sym (list 'if 'YaTeX-jisold old new)))

(YaTeX-setq-math-sym YaTeX-image-in		"E"    		"��")
(YaTeX-setq-math-sym YaTeX-image-ni		"��"		"��")
(YaTeX-setq-math-sym YaTeX-image-subset		" _\n(\n ~"	"��")
(YaTeX-setq-math-sym YaTeX-image-subseteq	" _\n(_\n ~"	"��")
(YaTeX-setq-math-sym YaTeX-image-supset		"_\n )\n~"	"��")
(YaTeX-setq-math-sym YaTeX-image-supseteq	"_\n_)\n~" 	"��")
(YaTeX-setq-math-sym YaTeX-image-nabla		"___\n\\\\/"	"��")
(YaTeX-setq-math-sym YaTeX-image-partial	" -+\n+-+\n+-+" "��")
(YaTeX-setq-math-sym YaTeX-image-dagger		"+\n|"		"��")
(YaTeX-setq-math-sym YaTeX-image-ddagger	"+\n+\n|"	"��")
(YaTeX-setq-math-sym YaTeX-image-equiv		"��\n�P"	"��")
(YaTeX-setq-math-sym YaTeX-image-int		" /\\\n \\\n\\/" "��")
(YaTeX-setq-math-sym YaTeX-image-bot		"�b\n�P"	"��")
(YaTeX-setq-math-sym YaTeX-image-neg		"��"		"��")
(YaTeX-setq-math-sym YaTeX-image-flat		"��"		"��")

(setq
 YaTeX-math-sign-alist-default
 '(
   ;frequently used
   ("||"	"|"		("||"		"�a"))
   ("sum"	"sum"		("\\-+\n >\n/-+" "��"))
   ("sigma"	"sum"		("\\-+\n >\n/-+" "��"))
   ("integral"	"int"		(" /\\\n \\\n\\/" YaTeX-image-int))
   ("ointegral"	"oint"		" /\\\n(\\)\n\\/")
   ("A"		"forall"	"|_|\nV")
   ("E"		"exists"	"-+\n-+\n-+")
   ("!"		"neg"		("--+\n  |" YaTeX-image-neg))
   ("oo"	"infty"		("oo"		"��"))
   ("\\"	"backslash"	("\\"		"�_"))

   ;;binary operators
   ("+-"	"pm"		("+\n-" "�}"))
   ("-+"	"mp"		"-\n+")
   ("x"		"times"		("x" "�~"))
   ("/"		"div"		(",\n-\n'" "��"))
   ("*"		"ast"		"*")
   ("#"		"star"		("_/\\_\n\\  /\n//\\\\" "��"))
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
   ("/\-"	"bigtriangleup"	("/\\\n~~" "��"))
   ("-\\/"	"bigtriangledown" ("__\n\\/" "��"))
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
   ("t"		"dagger"	("+\n|" YaTeX-image-dagger))
   ("tt"	"ddagger"	("+\n+\n|" YaTeX-image-ddagger))
   ("II"	"amalg"		"II")
   ;	:
   ;;relational operators
   ("<"		"leq"		("<\n-"		"��"))
   (">"		"geq"		(">\n-"		"��"))
   ("-="	"equiv"		("=\n-"		YaTeX-image-equiv))
   ("=-"	"equiv"		("=\n-"		YaTeX-image-equiv))
   ("---"	"equiv"		("=\n-"		YaTeX-image-equiv))
   ("("		"subset"	(" _\n(\n ~"	YaTeX-image-subset))
   ("(-"	"subseteq"	(" _\n(_\n ~"	YaTeX-image-subseteq))
   (")"		"supset"	("_\n )\n~"	YaTeX-image-supset))
   (")-"	"supseteq"	("_\n_)\n~"	YaTeX-image-supseteq))
   ("["		"sqsubset"	"[")
   ("[-"	"sqsubseteq"	"[\n~")
   ("]"		"sqsupset"	"]")
   ("]-"	"sqsupseteq"	"]\n~")
   ("{"		"in"		("(-" YaTeX-image-in))
   ("}"		"ni"		("-)" YaTeX-image-ni))
   ("|-"	"vdash"		"|-")
   ("-|"	"dashv"		"-|")
   ("~"		"sim"		"~(tild)")
   ("~-"	"simeq"		"~\n-")
   ("asymp"	"asymp"		"v\n^")
   ("~~"	"approx"	"~\n~")
   ("~="	"cong"		"~\n=")
   ("=/"	"neq"		("=/="		"��"))
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
   ("-<"	"prec"		("-<"		"��"))
   ("-<="	"preceq"	("-<\n-"	"��\n="))
   ("<<"	"ll"		("<<"		"�s"))
   ;	:
   ;;arrows
   ("<-"	"leftarrow"	("<-"		"��"))
   ("\C-b"	"leftarrow"	("<-"		"��"))
   ("<--"	"longleftarrow"	("<--"		"��--"))
   ("<="	"Leftarrow"	"<=")
   ("<=="	"Longleftarrow"	"<==")
   ("->"	"rightarrow"	("->"		"��"))
   ("\C-f"	"rightarrow"	("->"		"��"))
   ("-->"	"longrightarrow" ("-->"		"--��"))
   ("==>"	"Longrightarrow" "==>")
   ("<->"	"leftrightarrow" ("<->"		"����"))
   ("<-->"	"longleftrightarrow" ("<---->"	"��--��"))
   ("<=>"	"leftrightarrow" "<=>")
   ("<==>"	"Longleftrightarrow" "<==>")
   ("^|"	"uparrow"	("^\n|" "��"))
   ("\C-p"	"uparrow"	("^\n|" "��"))
   ("^||"	"Uparrow"	"/\\\n||")
   ("\C-n"	"downarrow"	("|\nv" "��"))
   ("v|"	"downarrow"	("|\nv" "��"))
   ("v||"	"Downarrow"	"||\n\\/")
   ("|->"	"mapsto"	("|->"		"|��"))
   ("<-)"	"hookleftarrow"	("   ,\n<--+"	"   �R\n<--/"))
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
   ("nabla"	"nabla"		("___\n\\\\/" YaTeX-image-nabla))
   ("\\/"	"surd"		"-\\/")
   ("surd"	"surd"		"-\\/")
   ("top"	"top"		"T")
   ("bot"	"bot"		("_|_"		YaTeX-image-bot))
   ("b"		"flat"		("b"		YaTeX-image-flat))
   ("LT"	"natural"	"|\nLT\n |")
   ("6"		"partial"	(" -+\n+-+\n+-+" YaTeX-image-partial))
   ("partial"	"partial"	(" -+\n+-+\n+-+" YaTeX-image-partial))
   ("round"	"partial"	(" -+\n+-+\n+-+" YaTeX-image-partial))
   ("[]"	"box"		"[]")
   ("Diamond"	"Diamond"	"/\\\n\\/")
   ("3"		"triangle"	"/\\\n~~")
   ("C"		"clubsuit"	" o\no+o\n |")
   ("D"		"diamondsuit"	"/\\\n\\/")
   ("H"		"heartsuit"	"<^^>\n \\/")
   ("S"		"spadesuit"	" /\\\n<++>\n /\\")

   ))

(defvar YaTeX-math-sign-alist-private nil
  "*User definable key vs LaTeX-math-command alist.")

(defvar YaTeX-math-quit-with-strict-match nil
  "*T for quitting completion as soon as strict-match is found.")
(setq YaTeX-math-sign-alist
      (append YaTeX-math-sign-alist-private YaTeX-math-sign-alist-default))

;;(defun YaTeX-math-alist2array (alist array)
;;  (set array
;;       (let ((array (make-vector (length alist) "")) (list alist) (i 0))
;;	 (while list
;;	   (aset array i (car (car list)))
;;	   (setq i (1+ i) list (cdr list)))
;;	 array))
;;)

(setq YaTeX-greek-key-alist-default
  '(
    ("a"	"alpha"		("a" "��"))
    ("b"	"beta"		("|>\n|>\n|" "��"))
    ("g"	"gamma"		("~r" "��"))
    ("G"	"Gamma"		("|~" "��"))
    ("d"	"delta"		("<~\n<>" "��"))
    ("D"	"Delta"		("/\\\n~~" "��"))
    ("e"	"epsilon"	"<\n<~")
    ("e-"	"varepsilon"	("(\n(~" "��"))
    ("z"	"zeta"		("(~\n >" "��"))
    ("et"	"eta"		("n\n/" "��"))
    ("th"	"theta"		("8" "��"))
    ("Th"	"Theta"		("(8)" "��"))
    ("th-"	"vartheta"	("-8" "-��"))
    ("i"	"iota"		("i\n\\_/" "��"))
    ("k"	"kappa"		("k" "��"))
    ("l"	"lambda"	("\\n/\\" "��"))
    ("L"	"Lambda"	("/\\" "��"))
    ("m"	"mu"		(" u_\n/" "��"))
    ("n"	"nu"		("|/" "��"))
    ("x"	"xi"		("E\n >" "��"))
    ("X"	"Xi"		("---\n -\n---" "��"))
    ("p"	"pi"		("__\n)(" "��"))
    ("P"	"Pi"		("__\n||" "��"))
    ("p-"	"varpi"		("_\nw" "__\n��"))
    ("r"	"rho"		("/O" "��"))
    ("r-"	"varrho"	("/O\n~~" "��\n~~"))
    ("s"	"sigma"		("o~" "��"))
    ("S"	"Sigma"		("\\-+\n >\n/-+" "��"))
    ("s-"	"varsigma"	"(~~ \n>")
    ("t"	"tau"		("__\n(" "��"))
    ("u"	"upsilon"	("~v" "��"))
    ("y"	"upsilon"	("~v" "��"))
    ("U"	"Upsilon"	("~Y~" "��"))
    ("Y"	"Upsilon"	("~Y~" "��"))
    ("ph"	"phi"		("  /\n(/)\n/" "��"))
    ("Ph"	"Phi"		(" _\n(|)\n ~" "��"))
    ("ph-"	"varphi"	"\\O\n|")
    ("c"	"chi"		("x" "��"))
    ("ps"	"psi"		("\\|/\\n |" "��"))
    ("Ps"	"Psi"		(" ~\n\\|/\\n |" "��"))
    ("o"	"omega"		("w" "��"))
    ("w"	"omega"		("w" "��"))
    ("O"	"Omega"		("(~)\n~ ~" "��"))
    ("W"	"Omega"		("(~)\n~ ~" "��"))
    ("f" "foo")
    )
)

(defvar YaTeX-greek-key-alist-private nil
  "*User definable key vs LaTeX-math-command alist.")

(setq YaTeX-greek-key-alist
      (append YaTeX-greek-key-alist-private YaTeX-greek-key-alist-default))

;;(mapcar (function (lambda (x) (YaTeX-math-alist2array x)))
;;	YaTeX-math-key-list)

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

(defvar YaTeX-math-key-list-default
  '(("," . YaTeX-math-sign-alist)
    ("/" . YaTeX-greek-key-alist))
  "Default key sequence to invoke math-mode's image completion."
)
(defvar YaTeX-math-key-list-private nil
  "*User defined alist, math-mode-prefix vs completion alist"
)
(defvar YaTeX-math-key-list
  (append YaTeX-math-key-list-private YaTeX-math-key-list-default)
  "Key sequence to invoke math-mode's image completion."
)
(defvar YaTeX-math-exit-key "\e"
  "*Key sequence after prefix key of YaTeX-math-mode to exit from math-mode."
)

(defmacro YaTeX-math-japanese-sign (list)
  (list 'nth 1 list))

(defvar YaTeX-math-cmd-regexp (concat (regexp-quote YaTeX-ec) "[A-z]"))

;;;
;;YaTeX math-mode functions
;;;
;##autoload from yatex.el
(defun YaTeX-toggle-math-mode (&optional arg)
  (interactive "P")
  (require 'yatexmth)
  (or (memq 'YaTeX-math-mode mode-line-format) nil
      (setq mode-line-format
	    (append (list "" 'YaTeX-math-mode) mode-line-format)))
  (if (or arg (null YaTeX-math-mode))
      (let ((keys ""))
	(setq YaTeX-math-mode "math:")
	(mapcar
	 (function (lambda (x)
		     (let ((key (car x)) (list (cdr x)))
		       (setq keys (concat keys " " key))
		       (put 'YaTeX-math-key-list list (key-binding key))
		       (define-key YaTeX-mode-map key
			 'YaTeX-math-insert-sequence))))
	 YaTeX-math-key-list)
	(message "Turn on math mode. Prefix keys are %s" keys)
	(sit-for 3)
	(message
	 (concat "To exit from math-mode, type `ESC' after prefix, "
		 "or type `"
		 (key-description
		  (car
		   (where-is-internal 'YaTeX-switch-mode-menu YaTeX-mode-map)))
		 " $'")))
    (setq YaTeX-math-mode nil)
    (mapcar
     (function (lambda (x)
		 (let ((key (car x)) (list (cdr x)))
		   (define-key YaTeX-mode-map key
		     (get 'YaTeX-math-key-list list)))))
     YaTeX-math-key-list)
    (setq YaTeX-math-mode nil)
    (message "Exit from math mode."))
  (set-buffer-modified-p (buffer-modified-p))
)

(defun YaTeX-math-forward (arg)
  (interactive "p")
  (re-search-forward YaTeX-math-cmd-regexp nil t arg))

(defun YaTeX-math-backward (arg)
  (interactive "p")
  (re-search-backward YaTeX-math-cmd-regexp nil t arg))

(defun YaTeX-math-gets (sign)
  (cond
   ((null sign) nil)
   ((listp sign)
    (setq sign
	  (cond
	   (YaTeX-japan (YaTeX-math-japanese-sign sign))
	   (t (car sign))))
    (YaTeX-math-gets sign))
   ((symbolp sign)
    (YaTeX-math-gets (symbol-value sign)))
   (t sign))
)

(defun YaTeX-math-get-sign (list)
  (YaTeX-math-gets (car (cdr-safe (cdr-safe list))))
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
    (while (and sign (not (string= "" sign)))
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
	  (cols 0) (list alist) command)
      (erase-buffer)
      (insert 
       "Candidates of sign. [n:next p:prev f:forw b:back q:quit RET:select]\n")
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
      (goto-line 4)
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
  (if (and image (not (string= image "")))
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
	  (set-buffer-modified-p modified)))))

(defun YaTeX-math-insert-sequence ()
  (interactive)
  (let*((key "") regkey str  last-char list i
	(case-fold-search nil) match sign
	(this-key (char-to-string last-command-char))
	(alist (symbol-value (cdr (assoc this-key YaTeX-math-key-list))))
	(n (length alist)) (beg (point)) result)
    (setq result
	  (catch 'complete
	    (while t
	      (message "Sequence: %s" key)
	      (setq last-char (read-char)
		    key (concat key (char-to-string last-char))
		    i 0)
	      (cond
	       ((string= key this-key)	;;invoke key itself
		(throw 'complete 'escape))
	       ((string= key YaTeX-math-exit-key)	;;exit from math-mode
		(throw 'complete 'exit))
	       ((string-match "[\C-g\C-c]" key) (throw 'complete 'abort))
	       ((string-match "[\n\r]" key) (throw 'complete 'menu))
	       ((string-match "[\C-h\C-?]" key)
		(if (string= key "") (throw 'complete 'abort))
		(setq key (substring key 0 -2))))
	      
	      (setq regkey (concat "^" (regexp-quote key)))
	      (message "Sequence: %s" key)
	      (if
		  (catch 'found
		    ;;(1)input string strictly matches with alist
		    (setq single-command (car (cdr match))
			  ;;remember previous match
			  match (assoc key alist))
		    ;;(2)search partial match into alist
		    (setq list alist)
		    (while (< i n)
		      (if (string-match
			   regkey
			   ;;(aref array i)
			   ;;(car (nth i alist))
			   (car (car list))
			   )
			  (progn
			    (or match
				;;(setq match (nth i alist))
				(setq match (car list)))
			    (throw 'found t)))
		      (setq i (1+ i) list (cdr list))))	;catch 'found
		  nil ;;if any match, continue reading
		;;else reading of sequence has been done.
		(message "complete.")
		(throw 'complete t)
		)
	      (if match
		  (progn (delete-region beg (point))
			 (insert YaTeX-ec (car (cdr match)))
			 (if (and YaTeX-math-need-image
				  (setq sign (YaTeX-math-get-sign match)))
			     (YaTeX-math-show-image (concat sign "\n")))
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
      ;;;(sit-for 1)
      (setq unread-command-char last-char)
      (insert (YaTeX-addin single-command)))
     ((eq result 'abort)
      (delete-region beg (point))
      (message "Abort."))
     ((eq result 'escape)
      (delete-region beg (point))
      (insert this-key))
     ((eq result 'exit)
      (delete-region beg (point))
      (YaTeX-toggle-math-mode))
     ((eq result 'menu)
      (delete-region beg (point))
      (setq key (concat "^" (regexp-quote (substring key 0 -1))))
      (insert (YaTeX-math-show-menu key)))))
)
;;
(provide 'yatexmth)

; Local variables: 
; fill-prefix: ";;;	" 
; paragraph-start: "^$\\|\\|;;;$" 
; paragraph-separate: "^$\\|\\|;;;$" 
; End: 

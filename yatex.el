;;; -*- Emacs-Lisp -*-
;;; Yet Another tex-mode for emacs.
;;; yatex.el rev.1.37
;;; (c)1991-1993 by HIROSE Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Fri Feb 12 16:05:54 1993 on VFR

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this software, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with this software so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(provide 'yatex)
(require 'comment)
(defconst YaTeX-revision-number "1.37"
  "Revision number of running yatex.el"
)

;---------- Local variables ----------
;;;
;; Initialize local variable for yatex-mode.
;; Preserving user preferred definitions.
;; ** Check all of these defvar-ed values **
;; ** and setq other values more suitable **
;; ** for your site, if needed.           **
;;;
(defvar YaTeX-prefix "\^C"
  "*Prefix key to trigger YaTeX functions.
You can select favorite prefix key by setq in your ~/.emacs."
)
(defvar YaTeX-open-lines 1
  "*Blank lines between text and \??{??}"
)
(defvar YaTeX-fill-prefix ""
  "*fill-prefix used for auto-fill-mode.
The defalut value is null string."
)
(defvar YaTeX-fill-column 72
  "*fill-column used for auto-fill-mode."
)
(defvar YaTeX-comment-prefix "%"
  "TeX comment prefix."
)
(defvar YaTeX-current-position-register ?3
  "*All of YaTeX completing input store the current position into
the register YaTeX-current-position-register.  So every time you
make a trip to any other part of text than you writing, you can
return to editing paragraph by calling register-to-point with argument
YaTeX-current-position-register."
)
(defvar YaTeX-user-completion-table
  (if (eq system-type 'ms-dos) "~/_yatexrc" "~/.yatexrc")
  "*Default filename in which user completion table is saved."
)
(defvar tex-command "jlatex"
  "*Default command for compiling LaTeX text."
)
(defvar dvi2-command		;previewer command for your site
  (if (eq system-type 'ms-dos) "dviout"
    (concat "xdvi -geo +0+0 -s 4 -display " (getenv "DISPLAY")))
  "*Default previewer command including its option.
This default value is for X window system.  If you want to use this
default, you have to make sure the environment variable DISPLAY is
correctly set."
)
(defvar dviprint-command-format
  (if (eq system-type 'ms-dos) "dviprt %s %f%t"
      "dvi2ps %f %t %s | lpr")
  "*Command line string to print out current file.  Format string %s
will be replaced by the filename.  Do not forget to specify the
`from usage' and `to usage' with their option by format string %f and %t.
See also documentation of dviprint-from-format and dviprint-to-format."
)
(defvar dviprint-from-format
  (if (eq system-type 'ms-dos) "%b-" "-f %b")
  "*From page format of dvi filter.  %b will turn to beginning page number."
)
(defvar dviprint-to-format
  (if (eq system-type 'ms-dos) "%e" "-t %e")
  "*To page format of dvi filter.  %e will turn to end page number."
)
(defvar YaTeX-japan (or (boundp 'NEMACS) (boundp 'MULE))
  "Whether yatex mode is running on Japanese environment or not."
)
(defvar YaTeX-default-document-style
  (concat (if YaTeX-japan "j") "article")
  "*Default LaTeX Documentstyle for YaTeX-typeset-region."
)
(defvar YaTeX-need-nonstop nil
  "*If t yatex automatically put `\nonstopmode{}' in current buffer
before invoke latex command."
)
(defvar latex-warning-regexp "line.* [0-9]*"
  "*Regular expression of line number of warning message by latex command."
)
(defvar latex-error-regexp "l\\.[1-9][0-9]*"
  "*Regular expression of line number of latex error.  Perhaps your latex
command stops at this error message with line number of LaTeX source text."
)
(defvar latex-dos-emergency-message
  "Emergency stop"      ;<- for Micro tex, ASCII-pTeX 1.6
  "Because Demacs (GNU Emacs on DOS) cannot have concurrent process, the
latex command which is stopping on a LaTeX error, is terminated by Demacs.
Many latex command on DOS display some message when it is terminated by
other process, user or OS.  Define this variable a message string of your
latex command on DOS shows at abnormal termination.
  Remember Demacs's call-process function is not oriented for interactive
process."
)
(defvar latex-message-kanji-code 2
  "*Kanji coding system latex command types out.
1 = Shift JIS, 2 = JIS, 3 = EUC."
)
(defvar NTT-jTeX nil
  "*Use NTT-jTeX for latex command."
)

;------------ Completion table ------------
; Set tex-section-like command possible completion
(setq section-table
      '(("part") ("section") ("subsection") ("subsubsection")
	("author") ("documentstyle") ("pagestyle")
	("documentstyle[10pt]") ("documentstyle[11pt]")
	("documentstyle[12pt]")
	("title") ("chapter") ("part") ("paragraph")
	("subparagraph") ("underline") ("label") ("footnote")
	("hspace*") ("vspace*") ("bibliography") ("bibitem[]") ("cite[]")
	("nocite") ("input") ("include") ("includeonly") ("mbox") ("hbox")
	("caption") ("newcommand") ("setlength") ("addtolength")
	("newenvironment") ("newtheorem")
	("cline") ("framebox")
))
(defvar user-section-table nil)

; Set style possible completion
(setq article-table
      '(("article") ("jarticle") ("report") ("jreport") ("jbook")
	("4em") ("2ex")
	("empty") ("headings") ("\\textwidth")
	("\\oddsidemargin") ("\\evensidemargin")
	("\\textheight") ("\\topmargin")
	("\\bottommargin") ("\\footskip") ("\\footheight")
	("\\baselineskip") ("\\baselinestretch") ("normalbaselineskip")
))
(defvar user-article-table nil)

; Set tex-environment possible completion
(setq env-table
      '(("quote") ("quotation") ("center") ("verse") ("document")
	("verbatim") ("itemize") ("enumerate") ("description")
	("list{}") ("tabular") ("table") ("tabbing") ("titlepage")
	("sloppypar") ("ref") ("quotation") ("quote") ("picture")
	("eqnarray") ("figure") ("equation") ("abstract") ("array")
	("thebibliography") ("theindex") ("flushleft") ("flushright")
))
(defvar user-env-table nil)

; Set {\Large }-like comletion
(setq fontsize-table
      '(("rm") ("em") ("bf") ("boldmath") ("it") ("sl") ("sf") ("sc") ("tt")
	("dg") ("dm")
	("tiny") ("scriptsize") ("footnotesize") ("small")("normalsize")
	("large") ("Large") ("LARGE") ("huge") ("Huge")
))
(defvar user-fontsize-table nil)

(setq singlecmd-table
      '(("maketitle") ("sloppy") ("protect")
	("alpha") ("beta") ("gamma") ("delta") ("epsilon") ("varepsilon")
	("zeta") ("eta") ("theta")("vartheta") ("iota") ("kappa")
	("lambda") ("mu") ("nu") ("xi") ("pi") ("varpi") ("rho") ("varrho")
	("sigma") ("varsigma") ("tau") ("upsilon") ("phi") ("varphi")
	("chi") ("psi") ("omega") ("Gamma") ("Delta") ("Theta") ("Lambda")
	("Xi") ("Pi") ("Sigma") ("Upsilon") ("Phi") ("Psi") ("Omega")
	("LaTeX") ("TeX") ("item[]") ("appendix") ("hline")
	("rightarrow") ("Rightarrow") ("leftarrow") ("Leftarrow")
	("pagebreak")
))
(defvar user-singlecmd-table nil)

;---------- Key mode map ----------
;;;
;; Create new key map: YaTeX-mode-map
;; Do not change this section.
;;;
(defvar YaTeX-inhibit-prefix-letter nil
  "*Switch which determins whether inhibit yatex.el from defining
key sequence \"C-c letter\" or not."
)
(defvar YaTeX-mode-map nil
  "Keymap used in YaTeX mode."
)
(defvar YaTeX-typesetting-mode-map nil
  "Keymap userd in YaTeX typesetting buffer."
)
(defvar YaTeX-prefix-map nil
  "Keymap used when YaTeX-prefix key pushed."
)

;---------- Define deafult key bindings on YaTeX mode map ----------
(defun YaTeX-define-key (key binding)
  "Define key on YaTeX-prefix-map"
  (if YaTeX-inhibit-prefix-letter
      (let ((c (aref key 0)))
	(cond
	 ((and (>= c ?a) (<= c ?z)) (aset key 0 (1+ (- c ?a))))
	 ((and (>= c ?A) (<= c ?Z)) (aset key 0 (1+ (- c ?A))))
	 (t nil))))
  (define-key YaTeX-prefix-map key binding)
)
(defun YaTeX-define-begend-key (key env)
  "Define short cut YaTeX-make-begin-end key."
  (YaTeX-define-key
   key
   (list 'lambda '(arg) '(interactive "P")
	 (list 'YaTeX-insert-begin-end env 'arg)))
)
(defun YaTeX-define-begend-region-key (key env)
  "Define short cut YaTeX-make-begin-end-region key."
  (YaTeX-define-key key (list 'lambda nil '(interactive)
			      (list 'YaTeX-insert-begin-end env t)))
)
;;;
;; Define key table
;;;
(if YaTeX-mode-map 
    nil
  (setq YaTeX-mode-map (make-sparse-keymap))
  (setq YaTeX-prefix-map (make-sparse-keymap))
  (define-key YaTeX-mode-map "\"" 'YaTeX-insert-quote)
  (define-key YaTeX-mode-map "{" 'YaTeX-insert-braces)
  (define-key YaTeX-mode-map "(" '(lambda () (interactive)
				    (insert "()") (backward-char 1)))
  ;(define-key YaTeX-mode-map "[" 'YaTeX-insert-brackets)
  (define-key YaTeX-mode-map YaTeX-prefix YaTeX-prefix-map)
  (YaTeX-define-key "t" 'YaTeX-typeset-menu)
  (YaTeX-define-key "'" 'YaTeX-prev-error)
  (YaTeX-define-key "^" 'YaTeX-visit-main)
  (YaTeX-define-key "4^" 'YaTeX-visit-main-other-window)
  (YaTeX-define-key " " 'YaTeX-do-completion)
  (YaTeX-define-key "v" 'YaTeX-version)

  (YaTeX-define-key "}" 'YaTeX-insert-braces-region)
  (YaTeX-define-key "]" 'YaTeX-insert-brackets-region)
  (YaTeX-define-key "d" 'YaTeX-insert-dollar)
  (YaTeX-define-key "i" 'YaTeX-fill-item)
  (YaTeX-define-key
   "\\" '(lambda () (interactive) (YaTeX-insert-string "$\\backslash$")))
  (YaTeX-define-begend-region-key "Bd" "document")
  (YaTeX-define-begend-key "bd" "document")
  (YaTeX-define-begend-region-key "BD" "description")
  (YaTeX-define-begend-key "bD" "description")
  (YaTeX-define-begend-region-key  "Be" "enumerate")
  (YaTeX-define-begend-key  "be" "enumerate")
  (YaTeX-define-begend-region-key  "Bi" "itemize")
  (YaTeX-define-begend-key  "bi" "itemize")
  (YaTeX-define-begend-region-key  "Bt" "tabbing")
  (YaTeX-define-begend-key  "bt" "tabbing")
  (YaTeX-define-begend-region-key  "BT" "tabular")
  (YaTeX-define-begend-key  "bT" "tabular")
  (YaTeX-define-begend-region-key  "Bq" "quote")
  (YaTeX-define-begend-key  "bq" "quote")
  (YaTeX-define-begend-region-key  "BQ" "quotation")
  (YaTeX-define-begend-key  "bQ" "quotation")
  (YaTeX-define-key "." 'YaTeX-comment-paragraph)
  (YaTeX-define-key "," 'YaTeX-uncomment-paragraph)
  (YaTeX-define-key ">" 'YaTeX-comment-region)
  (YaTeX-define-key "<" 'YaTeX-uncomment-region)
  (YaTeX-define-key "B " 'YaTeX-make-begin-end-region)
  (YaTeX-define-key "b " 'YaTeX-make-begin-end)
  (YaTeX-define-key "e" 'YaTeX-end-environment)
  (YaTeX-define-key "s" 'YaTeX-make-section)
  (YaTeX-define-key "L" 'YaTeX-make-fontsize-region)
  (YaTeX-define-key "l" 'YaTeX-make-fontsize)
  (YaTeX-define-key "m" 'YaTeX-make-singlecmd)
  (YaTeX-define-key "g" 'YaTeX-goto-corresponding-*)
  (YaTeX-define-key "k" 'YaTeX-kill-*)
  (YaTeX-define-key "c" 'YaTeX-change-*)
  (YaTeX-define-key "a" 'YaTeX-make-accent)
  (YaTeX-define-key "n"
    '(lambda () (interactive) (YaTeX-insert-string "\\\\")))
  (if (eq system-type 'ms-dos)
      (define-key YaTeX-prefix-map "L"
	'(lambda () (interactive)
	   (set-screen-height YaTeX-saved-screen-height) (recenter))))
)

(if YaTeX-typesetting-mode-map nil
  (setq YaTeX-typesetting-mode-map (make-keymap))
  (suppress-keymap YaTeX-typesetting-mode-map t)
  (define-key YaTeX-typesetting-mode-map " "
    'YaTeX-jump-error-line)
)

;---------- Customize as you like above ----------

;---------- Define other variable ----------
(defvar env-name "document")		;Initial tex-environment completion
(defvar section-name "documentstyle[12pt]") ;Initial tex-section completion
(defvar fontsize-name "large")		;Initial fontsize completion
(defvar single-command "maketitle")	;Initial LaTeX single command
(defvar YaTeX-user-table-has-read nil
  "Flag that means whether user completion table has been read or not."
)
(defvar YaTeX-user-table-modified nil
  "Flag that means whether user completion table has modified or not."
)
(defvar yatex-mode-hook nil
  "*List of functions to be called after .tex file is read
and yatex-mode starts."
)
(defvar YaTeX-kanji-code-alist
  '((1 . *sjis*) (2 . *junet*) (3 . *euc-japan*))
)
(defvar YaTeX-kanji-code (if (eq system-type 'ms-dos) 1 2)
  "*File kanji code used by Japanese TeX."
)
(cond
 ((boundp 'MULE)
  (defvar YaTeX-coding-system
    (symbol-value (cdr (assoc YaTeX-kanji-code YaTeX-kanji-code-alist)))
    "File coding system used by Japanese TeX")
  (if (not (eq system-type 'ms-dos))
      (defvar YaTeX-latex-message-code *autoconv*)))
 ((boundp 'NEMACS)
  (defvar YaTeX-latex-message-code latex-message-kanji-code))
)
;---------- Produce YaTeX-mode ----------
;;;
;; Major mode definition
;;;
(defun yatex-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'YaTeX-mode)
  (setq mode-name (if YaTeX-japan "‚â‚Ä‚Ó" "YaTeX"))
  (make-local-variable 'dvi2-command)
  (cond ((boundp 'MULE) ;;1992/12/21 by NIIMI Satoshi.
	 ;; file-coding-system is buffer local variable
	 (set-file-coding-system  YaTeX-coding-system))
  	((boundp 'NEMACS)
	 (make-local-variable 'kanji-fileio-code)
	 (setq kanji-fileio-code YaTeX-kanji-code)))
  (make-local-variable 'fill-column)
  (make-local-variable 'fill-prefix)
  (setq fill-column YaTeX-fill-column
	fill-prefix YaTeX-fill-prefix)
  (use-local-map YaTeX-mode-map)
  (if (eq system-type 'ms-dos)
	(setq YaTeX-saved-screen-height (screen-height)))
  (if YaTeX-user-table-has-read nil
    (YaTeX-read-user-completion-table)
    (setq YaTeX-user-table-has-read t))
  (run-hooks 'text-mode-hook 'yatex-mode-hook)
)

;---------- Define macro ---------
(defmacro point-beginning-of-line ()
  (save-excursion (beginning-of-line)(point))
)

(defmacro point-end-of-line ()
  (save-excursion (end-of-line)(point))
)

;---------- Define YaTeX-mode functions ----------
;;;
;; YaTeX-mode functions
;;;
(defun YaTeX-insert-begin-end (env arg)
  "Insert \begin{mode-name} and \end{mode-name}."
  (if arg
      (save-excursion
	(if (> (point) (mark)) (exchange-point-and-mark))
	(insert "\\begin{" env "}")
	(YaTeX-addin env)
	(insert "\n")
	(indent-relative-maybe)
	(exchange-point-and-mark)
	(insert "\\end{" env "}\n")
	(indent-relative-maybe))
    ;(delete-blank-lines)
    (let ((i 1))
      (insert "\\begin{" env "}")
      (YaTeX-addin env)
      (insert "\n")
      (indent-relative-maybe)
      ;;(newline (1+ (* 2 YaTeX-open-lines)))
      (while (<= i (1+ (* 2 YaTeX-open-lines)))
	(insert "\n")
	(indent-relative-maybe)
	(setq i (1+ i)))
      (insert "\\end{" env "}")
      (previous-line (+ 1 YaTeX-open-lines)));let i
    (if YaTeX-current-position-register
	(point-to-register YaTeX-current-position-register)))
)

(defun YaTeX-make-begin-end (arg)
  "Make LaTeX environment command of \\begin{env.} ... \\end{env.}
by completing read.
 If you invoke this command with universal argument,
\(key binding for universal-argument is \\[universal-argument]\)
you can put REGION into that environment between \\begin and \\end."
  (interactive "P")
  (let*
      ((mode (if arg " region" ""))
       (env
	(YaTeX-read-environment
	 (format "Begin environment%s(default %s): " mode env-name))))
    (if (string= env "")
	(setq env env-name))
    (setq env-name env)
    (if (not (assoc env-name (append user-env-table env-table))) ;if not exist
	(setq user-env-table (cons (list env-name) user-env-table)
	    YaTeX-user-table-modified t))
    (YaTeX-insert-begin-end env-name arg))
)

(defun YaTeX-make-begin-end-region ()
  "Call YaTeX-make-begin-end with ARG to specify region mode."
  (interactive)
  (YaTeX-make-begin-end t)
)

(defun YaTeX-inner-environment ()
  "Return current inner environment."
  (save-excursion
    (let ((nest 0) s)
      (while
	  (and
	   (>= nest 0)
	   (re-search-backward
	    "\\(\\\\begin{\\).*}\\|\\(\\\\end{\\).*}" (point-min) t))
	(if (re-search-backward "^[ 	]*%" (point-beginning-of-line) t)
	    nil	;ignore TeX comment usage.
	  (setq nest (if (eq (match-beginning 0) (match-beginning 1))
			 (1- nest) (1+ nest)))))
      (if (>= nest 0)
	  nil
	(goto-char (match-end 1))
	(setq s (point))
	(skip-chars-forward "^}")
	(buffer-substring s (point))
      )))
)

(defun YaTeX-end-environment ()
  "Close opening environment"
  (interactive)
  (let ((curp (point))
	(env (YaTeX-inner-environment)))

    (if (not env) (error "No premature environment")
      (save-excursion
	(if (and (re-search-forward "^[^\\%]*\\\\end{.*}" (point-max) t)
		 (progn (goto-char (match-beginning 0))
			(re-search-forward env (match-end 0) t)))
	    (if (y-or-n-p
		 (concat "Environment `" env
			 "' was already closed. Force close?"))
		nil
	      (error "end environment aborted."))))
      (message "")			;Erase (y or n) message.
      (insert "\\end{" env "}")
      (setq curp (point))
      (goto-char (match-end 0))
      (if (pos-visible-in-window-p)
	  (sit-for 1)
	(message (concat "Matches \\begin{" env
			 (format "} at line %d"
				   (count-lines (point-min) (match-end 0))))))
      (goto-char curp))
    )
)

(defun YaTeX-make-section (arg)
  "Make LaTeX \\section{} type command with completing read.
With ARG of numeric, you can specify the number of argument of
LaTeX command.
  For example, if you want to produce LaTeX command

	\\addtolength{\\topmargin}{8mm}

which has two arguments.  You can produce that sequence by typing...
	ESC 2 C-c s add SPC RET \\topm SPC RET 8mm RET
\(by default\)
You can complete symbol at LaTeX command and 1st argument."
  (interactive "p")
  (let*
      ((section
	(completing-read
	 (format "\\???{} (default %s): " section-name)
	 (append user-section-table section-table)
	 nil nil))
       (section (if (string= section "") section-name section))
       (title
	(completing-read (concat "\\" section "{???}: ")
			 (append user-article-table article-table)
			 nil nil)))
    (setq section-name section)
    (if (not (assoc section-name (append user-section-table section-table)))
	(setq user-section-table
	      (cons (list section-name) user-section-table)
	      YaTeX-user-table-modified t))
    (insert "\\" section-name "{" title "}")
    (if YaTeX-current-position-register
	(point-to-register YaTeX-current-position-register))
    (let ((j 2))
      (while (<= j arg)
	(insert (concat "{" (read-string (format "Argument %d: " j))))
	(insert "}")
	(setq j (1+ j)))
      )
    (if (string= title "") (forward-char -1)
      nil))
)

;(defun YaTeX-make-section-region ()
;  "Call YaTeX-make-section with ARG to specify region mode."
; (interactive)
; (YaTeX-make-section t)
;)

(defun YaTeX-make-fontsize (arg)
  "Make completion like {\\large ...} or {\\slant ...} in minibuffer.
If you invoke this command with universal argument, you can put region
into {\\xxx } braces.
\(key binding for universal-argument is \\[universal-argument]\)"
  (interactive "P")
  (let* ((mode (if arg "region" ""))
	 (fontsize
	  (completing-read
	   (format "{\\??? %s} (default %s): " mode fontsize-name)
	   (append user-fontsize-table fontsize-table)
	   nil nil )))
    (if (string= fontsize "")
	(setq fontsize fontsize-name))
    (setq fontsize-name fontsize)
    (if (not (assoc fontsize-name (append user-fontsize-table fontsize-table)))
	(setq user-fontsize-table
	      (cons (list fontsize-name) user-fontsize-table)
	      YaTeX-user-table-modified t))
    (if arg
	(save-excursion
	  (if (> (point) (mark)) (exchange-point-and-mark))
	  (insert "{\\" fontsize-name " ")
	  (exchange-point-and-mark)
	  (insert "}"))
      (insert "{\\" fontsize-name " }")
      (if YaTeX-current-position-register
	  (point-to-register YaTeX-current-position-register))
      (forward-char -1)))
)

(defun YaTeX-make-fontsize-region ()
  "Call function:YaTeX-make-fontsize with ARG to specify region mode."
  (interactive)
  (YaTeX-make-fontsize t)
)

(defun YaTeX-make-singlecmd (single)
  (interactive
   (list (completing-read
	  (format "\\??? (default %s): " single-command)
	  (append user-singlecmd-table singlecmd-table)
	  nil nil )))
  (if (string= single "")
      (setq single single-command))
  (setq single-command single)
  (if (not (assoc single-command
		  (append user-singlecmd-table singlecmd-table)))
      (setq user-singlecmd-table
	    (cons (list single-command) user-singlecmd-table)
	    YaTeX-user-table-modified t))
  (insert "\\" single-command " ")
  (if YaTeX-current-position-register
      (point-to-register YaTeX-current-position-register))
)

(defvar YaTeX-completion-begin-regexp "[{\\]"
  "Regular expression of limit where LaTeX command's
completion begins.")

(defun YaTeX-do-completion ()
  "Try completion on LaTeX command preceding point."
  (interactive)
  (if
      (or (eq (preceding-char) ? )
	  (eq (preceding-char) ?\t)
	  (eq (preceding-char) ?\n)
	  (bobp))
      (message "Nothing to complete.")   ;Do not complete
    (let* ((end (point))
	   (limit (save-excursion (beginning-of-line) (point)))
	   (completion-begin 
	    (progn (re-search-backward "[ \t\n]" limit 1)
		   (point)))
	   (begin (progn
		    (goto-char end)
		    (if (re-search-backward YaTeX-completion-begin-regexp
					    completion-begin t)
			(1+ (point))
		      nil))))
      (goto-char end)
      (cond
       ((null begin)
	(message "I think it is not LaTeX sequence."))
       (t
	(let* ((pattern (buffer-substring begin end))
	       (all-table (append section-table user-section-table
				  article-table user-article-table
				  env-table     user-env-table
				  singlecmd-table user-singlecmd-table))
	       ;; First,
	       ;; search completion without backslash.
	       (completion (try-completion pattern all-table nil)))
	  (if
	      (eq completion nil)
	      ;; Next,
	      ;; search completion with backslash
	      (setq completion
		    (try-completion (buffer-substring (1- begin) end)
				    all-table nil)
		    begin (1- begin)))
	  (cond
	   ((null completion)
	    (message (concat "Can't find completion for '" pattern "'"))
	    (ding))
	   ((eq completion t) (message "Sole completion."))
	   ((not (string= completion pattern))
	    (kill-region begin end)
	    (insert completion)
	    )
	   (t
	    (message "Making completion list...")
	    (with-output-to-temp-buffer "*Help*"
	      (display-completion-list
	       (all-completions pattern all-table))) )
	   ))))))
)

(defun YaTeX-insert-quote ()
  (interactive)
  (insert
   (cond
    ((YaTeX-quick-in-environment-p "verbatim") ?\")
    ((= (preceding-char) ?\\ ) ?\")
    ((= (preceding-char) ?\( ) ?\")
    ((= (preceding-char) 32)  "``")
    ((= (preceding-char) 9)   "``")
    ((= (preceding-char) ?\n) "``")
    ((bobp) "``")
    (t  "''")
)))

(defun YaTeX-insert-braces-region (beg end &optional open close)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (insert (or close "}"))
    (goto-char beg)
    (insert (or open "{")))
)

(defun YaTeX-insert-braces ()
  (interactive)
  (insert "{}")
  (forward-char -1)
)

(defun YaTeX-insert-brackets-region (beg end)
  (interactive "r")
  (save-excursion
    (YaTeX-insert-braces-region beg end "[" "]"))
)

(defun YaTeX-insert-dollar ()
  (interactive)
  (insert "$$")
  (forward-char -1)
)

(defun YaTeX-insert-string (s)
  (insert s)
)

(defun YaTeX-version ()
  "Return string of the version of running YaTeX."
  (interactive)
  (message
   (concat "Yet Another tex-mode "
	   (if YaTeX-japan "u–ì’¹v" "Wild Bird")
	   "Revision "
	   YaTeX-revision-number))
)

(defun YaTeX-typeset-sentinel (proc mes)
  (cond ((null (buffer-name (process-buffer proc)))
         ;; buffer killed
         (set-process-buffer proc nil))
        ((memq (process-status proc) '(signal exit))
         (let* ((obuf (current-buffer)))
           ;; save-excursion isn't the right thing if
           ;;  process-buffer is current-buffer
           (unwind-protect
               (progn
                 ;; Write something in *typesetting* and hack its mode line
		 (if (equal (current-buffer) (process-buffer proc))
		     nil
		   (other-window 1)
		   (switch-to-buffer (process-buffer proc))
		   (goto-char (point-max))
		   (recenter -3)
		   (other-window -1))
		 (set-buffer (process-buffer proc))
                 (goto-char (point-max))
                 (insert ?\n "latex typesetting " mes)
                 (forward-char -1)
                 (insert " at "
                         (substring (current-time-string) 0 -5))
		 (insert "\n * Hit any key to return * ")
                 (forward-char 1)
                 (setq mode-line-process
                       (concat ": "
                               (symbol-name (process-status proc))))
		 (message "latex typesetting done.")
                 ;; If buffer and mode line will show that the process
                 ;; is dead, we can delete it now.  Otherwise it
                 ;; will stay around until M-x list-processes.
                 (delete-process proc)
		 )
             (setq YaTeX-typesetting-process nil)
             ;; Force mode line redisplay soon
             (set-buffer-modified-p (buffer-modified-p))
	     )
	   (set-buffer obuf)
)))
)

(defvar YaTeX-typesetting-process nil
  "Process identifier for jlatex"
)
(defvar YaTeX-typeset-buffer "*YaTeX-typesetting*"
  "Process buffer for jlatex")

(defun YaTeX-typeset (command)
  "Execute jlatex (or other) to LaTeX typeset."
  (interactive)
  (if YaTeX-typesetting-process
   (if (eq (process-status YaTeX-typesetting-process) 'run)
	(progn (interrupt-process YaTeX-typesetting-process)
	       ;(sit-for 1)
	       (delete-process YaTeX-typesetting-process))
      nil) nil)
  (setq YaTeX-typesetting-process nil)
  (if (eq system-type 'ms-dos)				;if MS-DOS
      (with-output-to-temp-buffer YaTeX-typeset-buffer
	(message (concat "Typesetting " (buffer-name) "..."))
	(YaTeX-put-nonstopmode)
	(call-process shell-file-name
		      nil YaTeX-typeset-buffer nil "/c" command)
	(YaTeX-remove-nonstopmode))
    (setq YaTeX-typesetting-process			;if UNIX
	  (with-output-to-temp-buffer YaTeX-typeset-buffer
	    (start-process "LaTeX" YaTeX-typeset-buffer shell-file-name "-c"
			   command)
	    ))
    (set-process-sentinel YaTeX-typesetting-process 'YaTeX-typeset-sentinel))
  (setq current-TeX-buffer (buffer-name))
  (other-window 1)
  (use-local-map YaTeX-typesetting-mode-map)
  (setq mode-name "typeset")
  (if YaTeX-typesetting-process ; if process is running (maybe on UNIX)
      (cond ((boundp 'MULE)
	     (set-current-process-coding-system
	      YaTeX-latex-message-code YaTeX-coding-system))
	    ((boundp 'NEMACS)
	     (set-kanji-process-code YaTeX-latex-message-code))))
  (message "Type SPC to continue.")
  (goto-char (point-max))
  (sit-for 1)
  (if (eq system-type 'ms-dos) (message "") (read-char));hit any key
  (forward-line -1)
  (recenter -1)
  (other-window -1)
)

(defun YaTeX-typeset-region ()
  "Paste the region to the file `texput.tex' and execute jlatex (or other)
to LaTeX typeset.  The region is specified by the rule:
	(1)If keyword `%#BEGIN' is found in the upper direction from (point).
	  (1-1)if the keyword `%#END' is found after `%#BEGIN',
		->Assume the text between `%#BEGIN' and `%#END' as region.
	  (1-2)if the keyword `%#END' is not found anywhere after `%#BEGIN',
		->Assume the text after `%#BEGIN' as region.
	(2)If no `%#BEGIN' usage is found before the (point),
		->Assume the text between current (point) and (mark) as region.
DON'T forget to eliminate the `%#BEGIN/%#END' notation after editing
operation to the region."
  (interactive)
  (save-excursion
    (let*
	((end "") typeout ;Type out message that tells the method of cutting.
	 (buffer (current-buffer)) opoint preamble
	 (region
	  (if (re-search-backward
	       "%#BEGIN" nil t)
	      (progn
		(setq typeout "--- Region from BEGIN to " end "END ---")
		(buffer-substring
		 (match-end 0)
		 (if (re-search-forward "%#END" nil t)
		     (match-beginning 0)
		   (setq end "end of buffer ---")
		   (point-max))))
	    (setq typeout "=== Region from (point) to (mark) ===")
	    (buffer-substring (point) (mark)))))
      (YaTeX-visit-main)
      (setq opoint (point))
      (goto-char (point-min))
      (setq
       preamble
       (if (re-search-forward "^[ 	]*\\\\begin.*{document}" nil t)
	   (buffer-substring (point-min) (match-end 0))
	 (concat "\\documentstyle{" YaTeX-default-document-style "}\n"
		 "\\begin{document}")))
      (goto-char opoint)
      (switch-to-buffer buffer)		;for clarity
      (find-file "texput.tex")
      (erase-buffer)
      (if YaTeX-need-nonstop
	  (insert "\\nonstopmode{}\n"))
      (insert preamble "\n")
      (insert region)
      (insert "\\typeout{" typeout end "}\n") ;Notice the selected method.
      (insert "\n\\end{document}\n")
      (basic-save-buffer)
      (kill-buffer (current-buffer))
      (YaTeX-typeset (concat (YaTeX-get-latex-command nil) " texput.tex"))))
)

(defun YaTeX-typeset-buffer ()
  "Typeset whole buffer."
  (interactive)
  (if (YaTeX-main-file-p) nil
    (let*((me (substring (buffer-name) 0 (rindex (buffer-name) ?.))))
      (save-excursion		;save excursion of current buffer.
	(YaTeX-visit-main)
	(save-excursion		;save excursion of main .tex buffer
	  (push-mark (point) t)
	  (goto-char (point-min))
	  (if (and (re-search-forward "^[ 	]*\\\\begin{document}" nil t)
		   (re-search-backward "^[ 	]*\\\\includeonly{" nil t))
	      (let*
		  ((b (progn (skip-chars-forward "^{") (point)))
		   (e (progn (skip-chars-forward "^}") (1+ (point))))
		   (s (buffer-substring b e)) c)
		(if (string-match (concat "[{,]" me "[,}]") s)
		    nil
		  (ding)
		  (message
  "File:`%s' is not in includeonly list. A)ppend R)eplace %%)comment? " me)
		  (setq c (read-char))
		  (cond
		   ((= c ?a)
		    (goto-char (1+ b))
		    (insert me (if (string= s "{}") "" ",")))
		   ((= c ?r)
		    (delete-region (1+ b) (1- e)) (insert me))
		   ((= c ?%)
		    (beginning-of-line) (insert "%"))
		   (t nil))
		  (basic-save-buffer))))
	  (exchange-point-and-mark))
	)))
  (YaTeX-save-buffers)
  (YaTeX-typeset (YaTeX-get-latex-command t))
)

(defun YaTeX-preview (preview-command preview-file)
  "Execute xdvi (or other) to tex-preview."
  (interactive
   (list (read-string "Preview command: " dvi2-command)
	 (read-string "Prefiew file[.dvi]: "
		      ;;(substring (buffer-name) 0 -4)
		      (YaTeX-get-preview-file-name)	;ver 1.31
	 )))
  (setq dvi2-command preview-command)
  (with-output-to-temp-buffer "*dvi-preview*"
    (if (eq system-type 'ms-dos)
	(progn (send-string-to-terminal "\e[2J")	;if MS-DOS
	       (call-process shell-file-name "con" "*dvi-preview*" nil
			     "/c " dvi2-command preview-file)
	       (redraw-display))
      (start-process "xdvi" "*dvi-preview*" shell-file-name "-c"
		     (concat dvi2-command " " preview-file)) ;if UNIX
      (message (concat "Starting " dvi2-command " to preview " preview-file)))
  )
)

(defun YaTeX-prev-error ()
  "Visit previous error.  The reason why not NEXT-error is to
avoid make confliction of line numbers by editing."
  (interactive)
  (let ((cur-buf (buffer-name))
	YaTeX-error-line error-buffer)
    (if (null (get-buffer YaTeX-typeset-buffer))
	(message "There is no output buffer of typesetting.")
      (pop-to-buffer YaTeX-typeset-buffer)
      (if (eq system-type 'ms-dos)
	  (if (search-backward latex-dos-emergency-message nil t)
	      (progn (goto-char (point-max))
		     (setq error-regexp latex-error-regexp))
	    (beginning-of-line)
	    (forward-char -1)
	    (setq error-regexp latex-warning-regexp))
	(if YaTeX-typesetting-process      ; if jlatex on UNIX
	    (if (eq (process-status YaTeX-typesetting-process) 'run)
		(progn
		  (goto-char (point-max))
		  (setq error-regexp latex-error-regexp)))
	  (beginning-of-line)
	  (setq error-regexp latex-warning-regexp)))
      (if (re-search-backward error-regexp nil t)
	  (save-restriction
	    (set-mark-command nil)
	    (end-of-line)
	    (narrow-to-region (point) (mark))
	    (goto-char (point-min))
	    (re-search-forward "[0-9]")
	    (forward-char -1)
	    (set-mark (point))
	    (skip-chars-forward "0-9")
	    (narrow-to-region (point) (mark))
	    (goto-char (point-min))
	    (setq YaTeX-error-line (read (current-buffer))))
	(message "No more error on %s" cur-buf)
	(ding)
	)
      (setq error-buffer (YaTeX-get-error-file cur-buf))
      (other-window -1)
      (switch-to-buffer cur-buf)
      (if (null YaTeX-error-line)
	  nil
	;; if warning or error found
	(YaTeX-switch-to-buffer error-buffer)
	(goto-line YaTeX-error-line)
	(message "latex error or warning in '%s' at line: %d"
		 error-buffer YaTeX-error-line)
	(other-window 1)
	(skip-chars-backward "[0-9]")
	(recenter (/ (window-height) 2))
	(sit-for 3)
	(forward-line -1)
	(other-window -1)
	)))
)

(defun YaTeX-jump-error-line ()
  "Jump corresponding line on latex command's error message."
  (interactive)
  (let ((end (progn (end-of-line) (point)))
	(begin (progn (beginning-of-line)(point))))
    (if (null (re-search-forward "l[ ines]*\\.*[1-9][0-9]*" end t))
	(message "No line number expression")
      (goto-char (match-beginning 0))
      (re-search-forward "[1-9][0-9]*" end t)
      (save-restriction
	(let ((error-line
	       (string-to-int (buffer-substring (match-beginning 0)
						(match-end 0))))
	      (error-file (YaTeX-get-error-file current-TeX-buffer)))
	  (goto-char (match-beginning 0))
	  (other-window -1)
	  (message "errors in %s" error-file)
	  ;(switch-to-buffer current-TeX-buffer)
	  (if (not (YaTeX-switch-to-buffer error-file))
	      (error "%s is not found in this directory."))
	  (goto-line error-line)))))
)

(defun YaTeX-view-error ()
  (interactive)
  (if (null (get-buffer YaTeX-typeset-buffer))
      (message "No typeset buffer found.")
    (pop-to-buffer YaTeX-typeset-buffer)
    (goto-char (point-max))
    (recenter -1)
    (other-window -1))
)

(defun YaTeX-get-error-file (default)
  "Get current processing file by tex message."
  (let (file-name)
    (save-excursion
      (if (re-search-backward "([-A-Z_a-z0-9]+.tex" (point-min) t)
	  (buffer-substring (1+ (match-beginning 0)) (match-end 0))
	default)))
)
      
(defun YaTeX-put-nonstopmode ()
  (if YaTeX-need-nonstop
      (if (re-search-backward "\\nonstopmode{}" (point-min) t)
	  nil                    ;if already written in text then do nothing
	(save-excursion
	  (goto-char (point-min))
	  (insert "\\nonstopmode{}%_YaTeX_%\n")))
    )
)

(defun YaTeX-remove-nonstopmode ()
  (if YaTeX-need-nonstop ;for speed
      (save-excursion
	(goto-char (point-min))
	(forward-line 1)
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(delete-matching-lines "^\\\\nonstopmode\\{\\}%_YaTeX_%$")
	(widen)))
)

(defun YaTeX-typeset-menu (arg)
  "Typeset, preview, visit error and miscellaneous convinient menu."
  (interactive "P")
  (message "J)latex R)egion P)review V)iewerror L)pr")
  (let ((c (read-char)))
    (cond
     ((= c ?j) (YaTeX-typeset-buffer))
     ((= c ?r) (YaTeX-typeset-region))
     ((= c ?p) (call-interactively 'YaTeX-preview))
     ((= c ?v) (YaTeX-view-error))
     ((= c ?l) (YaTeX-lpr arg))
     ((= c ?b) (YaTeX-insert-string "\\"))))
)

(defun YaTeX-get-preview-file-name ()
  "Get file name to preview by inquiring YaTeX-get-latex-command"
  (let* ((latex-cmd (YaTeX-get-latex-command t))
	 (fname (substring latex-cmd (1+ (rindex latex-cmd ? ))))
	 (period))
    (if (eq fname "")
	(setq fname (substring (buffer-name) 0 -4))
      (setq period (rindex fname ?.))
      (setq fname (substring fname 0 (if (eq -1 period) nil period)))
      ))
)

(defun YaTeX-get-latex-command (switch)
  "Specify the latex-command name and its argument.
If there is a line which begins by string: \"%#!\", the following
strings are assumed to be the latex-command and arguments.  The
default value of latex-command is:
	tex-command (buffer-name)
and if you write \"%#!jlatex\" in the beginning of certain line.
	\"jlatex \" (buffer-name)
will be the latex-command,
and you write \"%#!jlatex main.tex\" on some line and argument SWITCH
is t, then
	\"jlatex main.tex\"
will be given to the shell."
  (let*
      ((default-command
	 (concat tex-command " "
		 (if switch (buffer-name) ""))));default value
    (save-excursion
      (goto-char (point-min))
      (if (null (re-search-forward "^%#!" (point-max) t))
	  default-command
	(skip-chars-forward "%#! 	")
	(if (eolp)
	    default-command
	  (let ((s (point)))
	    (skip-chars-forward "A-z")	;Skip command name
	    ;(setq YaTeX-latex-command (buffer-substring s (point)))
	    (cond
	     ((null switch)
	      (buffer-substring s (point)))
	     ((eolp)			 ;Only return command name
	      (concat (buffer-substring s (point)) " " (buffer-name)))
	     (t(end-of-line)		   ;Change entire command name
	       (buffer-substring s (point))) ;including arguments.
	    ))
	))))
)

(defun YaTeX-get-builtin (key)
  "Read source built-in command of %# usage."
  (save-excursion
    (goto-char (point-min))
    (if (and (search-forward (concat "%#" key) nil t)
	     (not (eolp)))
	(buffer-substring
	 (progn (skip-chars-forward " 	" (point-end-of-line))(point))
	 (point-end-of-line))
      nil))
)

(defun YaTeX-goto-corresponding-environment ()
  "Go to corresponding begin/end enclosure."
  (interactive)
  (if (not (YaTeX-on-begin-end-p)) nil
    (let ((p  (match-end 0)) env (nest 0) regexp re-s
	  (m0 (match-beginning 0))
	  (m1 (match-beginning 1))
	  (m2 (match-beginning 2)))
      (if (not
	   (save-excursion
	     (goto-char p)
	     (search-forward "}" (point-end-of-line) t)))
	  (error "Unterminated brackets for begin/end"))
      (setq env (buffer-substring p (match-beginning 0))) ;get current env
      (if (cond
	   ((equal m0 m1)		;if begin{xxx}
	    (setq regexp (concat "\\(\\\\end{" env "}\\)\\|"
				 "\\(\\\\begin{" env "}\\)"))
	    (fset re-s 're-search-forward))
	   ((equal m0 m2)		;if end{xxx}
	    (setq regexp (concat "\\(\\\\begin{" env "}\\)\\|"
				 "\\(\\\\end{" env "}\\)"))
	    (fset re-s 're-search-backward))
	   (error "Corresponding environment not found."))
	(while (and (>= nest 0) (funcall re-s regexp nil t))
	  (if (eq (match-beginning 0) m0) nil
	    (setq nest (if (eq (match-beginning 0) (match-beginning 1))
			   (1- nest) (1+ nest))))))
      (beginning-of-line));let
    t); if on begin/end line
)

(defun YaTeX-goto-corresponding-file ()
  "Visit or switch buffer of corresponding file, looking at \\input or
\\include or \includeonly on current line."
  (if (not (YaTeX-on-includes-p)) nil
    (beginning-of-line)
    (skip-chars-forward "^{")
    (let ((input-file
	   (concat
	    (buffer-substring (1+ (point))
			      (progn (skip-chars-forward "^ ,}") (point)))
	    ".tex")))
      (YaTeX-switch-to-buffer input-file)
      )
    t);if on \input or \include line.
)

(defun YaTeX-goto-corresponding-BEGIN-END ()
  (if (not (YaTeX-on-BEGIN-END-p)) nil
    (if (cond
	 ((equal (match-beginning 0) (match-beginning 1)) ;if on %#BEGIN
	  (not (search-forward "%#END" nil t)))
	 (t ; if on %#END
	  (not (search-backward "%#BEGIN" nil t))))
	(error "Corresponding %#BEGIN/END not found."))
    (beginning-of-line)
    t)
)

(defun YaTeX-switch-to-buffer (file)
  "Switch to buffer if buffer exists, find file if not."
  (interactive "Fswitch to file: ")
  (if (get-buffer file)
      (progn (switch-to-buffer file) t)
    (if (file-exists-p file)
	(progn (find-file file) t)
      (message "%s was not found in this directory." file)
      nil))
)

(defun YaTeX-switch-to-buffer-other-window (file)
  "Switch to buffer if buffer exists, find file if not."
  (interactive "Fswitch to file: ")
  (if (get-buffer file)
      (progn (switch-to-buffer-other-window file) t)
    (if (file-exists-p file)
	(progn (find-file-other-window file) t)
      (message "%s was not found in this directory." file)
      nil))
)

(defmacro YaTeX-main-file-p ()
  "Return if current buffer is main LaTeX source."
  (string-match (concat "^" (YaTeX-get-preview-file-name) ".tex")(buffer-name))
)

(defun YaTeX-visit-main ()
  "Switch to buffer main LaTeX source."
  (interactive)
  (let ((main-file (YaTeX-get-preview-file-name)))
    (if (string-match (concat "^" main-file ".tex") (buffer-name))
	(message "I think this is main LaTeX source.")
      (YaTeX-switch-to-buffer (concat main-file ".tex"))))
  nil
)

(defun YaTeX-visit-main-other-window ()
  "Switch to buffer main LaTeX source in other window."
  (interactive)
  (if (YaTeX-main-file-p) (message "I think this is main LaTeX source.")
      (YaTeX-switch-to-buffer-other-window
       (concat (YaTeX-get-preview-file-name) ".tex")))
)

(defun YaTeX-on-begin-end-p ()
  (save-excursion
    (beginning-of-line)
    (re-search-forward
     "\\(\\\\begin{\\)\\|\\(\\\\end{\\)" (point-end-of-line) t))
)
(defun YaTeX-on-includes-p ()
  (save-excursion
    (beginning-of-line)
    (re-search-forward "\\(\\(include.*\\)\\|\\(input\\)\\){.*}"
		       (point-end-of-line) t))
)
(defun YaTeX-on-BEGIN-END-p ()
  (save-excursion
    (let ((case-fold-sea nil))
      (beginning-of-line)
      (re-search-forward "\\(%#BEGIN\\)\\|\\(%#END\\)" (point-end-of-line) t)))
)
(defun YaTeX-goto-corresponding-* ()
  "Parse current line and call suitable function."
  (interactive)
  (cond
   ((YaTeX-goto-corresponding-environment))
   ((YaTeX-goto-corresponding-file))
   ((YaTeX-goto-corresponding-BEGIN-END))
   (t (message "I don't know where to go.")))
)

(defun YaTeX-comment-region (alt-prefix)
  "Comment out region by '%'. If you call this function on the 'begin{}' or
'end{}' line, it comments out whole environment"
  (interactive "P")
  (if (not (YaTeX-on-begin-end-p))
      (comment-region
       (if alt-prefix
	   (read-string "Insert prefix: ")
	 YaTeX-comment-prefix))
    (YaTeX-comment-uncomment-env 'comment-region))
)

(defun YaTeX-uncomment-region (alt-prefix)
  "Uncomment out region by '%'."
  (interactive "P")
  (if (not (YaTeX-on-begin-end-p))
      (uncomment-region
       (if alt-prefix (read-string "Remove prefix: ")
	 YaTeX-comment-prefix))
    (YaTeX-comment-uncomment-env 'uncomment-region))
)

(defun YaTeX-comment-uncomment-env (func)
  "Comment or uncomment out one LaTeX environment switching function by FUNC."
  (save-excursion
    (if (eq (match-beginning 0) (match-beginning 2)) ; if on the '\end{}' line
	(YaTeX-goto-corresponding-environment)) ; goto '\begin{}' line
    (beginning-of-line)
    (push-mark (point) t)
    (YaTeX-goto-corresponding-environment)
    (forward-line 1)
    (funcall func YaTeX-comment-prefix t) ; t makes uncomment once
    )
)

(defun YaTeX-mark-environment ()
  "Not implemented yet."
)

(defun YaTeX-comment-paragraph ()
  "Comment out current paragraph."
  (interactive)
  (save-excursion
    (if (YaTeX-on-begin-end-p)
	(progn
	  (beginning-of-line)
	  (insert YaTeX-comment-prefix)
	  (YaTeX-goto-corresponding-environment)
	  (beginning-of-line)
	  (insert YaTeX-comment-prefix))
      (mark-paragraph)
      (if (not (bobp)) (forward-line 1))
      (comment-region "%")))
)

(defun YaTeX-uncomment-paragraph ()
  "Uncomment current paragraph."
  (interactive)
  (save-excursion
    (if (YaTeX-on-begin-end-p)
	(progn
	  (YaTeX-remove-prefix YaTeX-comment-prefix t)
	  (YaTeX-goto-corresponding-environment)
	  (YaTeX-remove-prefix YaTeX-comment-prefix t))
      (let ((prefix fill-prefix))
	(setq fill-prefix "")
	(mark-paragraph)
	(if (not (bobp)) (forward-line 1))
	(uncomment-region "%")
	(setq fill-prefix prefix))))
)

(defun YaTeX-remove-prefix (prefix &optional once)
  "Remove prefix on current line so far as prefix detected. But
optional argument ONCE makes deletion once."
  (interactive "sPrefix:")
  (beginning-of-line)
  (while (re-search-forward (concat "^" prefix) (point-end-of-line) t)
    (replace-match "")
    (if once (end-of-line)))
)

(defun YaTeX-kill-some-pairs (predicate gofunc)
  "Kill some matching pair."
  (interactive)
  (if ;(not (YaTeX-on-begin-end-p)) nil
      (not (funcall predicate)) nil
    (save-excursion
      (push-mark (point) t)
      ;(YaTeX-goto-corresponding-environment)
      (funcall gofunc)
      (beginning-of-line)
      (kill-line 1)
      (exchange-point-and-mark)
      (beginning-of-line)
      (kill-line 1)
    t))
)

(defun YaTeX-read-environment (prompt)
  "Read the LaTeX environment name with completion."
  (let ((env
	 (completing-read prompt (append user-env-table env-table) nil nil)))
    (if (not (assoc env (append user-env-table env-table)))
	(setq user-env-table (cons (list env) user-env-table)
	      YaTeX-user-table-modified t))
  env)
)

(defun YaTeX-change-environment ()
  "Change the name of environment."
  (interactive)
  (if (not (YaTeX-on-begin-end-p)) nil
    (save-excursion
      (let (p env)
	(beginning-of-line)
	(skip-chars-forward "^{")
	(forward-char 1)
	(setq p (point))
	(skip-chars-forward "^}")
	(setq env (buffer-substring p (point)))
	(beginning-of-line)
	(set-mark-command nil)
	(YaTeX-goto-corresponding-environment)
	(setq newenv (YaTeX-read-environment
		      (format "Change environment `%s' to: " env)))
	(if (string= newenv "")
	    (message "Change environment cancelled.")
	  (search-forward (concat "{" env) (point-end-of-line) t)
	  (replace-match (concat "{" newenv))
	  (exchange-point-and-mark)
	  (search-forward (concat "{" env) (point-end-of-line) t)
	  (replace-match (concat "{" newenv)))
	t)))
)

(defun YaTeX-kill-* ()
  "Parse current line and call suitable function."
  (interactive)
  (cond
   ((YaTeX-kill-some-pairs 'YaTeX-on-begin-end-p
			   'YaTeX-goto-corresponding-environment))
   ((YaTeX-kill-some-pairs 'YaTeX-on-BEGIN-END-p
			   'YaTeX-goto-corresponding-BEGIN-END))
   (t (message "I don't know what to kill.")))
)

(defun YaTeX-change-* ()
  "Parse current line and call suitable function."
  (interactive)
  (cond
   ((YaTeX-change-environment))
   (t (message "I don't know what to change.")))
)

(defun YaTeX-addin (name)
  "Check availability of addin function and call it if exists."
  (if (fboundp (intern-soft (concat "YaTeX:" name)))
      (funcall (intern (concat "YaTeX:" name))))
)

(defun YaTeX-in-environment-p (env)
  "Return if current LaTeX environment is ENV."
  (let ((cur-env (YaTeX-inner-environment)) p)
    (cond
     ((atom env) (equal env cur-env))
     ((listp env)
      (while (and env (not p))
	(setq p (equal (car env) cur-env))
	(setq env (cdr env)))
      p)))
)

(defun YaTeX-quick-in-environment-p (env)
  "Check quickly but unsure if current environment is ENV."
  (let ((p (point))q)
    (while (and (not q) (search-backward (concat "\\begin{" env "}")nil t))
      ;;(goto-char (match-beginning 0))
      (if (search-backward "%" (point-beginning-of-line) t) nil
	(setq q t)))
    (if q (setq q (not (re-search-forward
			(concat "^[ 	]*\\\\end{" env "}") p t))))
    (goto-char p)
    q)
)

(defun YaTeX-remove-trailing-comment ()
  "Remove trailing comment in current line."
  (if (re-search-forward "[^\\\\]\\(%\\)" (point-end-of-line) t)
      (delete-region (match-beginning 1) (point-end-of-line)))
)

(defun YaTeX-fill-item ()
  "Fill item in itemize environment."
  (interactive)
  (save-excursion
      (let* ((p (point))
	     (bndry (prog2 (search-backward "\\begin{" nil t) (point)
			   (goto-char p)))
	     (item-term "\\(^$\\)\\|\\(\\\\item\\)\\|\\(\\\\end\\)")
	     fill-prefix start s2 col)
	(end-of-line)
	(if (not (re-search-backward "\\\\item" bndry t))
	    (error "\\item not found."))
	(skip-chars-forward "^ 	" (point-end-of-line))
	(skip-chars-forward " 	" (point-end-of-line))
	(if (not (eolp))  nil
	  (forward-line 1)
	  (skip-chars-forward "	 "))
	(setq start (point-beginning-of-line))
	(setq col (current-column))
	(YaTeX-remove-trailing-comment)	;should restrict to NTT-jTeX?
	(forward-line 1)
	(skip-chars-forward " 	")
	(if (looking-at item-term) nil
	  (delete-region (point) (point-beginning-of-line))
	  (indent-to col)
	  (setq s2 (point))
	  (setq fill-prefix
		(buffer-substring (point-beginning-of-line)(point)))
	  (YaTeX-remove-trailing-comment);should restrict to NTT-jTeX?
	  (re-search-forward item-term nil 1)
	  (beginning-of-line)
	  (push-mark (point) t)
	  (while (> (point) s2)
	    (forward-line -1)
	    (skip-chars-forward "	 ")
	    (delete-region (point) (point-beginning-of-line))
	    (YaTeX-remove-trailing-comment))
	  (fill-region-as-paragraph start (mark))
	  (if NTT-jTeX
	      (while (progn(forward-line -1)(end-of-line) (> (point) start))
		(insert ?%)))
	  (pop-mark))
	))
)

(defun YaTeX-fill-* ()
  "Fill paragraph according to its condition."
  (interactive)
  (cond
   ((YaTeX-fill-item))
   )
)

(defun YaTeX-save-buffers ()
  "Save buffers with `.tex' extension."
  (basic-save-buffer)
  (save-excursion
    (mapcar '(lambda (buf)
	       (set-buffer buf)
	       (if (and (buffer-file-name buf)
			(string-match "\\.tex$" (buffer-file-name buf))
			(buffer-modified-p buf)
			(y-or-n-p (format "Save %s" (buffer-name buf))))
		   (save-buffer buf)))
	    (buffer-list)))
)

(defun YaTeX-read-accent-char (x)
  "Read char in accent braces."
  (let ((c (read-char)))
    (concat
     (if (and (or (= c ?i) (= c ?j))
	      (not (string-match (regexp-quote x) "cdb")))
	 "\\" "")
     (char-to-string c)))
)

(defun YaTeX-make-accent ()
  "Make accent usage."
  (interactive)
  (message "1:` 2:' 3:^ 4:\" 5:~ 6:= 7:. u v H t c d b")
  (let ((c (read-char))(case-fold-search nil))
    (setq c (cond ((and (> c ?0) (< c ?8))
		   (substring "`'^\"~=." (1- (- c ?0)) (- c ?0)))
		  ((= c ?h) "H")
		  (t (char-to-string c))))
    (if (not (string-match c "`'^\"~=.uvHtcdb")) nil
      (insert "\\" c "{}")
      (backward-char 1)
      (insert (YaTeX-read-accent-char c))
      (if (string= c "t") (insert (YaTeX-read-accent-char c)))
      (forward-char 1)))
)

(defun YaTeX-replace-format (string format repl)
  "In STRING, replace first appearance of FORMAT to REPL as if
function `format' does.  FORMAT does not contain `%'"
  (let ((beg (or (string-match (concat "^\\(%" format "\\)") string)
		 (string-match (concat "[^%]\\(%" format "\\)") string)))
	(len (length format)))
    (if (null beg) string ;no conversion
      (concat
       (substring string 0 (match-beginning 1)) repl
       (substring string (match-end 1)))))
)
(defun YaTeX-lpr (arg)
  "Print out.  If prefix arg ARG is non nil, call print driver without
page range description."
  (interactive "P")
  (let*(from to (cmd (or (YaTeX-get-builtin "LPR") dviprint-command-format)))
    (setq
     cmd (YaTeX-replace-format
	  cmd
	  "f"
	  (if arg
	      ""
	    (YaTeX-replace-format
	     dviprint-from-format
	     "b"
	     (if (string=
		  (setq from (read-string "From page(default 1): ")) "")
		 "1" from)))))
    (setq
     cmd (YaTeX-replace-format
	  cmd
	  "t"
	  (if (or arg
		  (string= 
		   (setq to (read-string "To page(default none): ")) ""))
	      ""
	    (YaTeX-replace-format dviprint-to-format "e" to))))
    (setq cmd (read-string "Edit command line: "
			   (format cmd (YaTeX-get-preview-file-name))))
    (with-output-to-temp-buffer "*dvi-printing*"
      (if (eq system-type 'ms-dos)
	  (call-process shell-file-name "con" "*dvi-printing*" nil
			"/c " cmd)
	(start-process "print" "*dvi-printing*" shell-file-name "-c" cmd)
	(message (concat "Starting " cmd " to printing "
			 (YaTeX-get-preview-file-name))))
    ))
)

(defun YaTeX-read-user-completion-table ()
  "Append user completion table of LaTeX word"
  (message "Loading personal completion table")
  (let ((user-table (expand-file-name YaTeX-user-completion-table)))
    (if (file-exists-p user-table)
	(load-file user-table)
      (message "Personal completion table not found."))
))

(defun YaTeX-save-table ()
  "Save personal completion table as dictionary."
  (interactive)
  (if (not YaTeX-user-table-modified)
      nil
    (message "Saving user table in %s" YaTeX-user-completion-table)
    (find-file (expand-file-name YaTeX-user-completion-table))
    (erase-buffer)
    ;;  (prin1-to-string user-section-table)
    (insert "(setq user-section-table '(\n")
    (mapcar '(lambda (s)
	       (insert (prin1-to-string s))
	       (insert "\n"))
	    user-section-table)
    (insert "))\n\n")
    
    (insert "(setq user-article-table '(\n")
    (mapcar '(lambda (s)
	       (insert (prin1-to-string s))
	       (insert "\n"))
	    user-article-table)
    (insert "))\n\n")
    
    (insert "(setq user-env-table '(\n")
    (mapcar '(lambda (s)
	       (insert (prin1-to-string s))
	       (insert "\n"))
	    user-env-table)
    (insert "))\n\n")
    
    (insert "(setq user-fontsize-table '(\n")
    (mapcar '(lambda (s)
	       (insert (prin1-to-string s))
	       (insert "\n"))
	    user-fontsize-table)
    (insert "))\n\n")
    
    (insert "(setq user-singlecmd-table '(\n")
    (mapcar '(lambda (s)
	       (insert (prin1-to-string s))
	       (insert "\n"))
	    user-singlecmd-table)
    (insert "))\n")
    
    (basic-save-buffer)
    (kill-buffer (current-buffer))
    (message "")
    (setq YaTeX-user-table-modified nil))
)

;; --------------- General sub functions ---------------

;(defun index (string char)
;  (let ((pos 0)(len (1- (length string)))(index -1))
;    (while (<= pos len)
;      (cond
;       ((= (aref string pos) char)
;	(setq index pos) (setq pos len))
;       (t (setq pos (1+ pos))))
;      )
;    index)
;)

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

(defun append-to-hook (hook hook-list)
  "Add hook-list to certain emacs's hook correctly.
Argument hook-list is the list of function int the form to be called
Call this function with argument as next example,
	(append-to-hook '((ding))) ;If one function to add.
	(append-to-hook '((func1)(func2 arg)))."
  (if (null (symbol-value hook))   			;Not defined
      (set hook
	   (append '(lambda ()) hook-list))
    (if (listp (symbol-value hook))
	(if (eq (car (symbol-value hook)) 'lambda)	;'(lambda () ....)
	    (set hook
		 (append (symbol-value hook) hook-list))
	  (if (eq hook 'kill-emacs-hook)	;'(hook1 hook2 ...)
	      (progn				; this format is not
		(ding)				; for kill-emacs-hook
		(message
		 "Caution!! you have wrong format of kill-emacs-hook"))
	    (while (not (null hook-list))
	      (set hook
		   (append (symbol-value hook) (car hook-list)))
	      (setq hook-list (cdr hook-list))))
	  )
      (set hook					;'hook
	   (append '(lambda ())
		   (cons (list (symbol-value hook)) hook-list)))))
)
(append-to-hook 'kill-emacs-hook '((YaTeX-save-table)))

;--------------------------------- History ---------------------------------
; Rev. |   Date   | Contents
;------+----------+---------------------------------------------------------
; 1.00 | 91/ 6/13 | Initial version.
;      |          | Auto compilation & preview.
;      |          | \section{}-type and \begin{}\end{}-type completion.
; 1.01 | 91/ 6/14 | Add {\large ..} type completion (prefix+l).
; 1.10 |     6/21 | Add learning feature of completion.
; 1.11 |     6/27 | Simplify function begin-document etc. using lambda.
; 1.12 |     7/ 6 | Modify YaTeX-make-section, show section-name.
; 1.13 |    12/ 4 | Delete blank lines in make begin/end environment.
; 1.20 |    12/ 5 | Saving learned completion into user file.
; 1.21 |    12/ 6 | Add \maketitle type completion (prefix+m).
; 1.22 |    12/30 | Port yatex.el to DOS(Demacs).
; 1.23 | 92/ 1/ 8 | Enable latex and preview command on DOS.
; 1.24 |     1/ 9 | Add YaTeX-save-table to kill-emacs-hook automatically.
; 1.25 |     1/16 | YaTeX-do-completion (prefix+SPC) and argument
;      |          | acceptable YaTeX-make-section work. Put region into
;      |          | \begin...\end by calling YaTeX-make-begin-end with ARG.
;      |          | append-kill-emacs-hook was revised to append-to-hook.
; 1.26 |     1/18 | Region mode is added to {\large }. Default fontsize.
; 1.27 |     1/21 | Default name on completing-read.
; 1.28 |     7/ 2 | Add \nonstopmode{} automatically on DOS.
;      |     7/20 | %#! usage to specify latex command and its arguments.
;      |          | Change default fill-prefix from TAB to null string.
; 1.29 |     7/21 | Add YaTeX-end-environment.
; 1.30 |     9/26 | Support project 30 lines(other than 25 lines).
; 1.31 |    10/28 | Variable argument for previewer from %#! usage.
; 1.32 |    11/16 | YaTeX-goto-corresponding-environment.
;      |          | Comment out region/paragraph added.
; 1.33 |    11/29 | Variable default value, on DOS and other OS.
;      |          | Make dvi2-command buffer local.  Change the behavior of
;      |          | comment out region/paragraph on the \begin{} or \end{}
;      |          | line.  Make YaTeX-end-environment faster. Add YaTeX-
;      |          | define-key, YaTeX-define-begend-(region-)key.
; 1.34 |    12/26 | YaTeX-goto-corresponding-* automatically choose its move.
;      |          | YaTeX-prev-error supports separate typesetting.
; 1.35 | 93/ 1/25 | YaTeX-kill-environment erases pair of begin/end.
;      |          | YaTeX-change-environment change the environment name.
;      |          | Auto indent at YaTeX-make-begin-end.
; 1.36 |     1/27 | YaTeX-typeset-region typesets the region from %#BEGIN to
;      |          | %#END, or simple region between point and mark.
; 1.37 |     2/12 | YaTeX-kill-environment turns YaTeX-kill-some-pairs and
;      |          | now it can kill %#BEGIN and %#END pairs.
;      |          | Now YaTeX-goto-corresponding-environment detects nested
;      |          | environment.  Put " by " in verbatim.  Auto save buffers
;      |          | with quiery.  Add current file to includeonly list
;      |          | automatically.  Support YaTeX-fill-item, YaTeX-make-
;      |          | accent, YaTeX-visit-main-other-window.
;      |          | [prefix] tl for lpr.  Revise YaTeX-view-error.
;------+----------+---------------------------------------------------------
;
;----------------------------- End of yatex.el -----------------------------

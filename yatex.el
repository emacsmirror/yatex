;;; -*- Emacs-Lisp -*-
;;; Yet Another tex-mode for emacs.
;;; yatex.el rev. 1.63
;;; (c )1991-1997 by HIROSE Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Fri Jan 24 17:57:14 1997 on supra
;;; $Id$

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'comment)
(defconst YaTeX-revision-number "1.63"
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
(defvar YaTeX-prefix "\C-c"
  "*Prefix key to call YaTeX functions.
You can select favorite prefix key by setq in your ~/.emacs."
)
(defvar YaTeX-environment-indent 1
  "*Indentation depth at column width in LaTeX environments."
)
(defvar YaTeX-fill-prefix ""
  "*fill-prefix used for auto-fill-mode.
The default value is null string."
)
(defvar YaTeX-fill-column 72
  "*fill-column used for auto-fill-mode."
)
(defvar YaTeX-comment-prefix "%"
  "TeX comment prefix."
)
(defvar YaTeX-current-position-register ?3
  "*Position register to keep where the last completion was done.
All of YaTeX completing input store the current position into
the register YaTeX-current-position-register.  So every time you
make a trip to any other part of text other than you are writing, you can
return to the editing paragraph by calling register-to-point with argument
YaTeX-current-position-register."
)
(defvar YaTeX-dos (eq system-type 'ms-dos))
(defvar YaTeX-emacs-19 (string= "19" (substring emacs-version 0 2)))
(defvar YaTeX-user-completion-table
  (if YaTeX-dos "~/_yatexrc" "~/.yatexrc")
  "*Default filename in which user completion table is saved."
)
;;(defvar YaTeX-tmp-dic-unit 'main-file
;;  "*Default switching unit of temporary dictionary.
;;There are two switching unit:
;;'main-file	: switch tmp-dic according to main-file directory.
;;'directory	: switch tmp-dic dir by dir."
;;)
(defvar YaTeX-japan (or (boundp 'NEMACS) (boundp 'MULE))
  "Whether yatex mode is running on Japanese environment or not."
)
(defvar tex-command (if YaTeX-japan "jlatex" "latex")
  "*Default command for typesetting LaTeX text."
)
(defvar bibtex-command (if YaTeX-japan "jbibtex" "bibtex")
  "*Default command of BibTeX."
)
(defvar dvi2-command		;previewer command for your site
  (if YaTeX-dos "dviout -wait=0"
    "xdvi -geo +0+0 -s 4")
  "*Default previewer command including its option.
This default value is for X window system."
)
(defvar makeindex-command (if YaTeX-dos "makeind" "makeindex")
  "*Default makeindex command."
)
(defvar dviprint-command-format
  (if YaTeX-dos "dviprt %s %f%t"
      "dvi2ps %f %t %s | lpr")
  "*Command line string to print out current file.
Format string %s will be replaced by the filename.  Do not forget to
specify the `from usage' and `to usage' with their option by format string
%f and %t.
  See also documentation of dviprint-from-format and dviprint-to-format."
)
(defvar dviprint-from-format
  (if YaTeX-dos "%b-" "-f %b")
  "*`From' page format of dvi filter.  %b will turn to beginning page number."
)
(defvar dviprint-to-format
  (if YaTeX-dos "%e" "-t %e")
  "*`To' page format of dvi filter.  %e will turn to end page number."
)
(defvar YaTeX-default-document-style
  (concat (if YaTeX-japan "j") "article")
  "*Default LaTeX Documentstyle for YaTeX-typeset-region."
)
(defvar YaTeX-need-nonstop nil
  "*T for adding `\\nonstopmode{}' to text before invoking latex command."
)
(defvar latex-warning-regexp "line.* [0-9]*"
  "*Regular expression of line number of warning message by latex command."
)
(defvar latex-error-regexp "l\\.[1-9][0-9]*"
  "*Regular expression of line number of latex error.
Perhaps your latex command stops at this error message with line number of
LaTeX source text."
)
(defvar latex-dos-emergency-message
  "Emergency stop"      ;<- for Micro tex, ASCII-pTeX 1.6
  "Message pattern of emergency stop of typesetting.
Because Demacs (GNU Emacs on DOS) cannot have concurrent process, the
latex command which is stopping on a LaTeX error, is terminated by Demacs.
Many latex command on DOS display some messages when it is terminated by
other process, user or OS.  Define to this variable a message string of your
latex command on DOS shown at abnormal termination.
  Remember Demacs's call-process function is not oriented for interactive
process."
)
(defvar latex-message-kanji-code 2
  "*Kanji coding system latex command types out.
1 = Shift JIS, 2 = JIS, 3 = EUC."
)
(defvar YaTeX-inhibit-prefix-letter nil
  "*T for changing key definitions from [prefix] Letter to [prefix] C-Letter."
)
(defvar NTT-jTeX nil
  "*Use NTT-jTeX for latex command."
)
(defvar YaTeX-item-regexp (concat (regexp-quote "\\") "\\(sub\\)*item")
  "*Regular expression of item command."
)
(defvar YaTeX-nervous t
  "*If you are nervous about maintenance of yatexrc, set this value to T.
And you will have the local dictionary."
)
(defvar YaTeX-sectioning-regexp
  "\\(part\\|chapter\\*?\\|\\(sub\\)*\\(section\\|paragraph\\)\\*?\\)\\b"
  "*LaTeX sectioning commands regexp."
)
(defvar YaTeX-paragraph-start
  (concat "^[ \t]*%\\|^[ \t]*$\\|\\'\\|^\C-l\\|\\\\\\\\$\\|^[ \t]*\\\\\\("
	  YaTeX-sectioning-regexp		;sectioning commands
	  "\\|[A-z]*item\\|begin{\\|end{"	;special declaration
	  "\\|newpage\\b\\|vspace\\b"
	  "\\)")
  "*Paragraph starting regexp of common LaTeX source.  Use this value
for YaTeX-uncomment-paragraph."
)
(defvar YaTeX-paragraph-separate
  (concat "^[ \t]*%\\|^[ \t]*$\\|^\C-l\\|\\\\\\\\$\\|^[ \t]*\\\\\\("
	  YaTeX-sectioning-regexp		;sectioning commands
	  "\\|begin{\\|end{"			;special declaration
	  "\\|newpage\\b\\|vspace\\b"
	  "\\)")
  "*Paragraph delimiter regexp of common LaTeX source.  Use this value
for YaTeX-uncomment-paragraph."
)
(defvar YaTeX-verbatim-environments 
  '("verbatim" "verbatim*")
  "*Assume these environments of this variable disable LaTeX commands.")
(defvar YaTeX-verb-regexp "verb\\*?\\|path"
  "*Regexp of verb family.  Do not contain preceding \\\\ nor \\(\\).")
(defvar YaTeX-fill-inhibit-environments
  (append '("tabular" "tabular*" "array" "picture" "eqnarray" "eqnarray*"
	    "equation" "math" "displaymath")
	  YaTeX-verbatim-environments)
  "*In these environments, YaTeX inhibits fill-paragraph from formatting.
Define those environments as a form of list."
)
(defvar YaTeX-itemizing-env-regexp
  "itemize\\|enumerate\\|description\\|list"
  "*Regexp of itemizing environments")
(defvar YaTeX-equation-env-regexp
  "array\\*?\\|equation\\*?"
  "*Regexp of environments for equations")
(defvar YaTeX-array-env-regexp
  (concat
   "array\\*?\\|eqnarray\\*?\\|tabbing\\|tabular\\*?\\|"	;LaTeX
   "align\\*?\\|split\\*?\\|aligned\\*?\\|alignat\\*?\\|"	;AMS-LaTeX
   "xalignat\\*?\\|xxalignat\\*?")				;AMS-LaTeX
  "*Regexp of environments where `&' becomes field delimiter.")
(defvar YaTeX-uncomment-once t
  "*T for removing all continuous commenting character(%).
Nil for removing only one commenting character at the beginning-of-line."
)
(defvar YaTeX-default-pop-window-height 10
  "Default typesetting buffer height.
If integer, sets the window-height of typesetting buffer.
If string, sets the percentage of it.
If nil, use default pop-to-buffer."
)
(defvar YaTeX-close-paren-always t
  "Close parenthesis always when YaTeX-modify-mode is nil."
)
(defvar YaTeX-no-begend-shortcut nil
  "*T for disabling shortcut of begin-type completion, [prefix] b d, etc."
)
(defvar YaTeX-greek-by-maketitle-completion nil
  "*T for greek letters completion by maketitle-type completion."
)
(defvar YaTeX-auto-math-mode t
  "*T for changing YaTeX-math mode automatically.")
(defvar YaTeX-use-AMS-LaTeX nil
  "*T for using AMS-LaTeX"
)
(defvar yatex-mode-hook nil
  "*List of functions to be called at the end of yatex-mode initializations."
)
;;-- Math mode values --

(defvar YaTeX-math-key-list-default
  '((";" . YaTeX-math-sign-alist)
    (":" . YaTeX-greek-key-alist))
  "Default key sequence to invoke math-mode's image completion."
)
(defvar YaTeX-math-key-list-private nil
  "*User defined alist, math-mode-prefix vs completion alist."
)
(defvar YaTeX-math-key-list
  (append YaTeX-math-key-list-private YaTeX-math-key-list-default)
  "Key sequence to invoke math-mode's image completion."
)
(defvar YaTeX-create-file-prefix-g nil
  "*Non-nil creates new file when [prefix] g on \\include{foo}."
)
(defvar YaTeX-skip-default-reader nil
  "Non-nil skips default argument reader of section-type completion."
)
(defvar YaTeX-simple-messages nil
  "Non-nil makes minibuffer messages simpler."
)
(defvar YaTeX-addin-prefix "YaTeX:")
;------------ Completion table ------------
; Set tex-section-like command possible completion
(defvar section-table
  '(("part") ("chapter") ("section") ("subsection")
    ("subsubsection") ("paragraph") ("subparagraph")
    ("author") ("thanks") ("documentstyle") ("pagestyle")
    ("title") ("underline") ("label") ("makebox")
    ("footnote") ("footnotetext")
    ("hspace*") ("vspace*") ("bibliography") ("bibitem") ("cite")
    ("input") ("include") ("includeonly") ("mbox") ("hbox") ("caption")
    ("newlength") ("setlength" 2) ("addtolength" 2) ("settowidth" 2)
    ("setcounter" 2) ("addtocounter" 2) ("stepcounter" 2)
    ("newcommand" 2) ("renewcommand" 2)
    ("setcounter" 2) ("newenvironment" 3) ("newtheorem" 2)
    ("cline") ("framebox") ("savebox" 2) ("date") ("put") ("ref")
    ("frac" 2) ("multicolumn" 3) ("shortstack")
    )
  "Default completion table for section-type completion."
)
(defvar user-section-table nil)
(defvar tmp-section-table nil)

; Set tex-environment possible completion
(defvar env-table
  '(("quote") ("quotation") ("center") ("verse") ("document")
    ("verbatim") ("itemize") ("enumerate") ("description")
    ("list") ("tabular") ("tabular*") ("table") ("tabbing") ("titlepage")
    ("sloppypar") ("quotation") ("picture") ("displaymath")
    ("eqnarray") ("figure") ("equation") ("abstract") ("array")
    ("thebibliography") ("theindex") ("flushleft") ("flushright")
    ("minipage")
    )
  "Default completion table for begin-type completion."
)
(defvar user-env-table nil)
(defvar tmp-env-table nil)

; Set {\Large }-like completion
(defvar fontsize-table
  '(("rm") ("em") ("bf") ("boldmath") ("it") ("sl") ("sf") ("sc") ("tt")
    ("dg") ("dm")
    ("tiny") ("scriptsize") ("footnotesize") ("small")("normalsize")
    ("large") ("Large") ("LARGE") ("huge") ("Huge")
    )
  "Default completion table for large-type completion."
)
(defvar user-fontsize-table nil)
(defvar tmp-fontsize-table nil)

(defvar singlecmd-table
  (append
   '(("maketitle") ("sloppy") ("protect")
     ("LaTeX") ("TeX") ("item") ("item[]") ("appendix") ("hline")
     ;;("rightarrow") ("Rightarrow") ("leftarrow") ("Leftarrow")
     ("pagebreak") ("newpage") ("clearpage") ("cleardoublepage")
     ("footnotemark") ("verb") ("verb*")
     ("linebreak") ("pagebreak") ("noindent") ("indent")
     ("left") ("right")
     )
   (if YaTeX-greek-by-maketitle-completion
       '(("alpha") ("beta") ("gamma") ("delta") ("epsilon")
	 ("varepsilon") ("zeta") ("eta") ("theta")("vartheta")
	 ("iota") ("kappa") ("lambda") ("mu") ("nu") ("xi") ("pi")
	 ("varpi") ("rho") ("varrho") ("sigma") ("varsigma") ("tau")
	 ("upsilon") ("phi") ("varphi") ("chi") ("psi") ("omega")
	 ("Gamma") ("Delta") ("Theta") ("Lambda")("Xi") ("Pi")
	 ("Sigma") ("Upsilon") ("Phi") ("Psi") ("Omega"))))
  "Default completion table for maketitle-type completion."
)
(defvar user-singlecmd-table nil)
(defvar tmp-singlecmd-table nil)

;---------- Key mode map ----------
;;;
;; Create new key map: YaTeX-mode-map
;; Do not change this section.
;;;
(defvar YaTeX-mode-map nil
  "Keymap used in YaTeX mode"
)
(defvar YaTeX-typesetting-mode-map nil
  "Keymap used in YaTeX typesetting buffer"
)
(defvar YaTeX-prefix-map nil
  "Keymap used when YaTeX-prefix key pushed"
)
(defvar YaTeX-user-extensional-map (make-sparse-keymap)
  "*Keymap used for the user's customization")
(defvar YaTeX-current-completion-type nil
  "Has current completion type.  This may be used in YaTeX addin functions."
)
(defvar YaTeX-modify-mode nil
  "*Current editing mode.
When non-nil, each opening parentheses only opens,
nil enters both open/close parentheses when opening parentheses key pressed."
)
(defvar YaTeX-math-mode nil
  "Holds whether current mode is math-mode."
)
;---------- Define default key bindings on YaTeX mode map ----------
(defun YaTeX-define-key (key binding &optional map)
  "Define key on YaTeX-prefix-map."
  (if YaTeX-inhibit-prefix-letter
      (let ((c (aref key 0)))
	(cond
	 ((and (>= c ?a) (<= c ?z)) (aset key 0 (1+ (- c ?a))))
	 ((and (>= c ?A) (<= c ?Z) (numberp YaTeX-inhibit-prefix-letter))
	  (aset key 0 (1+ (- c ?A))))
	 (t nil))))
  (define-key (or map YaTeX-prefix-map) key binding)
)
(defun YaTeX-define-begend-key-normal (key env &optional map)
  "Define short cut YaTeX-make-begin-end key."
  (YaTeX-define-key
   key
   (list 'lambda '(arg) '(interactive "P")
	 (list 'YaTeX-insert-begin-end env 'arg))
   map)
)
(defun YaTeX-define-begend-region-key (key env &optional map)
  "Define short cut YaTeX-make-begin-end-region key."
  (YaTeX-define-key key (list 'lambda nil '(interactive)
			      (list 'YaTeX-insert-begin-end env t)) map)
)
(defun YaTeX-define-begend-key (key env &optional map)
  "Define short cut key for begin type completion both for normal
and region mode.  To customize YaTeX, user should use this function."
  (YaTeX-define-begend-key-normal key env map)
  (if YaTeX-inhibit-prefix-letter nil
    (YaTeX-define-begend-region-key
     (concat (upcase (substring key 0 1)) (substring key 1)) env))
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
  (define-key YaTeX-mode-map "(" 'YaTeX-insert-parens)
  (define-key YaTeX-mode-map "$" 'YaTeX-insert-dollar)
  (define-key YaTeX-mode-map "&" 'YaTeX-insert-amper)
  (define-key YaTeX-mode-map "[" 'YaTeX-insert-brackets)
  (define-key YaTeX-mode-map YaTeX-prefix YaTeX-prefix-map)
  (define-key YaTeX-mode-map "\M-\C-@" 'YaTeX-mark-environment)
  (define-key YaTeX-mode-map "\M-\C-a" 'YaTeX-beginning-of-environment)
  (define-key YaTeX-mode-map "\M-\C-e" 'YaTeX-end-of-environment)
  (define-key YaTeX-mode-map "\M-\C-m" 'YaTeX-intelligent-newline)
  (define-key YaTeX-mode-map "\C-i" 'YaTeX-indent-line)
  (YaTeX-define-key "%" 'YaTeX-%-menu)
  (YaTeX-define-key "t" 'YaTeX-typeset-menu)
  (YaTeX-define-key "w" 'YaTeX-switch-mode-menu)
  (YaTeX-define-key "'" 'YaTeX-prev-error)
  (YaTeX-define-key "^" 'YaTeX-visit-main)
  (YaTeX-define-key "4^" 'YaTeX-visit-main-other-window)
  (YaTeX-define-key "4g" 'YaTeX-goto-corresponding-*-other-window)
  (YaTeX-define-key "44" 'YaTeX-switch-to-window)
  (and YaTeX-emacs-19 window-system
       (progn
	 (YaTeX-define-key "5^" 'YaTeX-visit-main-other-frame)
	 (YaTeX-define-key "5g" 'YaTeX-goto-corresponding-*-other-frame)
	 (YaTeX-define-key "55" 'YaTeX-switch-to-window)))
  (YaTeX-define-key " " 'YaTeX-do-completion)
  (YaTeX-define-key "v" 'YaTeX-version)

  (YaTeX-define-key "}" 'YaTeX-insert-braces-region)
  (YaTeX-define-key "]" 'YaTeX-insert-brackets-region)
  (YaTeX-define-key ")" 'YaTeX-insert-parens-region)
  (YaTeX-define-key "$" 'YaTeX-insert-dollars-region)
  (YaTeX-define-key "i" 'YaTeX-fill-item)
  (YaTeX-define-key
   "\\" '(lambda () (interactive) (insert "$\\backslash$")))
  (if YaTeX-no-begend-shortcut
      (progn
	(YaTeX-define-key "B" 'YaTeX-make-begin-end-region)
	(YaTeX-define-key "b" 'YaTeX-make-begin-end))
    (YaTeX-define-begend-key "bc" "center")
    (YaTeX-define-begend-key "bd" "document")
    (YaTeX-define-begend-key "bD" "description")
    (YaTeX-define-begend-key "be" "enumerate")
    (YaTeX-define-begend-key "bE" "equation")
    (YaTeX-define-begend-key "bi" "itemize")
    (YaTeX-define-begend-key "bl" "flushleft")
    (YaTeX-define-begend-key "bm" "minipage")
    (YaTeX-define-begend-key "bt" "tabbing")
    (YaTeX-define-begend-key "bT" "tabular")
    (YaTeX-define-begend-key "b\^t" "table")
    (YaTeX-define-begend-key "bp" "picture")
    (YaTeX-define-begend-key "bq" "quote")
    (YaTeX-define-begend-key "bQ" "quotation")
    (YaTeX-define-begend-key "br" "flushright")
    (YaTeX-define-begend-key "bv" "verbatim")
    (YaTeX-define-begend-key "bV" "verse")
    (YaTeX-define-key "B " 'YaTeX-make-begin-end-region)
    (YaTeX-define-key "b " 'YaTeX-make-begin-end))
  (YaTeX-define-key "e" 'YaTeX-end-environment)
  (YaTeX-define-key "S" 'YaTeX-make-section-region)
  (YaTeX-define-key "s" 'YaTeX-make-section)
  (YaTeX-define-key "L" 'YaTeX-make-fontsize-region)
  (YaTeX-define-key "l" 'YaTeX-make-fontsize)
  (YaTeX-define-key "m" 'YaTeX-make-singlecmd)
  (YaTeX-define-key "." 'YaTeX-comment-paragraph)
  (YaTeX-define-key "," 'YaTeX-uncomment-paragraph)
  (YaTeX-define-key ">" 'YaTeX-comment-region)
  (YaTeX-define-key "<" 'YaTeX-uncomment-region)
  (YaTeX-define-key "g" 'YaTeX-goto-corresponding-*)
  (YaTeX-define-key "k" 'YaTeX-kill-*)
  (YaTeX-define-key "c" 'YaTeX-change-*)
  (YaTeX-define-key "a" 'YaTeX-make-accent)
  (YaTeX-define-key "?" 'YaTeX-help)
  (YaTeX-define-key "/" 'YaTeX-apropos)
  (YaTeX-define-key "&" 'YaTeX-what-column)
  (YaTeX-define-key "d" 'YaTeX-display-hierarchy)
  (YaTeX-define-key "x" YaTeX-user-extensional-map)
  (YaTeX-define-key "n"
    '(lambda () (interactive) (insert "\\\\")))
  (if YaTeX-dos
      (define-key YaTeX-prefix-map "\C-r"
	'(lambda () (interactive)
	   (set-screen-height YaTeX-saved-screen-height) (recenter))))
  (mapcar
   (function
    (lambda (key)
      (define-key YaTeX-mode-map (car key) 'YaTeX-math-insert-sequence)))
   YaTeX-math-key-list)
)

(if YaTeX-typesetting-mode-map nil
  (setq YaTeX-typesetting-mode-map (make-keymap))
  ;(suppress-keymap YaTeX-typesetting-mode-map t)
  (define-key YaTeX-typesetting-mode-map " " 'YaTeX-jump-error-line)
  (define-key YaTeX-typesetting-mode-map "\C-m" 'YaTeX-send-string)
)

(defvar YaTeX-section-completion-map nil
  "*Key map used at YaTeX completion in the minibuffer.")
(if YaTeX-section-completion-map nil
  (setq YaTeX-section-completion-map
	(copy-keymap (or (and (boundp 'gmhist-completion-map)
			      gmhist-completion-map)
			 minibuffer-local-completion-map)))
  (define-key YaTeX-section-completion-map
    " " 'YaTeX-minibuffer-complete)
  (define-key YaTeX-section-completion-map
    "\C-i" 'YaTeX-minibuffer-complete)
  (define-key YaTeX-section-completion-map
    "\C-v" 'YaTeX-read-section-with-overview))

(defvar YaTeX-recursive-map nil
  "*Key map used at YaTeX reading arguments in the minibuffer.")
(if YaTeX-recursive-map nil
  (setq YaTeX-recursive-map (copy-keymap global-map))
  (define-key YaTeX-recursive-map YaTeX-prefix YaTeX-prefix-map))

;;    (define-key YaTeX-recursive-map
;;      (concat YaTeX-prefix (if YaTeX-inhibit-prefix-letter "\C-s" "s"))
;;      'YaTeX-make-section)
;;    (define-key map
;;      (concat YaTeX-prefix (if YaTeX-inhibit-prefix-letter "\C-m" "m"))
;;      'YaTeX-make-singlecmd)
;;    (define-key map
;;      (concat YaTeX-prefix (if YaTeX-inhibit-prefix-letter "\C-l" "l"))
;;      'YaTeX-make-fontsize)


;---------- Define other variable ----------
(defvar env-name "document" "*Initial tex-environment completion")
(defvar section-name "documentstyle" "*Initial tex-section completion")
(defvar fontsize-name "large" "*Initial fontsize completion")
(defvar single-command "maketitle" "*Initial LaTeX single command")
(defvar YaTeX-user-table-is-read nil
  "Flag that means whether user completion table has been read or not."
)
(defvar YaTeX-kanji-code-alist nil
  "Kanji-code expression translation table."
)
(if (boundp 'MULE)
    (setq YaTeX-kanji-code-alist
	  (list (cons
		 1
		 (if YaTeX-dos (if (boundp '*sjis-dos*) *sjis-dos* *sjis*dos)
		   *sjis*))
		'(2 . *junet*) '(3 . *euc-japan*))
))
(defvar YaTeX-kanji-code (if YaTeX-dos 1 2)
  "*File kanji code used by Japanese TeX."
)
(defvar YaTeX-coding-system nil "File coding system used by Japanese TeX.")
(defvar YaTeX-latex-message-code "Process coding system for LaTeX.")
(cond
 ((boundp 'MULE)
  (setq YaTeX-coding-system
	(symbol-value (cdr (assoc YaTeX-kanji-code YaTeX-kanji-code-alist))))
  (if (not YaTeX-dos)
      (setq YaTeX-latex-message-code *autoconv*)))
 ((boundp 'NEMACS)
  (setq YaTeX-latex-message-code latex-message-kanji-code))
)
(defvar YaTeX-parent-file nil
  "*Main LaTeX source file name used when %#! expression doesn't exist.")
(make-variable-buffer-local 'YaTeX-parent-file)

;---------- Provide YaTeX-mode ----------
;;;
;; Major mode definition
;;;
(defun yatex-mode ()
  "  Yet Another LaTeX mode: Major mode for editing input files of LaTeX.
-You can invoke processes concerning LaTeX typesetting by
 		\\[YaTeX-typeset-menu]
-Complete LaTeX environment form of `\\begin{env} ... \\end{env}' by
		\\[YaTeX-make-begin-end]
-Enclose region into some environment by
		\\[universal-argument] \\[YaTeX-make-begin-end]
-Complete LaTeX command which takes argument like `\\section{}' by
		\\[YaTeX-make-section]
-Put LaTeX command which takes no arguments like `\\maketitle' by
		\\[YaTeX-make-singlecmd]
-Complete font or character size descriptor like `{\\large }' by
		\\[YaTeX-make-fontsize]
-Enclose region into those descriptors above by
		\\[universal-argument] \\[YaTeX-make-fontsize]
-Enter European accent notations by
		\\[YaTeX-make-accent]
-Toggle various modes of YaTeX by
		\\[YaTeX-switch-mode-menu]
-Change environt name (on the begin/end line) by
		\\[YaTeX-change-*]
-Kill LaTeX command/environment sequences by
		\\[YaTeX-kill-*]
-Kill LaTeX command/environment with its contents 
		\\[universal-argument] \\[YaTeX-kill-*]
-Go to corresponding object (begin/end, file, labels) by
		\\[YaTeX-goto-corresponding-*]   or
		\\[YaTeX-goto-corresponding-*-other-window]   (in other window)
		\\[YaTeX-goto-corresponding-*-other-frame]   (in other frame)
-Go to main LaTeX source text by
		\\[YaTeX-visit-main]   or
		\\[YaTeX-visit-main-other-window]   (in other window)
		\\[YaTeX-visit-main-other-frame]   (in other frame)
-Comment out or uncomment region by
		\\[YaTeX-comment-region]  or  \\[YaTeX-uncomment-region]
-Comment out or uncomment paragraph by
		\\[YaTeX-comment-paragraph]  or  \\[YaTeX-uncomment-paragraph]
-Make an \\item entry hang-indented by
		\\[YaTeX-fill-item]
-Enclose the region with parentheses by
		\\[YaTeX-insert-parens-region]
		\\[YaTeX-insert-braces-region]
		\\[YaTeX-insert-brackets-region]
		\\[YaTeX-insert-dollars-region]
-Look up the corresponding column header of tabular environment by
		\\[YaTeX-what-column]
-Enter a newline and an entry suitable for environment by
		\\[YaTeX-intelligent-newline]
-View the structure of file inclusion by
		\\[YaTeX-display-hierarchy]
-Refer the online help of popular LaTeX commands by
		\\[YaTeX-help]   (help)
		\\[YaTeX-apropos]   (apropos)
-Edit `%# notation' by
		\\[YaTeX-%-menu]

  Those are enough for fastening your editing of LaTeX source.  But further
more features are available and they are documented in the manual.
"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'yatex-mode)
  (setq mode-name (if YaTeX-japan "やてふ" "YaTeX"))
  (mapcar 'make-local-variable
	  '(dvi2-command fill-column fill-prefix
	    tmp-env-table tmp-section-table tmp-fontsize-table
	    tmp-singlecmd-table paragraph-start paragraph-separate
	    YaTeX-math-mode indent-line-function
	    comment-start comment-start-skip
	    ))
  (cond ((boundp 'MULE)
	 (set-file-coding-system  YaTeX-coding-system))
  	((boundp 'NEMACS)
	 (make-local-variable 'kanji-fileio-code)
	 (setq kanji-fileio-code YaTeX-kanji-code)))
  (setq fill-column YaTeX-fill-column
	fill-prefix YaTeX-fill-prefix
	paragraph-start    YaTeX-paragraph-start
	paragraph-separate YaTeX-paragraph-separate
	indent-line-function 'YaTeX-indent-line
	comment-start YaTeX-comment-prefix
	comment-end ""
	;;comment-start-skip "[^\\\\]%+[ \t]*"
	)
  (use-local-map YaTeX-mode-map)
  (if YaTeX-dos (setq YaTeX-saved-screen-height (screen-height)))
  (YaTeX-read-user-completion-table)
  (and (fboundp 'YaTeX-19-collect-macros) (YaTeX-19-collect-macros))
  (turn-on-auto-fill)			;1.63
  (run-hooks 'text-mode-hook 'yatex-mode-hook)
)

;---------- Define YaTeX-mode functions ----------
(defvar YaTeX-ec "\\" "Escape character of current mark-up language.")
(defvar YaTeX-ec-regexp (regexp-quote YaTeX-ec))
(defvar YaTeX-struct-begin
  (concat YaTeX-ec "begin{%1}%2")
  "Keyword to begin environment.")
(defvar YaTeX-struct-end (concat YaTeX-ec "end{%1}")
  "Keyword to end environment.")
(defvar YaTeX-struct-name-regexp "[^}]+"
  "Environment name regexp.")
(defvar YaTeX-TeX-token-regexp
  (cond (YaTeX-japan "[A-Za-z*あ-ん亜-龠]+")
	(t "[A-Za-z*]+"))
  "Regexp of characters which can be a member of TeX command's name.")
(defvar YaTeX-command-token-regexp YaTeX-TeX-token-regexp
  "Regexp of characters which can be a member of current mark up language's command name.")

;;(defvar YaTeX-struct-section
;;  (concat YaTeX-ec "%1{%2}")
;;  "Keyword to make section.")

;;;
;; autoload section
;;;
;;autoload from yatexlib(general functions).
(autoload 'YaTeX-showup-buffer "yatexlib" "YaTeX library" t)
(autoload 'YaTeX-window-list "yatexlib" "YaTeX library" t)
(autoload 'YaTeX-search-active-forward "yatexlib" "YaTeX library" t)
(autoload 'YaTeX-search-active-backward "yatexlib" "YaTeX library" t)
(autoload 'substitute-all-key-definition "yatexlib" "YaTeX library" t)
(autoload 'YaTeX-switch-to-buffer "yatexlib" "YaTeX library" t)
(autoload 'YaTeX-switch-to-buffer-other-window "yatexlib" "YaTeX library" t)
(autoload 'YaTeX-replace-format "yatexlib" "YaTeX library" t)
(autoload 'YaTeX-replace-format-args "yatexlib" "YaTeX library" t)
(autoload 'rindex "yatexlib" "YaTeX library" t)
(autoload 'YaTeX-match-string "yatexlib" "YaTeX library" t)
(autoload 'YaTeX-minibuffer-complete "yatexlib" "YaTeX library" t)
(autoload 'goto-buffer-window "yatexlib" "YaTeX library" t)
(autoload 'split-window-calculate-height "yatexlib" "YaTeX library" t)
(autoload 'read-string-with-history "yatexlib" "YaTeX library" t)
(autoload 'read-from-minibuffer-with-history "yatexlib" "YaTeX library" t)
(autoload 'completing-read-with-history "yatexlib" "YaTeX library" t)
(autoload 'YaTeX-switch-to-window "yatexlib" "For windows.el" t)

;;autoload from yatexprc.el
(autoload 'YaTeX-visit-main "yatexprc" "Visit main LaTeX file." t)
(autoload 'YaTeX-visit-main-other-window "yatexprc"
  "Visit main other window." t)
(autoload 'YaTeX-main-file-p "yatexprc" "Check if the file is main." t)
(autoload 'YaTeX-get-builtin "yatexprc" "Get %# built-in." t)
(autoload 'YaTeX-system "yatexprc" "Call system command" t)
(autoload 'YaTeX-save-buffers "yatexprc" "Save buffers of same major mode" t)

;;autoload from yatexmth.el
(autoload 'YaTeX-math-insert-sequence "yatexmth" "Image input." t)
(autoload 'YaTeX-in-math-mode-p "yatexmth" "Check if in math-env." t)
(autoload 'YaTeX-toggle-math-mode "yatexmth" "YaTeX math-mode interfaces." t)
(autoload 'YaTeX-math-member-p "yatexmth" "Check if a word is math command." t)

;;autoload from yatexhlp.el
(autoload 'YaTeX-help "yatexhlp" "YaTeX helper with LaTeX commands." t)
(autoload 'YaTeX-apropos "yatexhlp" "Apropos for (La)TeX commands." t)

;;autoload from yatexgen.el
(autoload 'YaTeX-generate "yatexgen" "YaTeX add-in function generator." t)
(autoload 'YaTeX-generate-simple "yatexgen" "YaTeX add-in support." t)

;;autoload from yatexsec.el
(autoload 'YaTeX-read-section-in-minibuffer "yatexsec" "YaTeX sectioning" t)
(autoload 'YaTeX-make-section-with-overview "yatexsec" "YaTeX sectioning" t)

;;autoload from yatexenv.el
(autoload 'YaTeX-what-column "yatexenv" "YaTeX env. specific funcs" t)
(autoload 'YaTeX-intelligent-newline "yatexenv" "YaTeX env. specific funcs" t)
(autoload 'YaTeX-indent-line-equation "yatexenv" "Indent equation lines." t)
(autoload 'YaTeX-goto-corresponding-leftright "yatexenv" "\left\right jumps" t)

;;autoload from yatexhie.el
(autoload 'YaTeX-display-hierarchy "yatexhie"
  "YaTeX document hierarchy browser" t)

;; autoload from yahtml.el
(autoload 'yahtml-inner-environment-but "yahtml" "yahtml internal func." t)

;;;
;; YaTeX-mode functions
;;;
(defun YaTeX-insert-begin-end (env region-mode)
  "Insert \\begin{mode-name} and \\end{mode-name}.
This works also for other defined begin/end tokens to define the structure."
  (setq YaTeX-current-completion-type 'begin)
  (let*((ccol (current-column)) beg beg2 exchange
	(arg region-mode)		;for old compatibility
	(indent-column (+ ccol YaTeX-environment-indent))(i 1) func)
    (if (and region-mode (> (point) (mark)))
	(progn (exchange-point-and-mark)
	       (setq exchange t
		     ccol (current-column)
		     indent-column (+ ccol YaTeX-environment-indent))))
    ;;VER2 (insert "\\begin{" env "}" (YaTeX-addin env))
    (setq beg (point))
    (YaTeX-insert-struc 'begin env)
    (setq beg2 (point))
    (insert "\n")
    (indent-to indent-column)
    (save-excursion
      ;;indent optional argument of \begin{env}, if any
      (while (> (point-beginning-of-line) beg)
	(skip-chars-forward "\\s " (point-end-of-line))
	(indent-to indent-column)
	(forward-line -1)))
    (require 'yatexenv)
    (if region-mode
	  ;;if region-mode, indent all text in the region
	(save-excursion
	  (if (fboundp (intern-soft (concat "YaTeX-enclose-" env)))
	      (funcall (intern-soft (concat "YaTeX-enclose-" env))
		       (point) (mark))
	    (while (< (progn (forward-line 1) (point)) (mark))
	      (if (eolp) nil
		(skip-chars-forward " \t\n")
		(indent-to indent-column))))))
    (if region-mode (exchange-point-and-mark))
    (indent-to ccol)
    ;;VER2 (insert "\\end{" env "}\n")
    (YaTeX-insert-struc 'end env)
    (YaTeX-reindent ccol)
    (if region-mode
	(progn
	  (insert "\n")
	  (or exchange (exchange-point-and-mark)))
      (goto-char beg2)
      (YaTeX-intelligent-newline nil))
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
    (YaTeX-update-table
     (list env-name) 'env-table 'user-env-table 'tmp-env-table)
    (YaTeX-insert-begin-end env-name arg))
)

(defun YaTeX-make-begin-end-region ()
  "Call YaTeX-make-begin-end with ARG to specify region mode."
  (interactive)
  (YaTeX-make-begin-end t)
)

(defun YaTeX-inner-environment (&optional quick)
  "Return current inner-most environment.
Non-nil for optional argument QUICK restricts search bound to most
recent sectioning command.  Matching point is stored to property 'point
of 'YaTeX-inner-environment, which can be referred by
 (get 'YaTeX-inner-environment 'point)."
  (let*((nest 0)
	(beg (YaTeX-replace-format-args
	      (regexp-quote YaTeX-struct-begin)
	      YaTeX-struct-name-regexp
	      (if (eq major-mode 'yahtml-mode) "\\s *.*" "")
	      ""))
	(end (YaTeX-replace-format-args
	      (regexp-quote YaTeX-struct-end)
	      YaTeX-struct-name-regexp "" ""))
	(begend (concat "\\(" beg "\\)\\|\\(" end "\\)"))
	bound m0
	(open
	 (concat "^" (or (cdr (assq major-mode '((yahtml-mode . "<")))) "{")))
	(close
	 (concat "^"
		 (or (cdr(assq major-mode '((yahtml-mode . "\n\t >")))) "}"))))
    (save-excursion
      (if quick
	  (setq bound
		(save-excursion
		  (YaTeX-re-search-active-backward
		   (concat YaTeX-ec-regexp
			   "\\(" YaTeX-sectioning-regexp "\\)\\*?\\{")
		   YaTeX-comment-prefix nil 1)
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
	   (progn (skip-chars-forward close) (point))))))
)

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
		   (count-lines (point-min) (point)))))))
)

;;;VER2
(defun YaTeX-insert-struc (what env)
  (cond
   ((eq what 'begin)
    (insert (YaTeX-replace-format-args
	     YaTeX-struct-begin env (YaTeX-addin env))))
   ((eq what 'end)
    (insert (YaTeX-replace-format-args YaTeX-struct-end env)))
   (t nil))
)

(defun YaTeX-make-section (arg &optional beg end cmd)
  "Make LaTeX \\section{} type command with completing read.
With numeric ARG, you can specify the number of arguments of
LaTeX command.
  For example, if you want to produce LaTeX command

	\\addtolength{\\topmargin}{8mm}

which has two arguments.  You can produce that sequence by typing...
	ESC 2 C-c s add SPC RET \\topm SPC RET 8mm RET
\(by default\)
Then yatex will automatically complete `addtolength' with two arguments
next time.
  You can complete symbol at LaTeX command and the 1st argument.

If the optional 2nd and 3rd argument BEG END are specified, enclose
the region from BEG to END into the first argument of the LaTeX sequence.
Optional 4th arg CMD is LaTeX command name, for non-interactive use."
  (interactive "P")
  (setq YaTeX-current-completion-type 'section)
  (unwind-protect
      (let*
	  ((source-window (selected-window))
	   (section
	    (or cmd
		(YaTeX-read-section
		 (if YaTeX-simple-messages
		     (format "Section-type (default %s): " section-name)
		   (if (> (minibuffer-depth) 0)
		       (format "%s???{} (default %s)%s: " YaTeX-ec section-name
			       (format "[level:%d]" (minibuffer-depth)))
		     (format "(C-v for view-section) %s???{} (default %s): "
			     YaTeX-ec section-name)))
		 nil)))
	   (section (if (string= section "") section-name section))
	   (numarg	;; The number of section-type command's argument
	    (or arg
		(nth 1 (YaTeX-lookup-table section 'section))
		1))
	   (arg-reader (intern-soft (concat "YaTeX::" section)))
	   (addin-args (and arg-reader (fboundp arg-reader)))
	   (title "")
	   (j 1)
	   (enable-recursive-minibuffers t));;let
	(setq section-name section)
	(if beg
	    (let ((e (make-marker)))
	      (goto-char end)
	      (insert "}")
	      (set-marker e (point))
	      (goto-char beg)
	      (insert YaTeX-ec section-name "{")
	      (goto-char (marker-position e)))
	  (use-global-map YaTeX-recursive-map)
	  (if (= numarg 0) (YaTeX-make-singlecmd section-name)
	    (insert YaTeX-ec section-name (YaTeX-addin section-name)))
	  (while (<= j numarg)
	    (insert
	     "{"
	     (setq title
		   (cond
		    (addin-args (funcall arg-reader j))
		    (YaTeX-skip-default-reader "")
		    (t
		     (read-string (format "Argument %d of %s: " j section)))))
	     "}")
	    (setq j (1+ j))))
	(YaTeX-update-table
	 (if (/= numarg 1) (list section numarg)
	   (list section))
	 'section-table 'user-section-table 'tmp-section-table)
	(if YaTeX-current-position-register
	    (point-to-register YaTeX-current-position-register))
	(if (string= (buffer-substring (- (point) 2) (point)) "{}")
	  (forward-char -1))
	(while (string= (buffer-substring (- (point) 3) (1- (point))) "{}")
	  (forward-char -2)))
    (if (<= (minibuffer-depth) 0) (use-global-map global-map)))
)

(defun YaTeX-make-section-region (args beg end)
  "Call YaTeX-make-section with arguments to specify region mode."
 (interactive "P\nr")
 (YaTeX-make-section args beg end)
)

(defun YaTeX-make-fontsize (arg &optional fontsize)
  "Make completion like {\\large ...} or {\\slant ...} in minibuffer.
If you invoke this command with universal argument, you can put region
into {\\xxx } braces.
\(key binding for universal-argument is \\[universal-argument]\)"
  (interactive "P")
  (YaTeX-sync-local-table 'tmp-fontsize-table)
  (let* ((mode (if arg "region" ""))
	 (fontsize
	  (or fontsize
	      (YaTeX-read-fontsize
	       (if YaTeX-simple-messages
		   (format "Font or size (default %s): " fontsize-name)
		 (format "{\\??? %s} (default %s)%s: " mode fontsize-name
			 (if (> (minibuffer-depth) 0)
			     (format "[level:%d]" (minibuffer-depth)) "")))
	       nil nil))))
    (if (string= fontsize "")
	(setq fontsize fontsize-name))
    (setq fontsize-name fontsize)
    (YaTeX-update-table
     (list fontsize-name)
     'fontsize-table 'user-fontsize-table 'tmp-fontsize-table)
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

(defvar YaTeX-singlecmd-suffix "" "*Suffix for maketitle-type commands.")
(defvar YaTeX-read-singlecmd-history nil "Holds maketitle-type history.")
(put 'YaTeX-read-singlecmd-history 'no-default t)
(defun YaTeX-make-singlecmd (single)
  (interactive
   (list (YaTeX-cplread-with-learning
	  (if YaTeX-simple-messages
	      (format "maketitle-type (default %s): " single-command)
	    (format "%s??? (default %s)%s: " YaTeX-ec single-command
		    (if (> (minibuffer-depth) 0)
			(format "[level:%d]" (minibuffer-depth)) "")))
	  'singlecmd-table 'user-singlecmd-table 'tmp-singlecmd-table
	  nil nil nil 'YaTeX-read-singlecmd-history)))
  (if (string= single "")
      (setq single single-command))
  (setq single-command single)
  (setq YaTeX-current-completion-type 'maketitle)
  (let ((dollar (and (not (YaTeX-in-math-mode-p))
		     (YaTeX-math-member-p single-command)))
	p q)
    (if dollar (insert "$"))
    (insert YaTeX-ec single-command)
    (setq p (point))
    (insert (YaTeX-addin single) YaTeX-singlecmd-suffix)
    (if dollar (insert "$"))
    (setq q (point))
    (goto-char p)
    (forward-char -2)
    (if (looking-at "\\[\\]") (forward-char 1) (goto-char q)))
  (if YaTeX-current-position-register
      (point-to-register YaTeX-current-position-register))
)

(defvar YaTeX-completion-begin-regexp "[{\\]"
  "Regular expression of limit where LaTeX command's completion begins.")

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
	   (limit (point-beginning-of-line))
	   (completion-begin 
	    (progn (re-search-backward "[ \t\n]" limit 1) (point)))
	   (begin (progn
		    (goto-char end)
		    (if (re-search-backward YaTeX-completion-begin-regexp
					    completion-begin t)
			(1+ (point))
		      nil))))
      (goto-char end)
      (cond
       ((null begin)
	(message "I think it is not a LaTeX sequence."))
       (t
	(mapcar 'YaTeX-sync-local-table
		'(tmp-section-table tmp-env-table tmp-singlecmd-table))
	(let*((pattern (buffer-substring begin end))
	      (all-table
	       (append
		section-table user-section-table tmp-section-table
		env-table     user-env-table     tmp-env-table
		singlecmd-table user-singlecmd-table tmp-singlecmd-table))
	      ;; First,
	      ;; search completion without backslash.
	      (completion (try-completion pattern all-table)))
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
	       (all-completions pattern all-table))))))))))
)

(defun YaTeX-toggle-modify-mode (&optional arg)
  (interactive "P")
  (or (memq 'YaTeX-modify-mode mode-line-format)
      (setq mode-line-format
	    (append (list "" 'YaTeX-modify-mode) mode-line-format)))
  (if (or arg (null YaTeX-modify-mode))
      (progn
	(setq YaTeX-modify-mode "*m*")
	(message "Modify mode"))
    (setq YaTeX-modify-mode nil)
    (message "Cancel modify mode."))
  (set-buffer-modified-p (buffer-modified-p))	;redraw mode-line
)

(defun YaTeX-switch-mode-menu (arg &optional char)
  (interactive "P")
  (message "Toggle: (M)odify-mode ma(T)h-mode")
  (let ((c (or char (read-char))))
    (cond
     ((= c ?m) (YaTeX-toggle-modify-mode arg))
     ((or (= c ?$) (= c ?t))
      (if YaTeX-auto-math-mode
	  (message "Makes no sense in YaTeX-auto-math-mode.")
	(YaTeX-toggle-math-mode arg)))))
)

(defun YaTeX-insert-quote ()
  (interactive)
  (insert
   (cond
    ((YaTeX-literal-p) ?\")
    ((= (preceding-char) ?\\ ) ?\")
    ;((= (preceding-char) ?\( ) ?\")
    ((or (= (preceding-char) 32)
	 (= (preceding-char) 9)
	 (= (preceding-char) ?\n)
	 (bobp)
	 (string-match
	  (regexp-quote (char-to-string (preceding-char)))
	  "、。，．？！「」『』【】()"))
     "``")
    (t  "''")))
)

(defun YaTeX-closable-p ()
  (and (not YaTeX-modify-mode)
       (not (eq YaTeX-close-paren-always 'never))
       (or YaTeX-close-paren-always (eolp))
       (not (input-pending-p))
       (not (YaTeX-literal-p)))
  ;;(or YaTeX-modify-mode
  ;;    (and (not YaTeX-close-paren-always) (not (eolp)))
  ;;    (input-pending-p)
  ;;    (YaTeX-quick-in-environment-p "verbatim"))
)

(defun YaTeX-insert-braces-region (beg end &optional open close)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (insert (or close "}"))
    (goto-char beg)
    (insert (or open "{")))
)

(defun YaTeX-insert-braces (arg &optional open close)
  (interactive "p")
  (let (env)
    (cond
     ((YaTeX-jmode) (YaTeX-self-insert arg))
     ((not (YaTeX-closable-p)) (YaTeX-self-insert arg))
     ((save-excursion
	(and (> (- (point) (point-min)) 6)
	     (condition-case () (forward-char -6) (error nil)))
	(looking-at "\\\\left\\\\"))
      (insert "{\\right\\}")
      (forward-char -8))
     ((and (> (point) (+ (point-min) 4))
	   (save-excursion (backward-char 4) (looking-at "\\\\end"))
	   (not (YaTeX-literal-p))
	   (setq env (YaTeX-inner-environment)))
      (momentary-string-display
       (concat
	"{"
	(cond
	 (YaTeX-japan
	  (format "今度からはちゃんと %s b を使いましょう" YaTeX-prefix))
	 (t (format "You don't understand Zen of `%s b':p" YaTeX-prefix)))
	"}")
       (point))
      (insert (or open "{") env (or close "}")))
     (t
      (insert (or open "{") (or close "}"))
      (forward-char -1)
      (if (and (eq (char-after (point)) ?\})
	       (eq (char-after (- (point) 2)) ?\\ ))
	  (progn (insert "\\") (forward-char -1)))
      )))
)

(defun YaTeX-jmode ()
  (or (and (boundp 'canna:*japanese-mode*) canna:*japanese-mode*)
      (and (boundp 'egg:*mode-on*) egg:*mode-on* egg:*input-mode*))
)

(defun YaTeX-jmode-off ()
  (cond
   ((and (boundp 'canna:*japanese-mode*) canna:*japanese-mode*)
    (canna-toggle-japanese-mode))
   ((and (boundp 'egg:*mode-on*) egg:*mode-on* egg:*input-mode*)
    (egg:toggle-egg-mode-on-off))
   ((and (fboundp 'skk-mode) (boundp 'skk-mode) skk-mode)
    (if (fboundp 'skk-mode-off) (skk-mode-off) (j-mode-off)))
   ((and (fboundp 'fep-force-off) (fep-force-off))))
)

(defun YaTeX-self-insert (arg)
  (call-interactively (global-key-binding (char-to-string last-command-char))))

(defun YaTeX-insert-brackets (arg)
  "Insert Kagi-kakko or \\ [ \\] pair or simply \[."
  (interactive "p")
  (let ((col (1- (current-column))))
    (cond
     ((YaTeX-jmode) (YaTeX-self-insert arg))
     ((not (YaTeX-closable-p))
      (YaTeX-self-insert arg))
     ((save-excursion
	(and (> (- (point) (point-min)) 5) (forward-char -5))
	(looking-at "\\\\left"))
      (insert "[\\right]")
      (forward-char -7))
     ((and (= (preceding-char) ?\\ )
	   (/= (char-after (- (point) 2)) ?\\ )
	   (not (YaTeX-in-math-mode-p)))
      (insert last-command-char "\n")
      (indent-to (max 0 col))
      (insert "\\]")
      (beginning-of-line)
      (open-line 1)
      (delete-region (point) (progn (beginning-of-line) (point)))
      (indent-to (+ YaTeX-environment-indent (max 0 col)))
      (or YaTeX-auto-math-mode YaTeX-math-mode (YaTeX-toggle-math-mode 1)))
     ((YaTeX-closable-p)
      (insert "[]")
      (backward-char 1))
     (t (YaTeX-self-insert arg)))
    )
)

(defun YaTeX-insert-brackets-region (beg end)
  (interactive "r")
  (YaTeX-insert-braces-region beg end "[" "]")
)

(defun YaTeX-insert-parens (arg)
  "Insert parenthesis pair."
  (interactive "p")
  (cond
   ((YaTeX-jmode) (YaTeX-self-insert arg))
   ((not (YaTeX-closable-p)) (YaTeX-self-insert arg))
   ((save-excursion
      (and (> (- (point) (point-min)) 5) (forward-char -5))
      (looking-at "\\\\left"))
    (insert "(\\right)")
    (forward-char -7))
   ((and (= (preceding-char) ?\\ ) (not (YaTeX-in-math-mode-p)))
    (insert "(\\)")
    (backward-char 2))
   ((YaTeX-closable-p)
    (insert "()")
    (backward-char 1))
   (t (YaTeX-self-insert arg)))
)

(defun YaTeX-insert-parens-region (beg end)
  (interactive "r")
  (YaTeX-insert-braces-region beg end "(" ")")
)

(defun YaTeX-insert-dollar ()
  (interactive)
  (if (or (not (YaTeX-closable-p))
	  (= (preceding-char) 92)
	  (and (YaTeX-in-math-mode-p)
	       (or (/= (preceding-char) ?$) (/= (following-char) ?$))))
      (insert "$")
    (insert "$$")
    (forward-char -1)
    (YaTeX-jmode-off)
    (or YaTeX-auto-math-mode YaTeX-math-mode (YaTeX-toggle-math-mode 1)))
)

(defun YaTeX-insert-dollars-region (beg end)
  (interactive "r")
  (YaTeX-insert-braces-region beg end "$" "$")
)

(defun YaTeX-insert-amper ()
  (interactive)
  (if (or (string-match YaTeX-array-env-regexp
			(or (YaTeX-inner-environment t) "document"))
	  (= (preceding-char) 92)
	  (YaTeX-literal-p))
      (insert "&")
    (insert "\\&"))
)

(defun YaTeX-version ()
  "Return string of the version of running YaTeX."
  (interactive)
  (message
   (concat "Yet Another tex-mode "
	   (if YaTeX-japan "「野鳥」" "`Wild Bird'")
	   " Revision "
	   YaTeX-revision-number))
)

(defun YaTeX-typeset-menu (arg &optional char)
  "Typeset, preview, visit error and miscellaneous convenient menu.
Optional second argument CHAR is for non-interactive call from menu."
  (interactive "P")
  (message
   (concat "J)latex R)egion B)ibtex mk(I)ndex "
	   (if (not YaTeX-dos) "K)ill-latex ")
	   "P)review "
	   (and (boundp 'window-system) window-system "S)earch ")
	   "V)iewerr L)pr"))
  (let ((sw (selected-window)) (c (or char (read-char))))
    (require 'yatexprc)			;for Nemacs's bug
    (select-window sw)
    (cond
     ((= c ?j) (YaTeX-typeset-buffer))
     ((= c ?r) (YaTeX-typeset-region))
     ((= c ?b) (YaTeX-call-command-on-file
		bibtex-command "*YaTeX-bibtex*"))
     ((= c ?i) (YaTeX-call-command-on-file
		makeindex-command "*YaTeX-makeindex*"))
     ((= c ?k) (YaTeX-kill-typeset-process YaTeX-typeset-process))
     ((= c ?p) (call-interactively 'YaTeX-preview))
     ((= c ?q) (YaTeX-system "lpq" "*Printer queue*"))
     ((= c ?v) (YaTeX-view-error))
     ((= c ?l) (YaTeX-lpr arg))
     ((= c ?m) (YaTeX-switch-mode-menu arg))
     ((= c ?b) (YaTeX-insert-string "\\"))
     ((= c ?s) (YaTeX-xdvi-remote-search arg))))
)

(defun YaTeX-%-menu (&optional beg end char)
  "Operate %# notation."
  ;;Do not use interactive"r" for the functions which require no mark
  (interactive)
  (message "!)Edit-%%#! B)EGIN-END-region L)Edit-%%#LPR")
  (let ((c (or char (read-char))) (string "") key
	(b (make-marker)) (e (make-marker)))
    (save-excursion
      (cond
       ((or (= c ?!) (= c ?l))		;Edit `%#!'
	(goto-char (point-min))
	(setq key (cond ((= c ?!) "%#!")
			((= c ?l) "%#LPR")))
	(if (re-search-forward key nil t)
	    (progn
	      (setq string (buffer-substring (point) (point-end-of-line)))
	      (delete-region (point) (progn (end-of-line) (point))))
	  (open-line 1)
	  (delete-region (point) (progn (beginning-of-line)(point)));for 19 :-<
	  (insert key))
	(unwind-protect
	    (setq string (read-string (concat key ": ") string))
	  (insert string)))

       ((= c ?b)			;%#BEGIN %#END region
	(or end (setq beg (min (point) (mark)) end (max (point) (mark))))
	(set-marker b beg)
	(set-marker e end)
	(goto-char (point-min))
	(while (re-search-forward "^%#\\(BEGIN\\)\\|\\(END\\)$" nil t)
	  (beginning-of-line)
	  (delete-region (point) (progn (forward-line 1) (point))))
	(goto-char (marker-position b))
	(open-line 1)
	(delete-region (point) (progn (beginning-of-line)(point)));for 19 :-<
	(insert "%#BEGIN")
	(goto-char (marker-position e))
	(insert "%#END\n"))
       )))
)

(defun YaTeX-goto-corresponding-label (reverse &optional otherwin)
  "Jump to corresponding \\label{} and \\ref{} or \\cite and \\bibitem.
  The default search direction depends on the command at the cursor position.
When the cursor is on \\ref(\\cite), YaTeX will try to search the
corresponding \\label(\\bibitem) backward,
and if it fails search forward again.  And when the cursor is
on \\label(\\bibitem), YaTeX will search the corresponding \\ref(\\cite)
forward at first and secondary backward.
  Argument REVERSE non-nil makes the default
direction rule reverse.  Since Search string is automatically set in
search-last-string, you can repeat search the same label/ref by typing
\\[isearch-forward] or \\[isearch-backward].
  If optional second argument OTHERWIN is non-nil, move to other window."

  (let ((scmd "") label direc string blist (p (point)) (cb (current-buffer))
	(refcommands "label\\|ref\\|cite\\|bibitem")
	(func (function (lambda (string sfunc)
			  (or
			   (funcall sfunc string nil t)
			   (funcall (if (eq sfunc 'search-forward)
					'search-backward 'search-forward)
				    string nil t))))))
    (cond
     ((YaTeX-on-section-command-p refcommands)
      (setq scmd (cdr (assoc (YaTeX-match-string 1)
			     '(("label" . "ref") ("ref" . "label")
			       ("cite" . "bibitem") ("bibitem" . "cite")))))
      (goto-char (match-end 0))
      (let ((label (buffer-substring 
		    (1- (point)) (progn (backward-list 1) (1+ (point))))))
	(setq string (concat "\\" scmd "{" label "}"))
	(setq direc (if (string-match "ref\\|cite" scmd)
			'search-forward 'search-backward))
	(if YaTeX-current-position-register
	    (point-to-register YaTeX-current-position-register))
	(if reverse (setq direc (if (eq direc 'search-forward)
				    'search-backward 'search-forward)))
	(if (funcall func string direc)	;label/ref found!
	    (progn
	      (if otherwin
		  (progn
		    (goto-char p)
		    (if (one-window-p)
			(split-window-calculate-height
			 YaTeX-default-pop-window-height))
		    (select-window (get-lru-window))
		    (switch-to-buffer cb)))
	      (goto-char (match-beginning 0))
	      (push-mark p))
	  ;;if label/ref not found, search through all yatex buffers.
	  (goto-char p)			;resume position of current buffer
	  (setq blist (YaTeX-yatex-buffer-list))
	  (catch 'found
	    (while blist
	      (set-buffer (car blist))
	      (if (YaTeX-on-section-command-p refcommands)
		  (goto-char (match-beginning 0)))
	      (if (funcall func string direc)
		  (progn
		    (cond
		     (otherwin
		      (set-buffer cb)
		      (goto-char p)
		      (if (one-window-p)
			  (split-window-calculate-height
			   YaTeX-default-pop-window-height))
		      (select-window (get-lru-window))
		      (switch-to-buffer (car blist)))
		     ((or (get-buffer-window (car blist))
			  (and YaTeX-emacs-19
			       (get-buffer-window (car blist) t)))
		      (goto-buffer-window (car blist)))
		     (t
		      (switch-to-buffer (car blist))
		      (message
		       (format "Type %s %c to return to original position."
			       (key-description
				(car
				 (or (where-is-internal 'register-to-point)
				     (where-is-internal 'jump-to-register))))
			       YaTeX-current-position-register))))
		    (goto-char (match-beginning 0))
		    (throw 'found t)))
	      (setq blist (cdr blist)))))
	)
      (if YaTeX-emacs-19
	  (setq search-ring (cons string search-ring))
	(setq search-last-string string)))
     (t nil)))
)

(defun YaTeX-goto-corresponding-environment (&optional allow-mismatch noerr)
  "Go to corresponding begin/end enclosure.
Optional argument ALLOW-MISMATCH allows mismatch open/clese.  Use this
for \left(, \right).
Optional third argument NOERR causes no error for unballanced environment."
  (interactive)
  (if (not (YaTeX-on-begin-end-p)) nil
    (let ((p  (match-end 0)) b0 b1 env (nest 0) regexp re-s (op (point))
	  (m0 (match-beginning 0))	;whole matching
	  (m1 (match-beginning 1))	;environment in \begin{}
	  (m2 (match-beginning 2)))	;environment in \end{}
      ;(setq env (regexp-quote (buffer-substring p (match-beginning 0))))
      (if (cond
	   (m1				;if begin{xxx}
	    (setq env
		  (if allow-mismatch YaTeX-struct-name-regexp
		    (regexp-quote (buffer-substring m1 (match-end 1)))))
	;    (setq regexp (concat "\\(\\\\end{" env "}\\)\\|"
	;			 "\\(\\\\begin{" env "}\\)"))
	    (setq regexp
		  (concat
		   "\\("
		   (YaTeX-replace-format-args
		    (regexp-quote YaTeX-struct-end) env "" "")
		   "\\)\\|\\("
		   (YaTeX-replace-format-args
		    (regexp-quote YaTeX-struct-begin) env "" "")
		   "\\)"))
	    (setq re-s 're-search-forward))
	   (m2				;if end{xxx}
	    (setq env
		  (if allow-mismatch YaTeX-struct-name-regexp
		    (regexp-quote (buffer-substring m2 (match-end 2)))))
	;   (setq regexp (concat "\\(\\\\begin{" env "}\\)\\|"
	;			 "\\(\\\\end{" env "}\\)"))
	    (setq regexp
		  (concat
		   "\\("
		   (YaTeX-replace-format-args
		    (regexp-quote YaTeX-struct-begin) env "" "")
		   "\\)\\|\\("
		   (YaTeX-replace-format-args
		    (regexp-quote YaTeX-struct-end) env "" "")
		   "\\)"))
	    (setq re-s 're-search-backward))
	   (t (if noerr nil (error "Corresponding environment not found."))))
	  (progn
	    (while (and (>= nest 0) (funcall re-s regexp nil t))
	      (setq b0 (match-beginning 0) b1 (match-beginning 1))
	      (if (or (equal b0 m0)
		      (YaTeX-literal-p b0))
		  nil
		(setq nest (if (equal b0 b1)
			       (1- nest) (1+ nest)))))
	    (if (< nest 0)
		(goto-char (match-beginning 0)) ;found.
	      (goto-char op)
	      (funcall
	       (if noerr 'message 'error)
	       "Corresponding environment `%s' not found." env)
	      (sit-for 1)
	      nil)))))
)

(defun YaTeX-goto-corresponding-file (&optional other)
  "Visit or switch buffer of corresponding file,
looking at \\input or \\include or \includeonly on current line."
  (if (not (YaTeX-on-includes-p)) nil
    (let ((parent buffer-file-name) input-file)
      (save-excursion
	(if (search-forward "{" (point-end-of-line) t)
	    nil
	  (skip-chars-backward "^,{"))
	(setq input-file
	      (buffer-substring
	       (point) (progn (skip-chars-forward "^ ,}") (point))))
	(if (not (string-match "\\.\\(tex\\|sty\\)$" input-file))
	    (setq input-file (concat input-file ".tex"))))
      (cond
       (other (YaTeX-switch-to-buffer-other-window input-file))
       ((get-file-buffer input-file) (goto-buffer-window input-file))
       (t (YaTeX-switch-to-buffer input-file)))
      (or (YaTeX-get-builtin "!")
	  YaTeX-parent-file
	  (setq YaTeX-parent-file parent))))
)

(defun YaTeX-goto-corresponding-BEGIN-END ()
  (if (not (YaTeX-on-BEGIN-END-p)) nil
    (if (cond
	 ((equal (match-beginning 0) (match-beginning 1)) ;if on %#BEGIN
	  (not (search-forward "%#END" nil t)))
	 (t ; if on %#END
	  (not (search-backward "%#BEGIN" nil t))))
	(error "Corresponding %%#BEGIN/END not found."))
    (beginning-of-line)
    t)
)

(defvar YaTeX-processed-file-regexp-alist nil
  "Alist of regexp of processed file regexp vs. its file name part;
For example, if you include image file with `\\epsfile{file=FILE}' where
`FILE' is processed file.  You might want to view FILE with other previewer
such as ghostview, or want to preview its source which was drawn with
other drawing tool, tgif for example.  Then you should set entire regexp
of including expression and enclose its file name part with \\\\( and \\\\).

 Ex. (\"\\\\\\\\epsfile{[^}]*file=\\\\([^,} ]+\\\\)\\\\(\\\\.e?ps\\\\)?[^}]*}\" 1)

Where the first group surrounded by \\\\( and \\\\) is the file name part
of expression.  So you should set 1 to second element.  And the first
matching group is sent to (image) processor defined by the variable
YaTeX-file-processor-alist. See also the documentation of
YaTeX-file-processor-alist.

↑じゃ良くわかんないすね。例えば tgif hoge.obj して hoge.eps を
\\epsfile{file=hoge.eps} でインクルードしているとしよう。その行で
\[prefix\] g を押した時に tgif を起動して欲しかったら、まず上のような
正規表現を設定する。\\\\(と\\\\)で囲んだところがファイル名になるように
注意する。でファイル名部分が何番目の\\\\(\\\\)になるかをリストの2番目に書く。
すると、その部分が変数 YaTeX-file-processor-alist で定義された
処理プログラムに渡される。というわけ。
ん～やっぱりむずかしいね。分からない時は隣の Lisper に聞くか、
fj野鳥の会で聞こう!
")

(defvar YaTeX-processed-file-regexp-alist-default
  '(("\\\\epsfile{[^},]*file=\\(\\([^,} ]*/\\)?[^,}. ]+\\)\\(\\.e?ps\\)?[^}]*}" 1)
    ("\\\\epsfig{[^},]*fi\\(le\\|gure\\)=\\(\\([^,} ]*/\\)?[^,}. ]+\\)\\(\\.e?ps\\)?[^}]*}" 2)
    ("\\\\postscriptbox{[^}]*}{[^}]*}{\\(\\([^,} ]*/\\)?[^}. ]+\\)\\(\\.e?ps\\)?}" 1)
    ("\\\\\\(epsfbox\\|includegraphics\\){\\(\\([^,} ]*/\\)?[^} ]+\\)\\(\\.e?ps\\)?}" 2)
    ("\\\\\\(psbox\\)\\(\\[[^]]+\\]\\)?{\\(\\([^,} ]*/\\)?[^} ]+\\)\\(\\.e?ps\\)}" 3) ;\psbox[options...]{hoge.eps} (97/1/11)
    )
  "See the documentation of YaTeX-processed-file-regexp-alist."
)

(defvar YaTeX-file-processor-alist nil
  "*Alist of files' processor vs. its extension;
See also the documentation of YaTeX-processed-file-regexp-alist."
)  
(defvar YaTeX-file-processor-alist-default
  '(("tgif" . ".obj")
    ("ghostview" . ".ps")
    ("ghostview" . ".eps")
    (t . ".tex")
    (t . ".sty")
    (t . ""))
  "See the documentation of YaTeX-file-processor-alist."
)

(defun YaTeX-goto-corresponding-file-processor (&optional other)
  "Execute corresponding file processor."
  (save-excursion
    (or (looking-at YaTeX-ec-regexp)
	(skip-chars-backward (concat "^" YaTeX-ec) (point-beginning-of-line)))
    (let ((list (append YaTeX-processed-file-regexp-alist
			YaTeX-processed-file-regexp-alist-default))
	  (p (point)) flist file
	  (peol (point-end-of-line)))
      (setq flist (catch 'found
		   (while list
		     (goto-char p)
		     (if (re-search-forward (car (car list)) peol t)
			 (progn
			   (setq file (YaTeX-match-string
				       (car (cdr (car list)))))
			   (throw 'found (cdr (car list)))))
		     (setq list (cdr list)))))
      (if flist				;if pattern and file name found
	  (let*((plist (append YaTeX-file-processor-alist
			       YaTeX-file-processor-alist-default))
		(plist0 plist)
		ext cmd src buf (alt (car (cdr flist))))
	    (if (and (re-search-forward
		      (concat YaTeX-comment-prefix "\\s *\\(.*\\)$") peol t)
		     (assoc (setq cmd (YaTeX-match-string 1))
			    YaTeX-file-processor-alist))
		(setq src		;if processor is specified
		      (concat file
			      (cdr (assoc cmd YaTeX-file-processor-alist))))
	      (while plist		;if processor is not specified
		(setq ext (cdr (car plist)))
		(if (and (string< "" (concat file ext))
			 (file-exists-p (concat file ext)))
		      (setq cmd (car (car plist))
			    src (concat file ext) plist nil))
		(setq plist (cdr plist)))
	      (if (and (null src) alt YaTeX-create-file-prefix-g)
		  (setq cmd alt
			src (concat file (cdr (assoc alt plist0))))))
	    (if src		;if processor and src file found
		(cond
		 ((stringp cmd)
		  (let ((buf (concat "* " cmd " " src " *")))
		    (YaTeX-system (concat cmd " " src) buf)
		    t))
		 ((eq t cmd)
		  (let ((parent buffer-file-name))
		    (funcall
		     (cond
		      (other 'YaTeX-switch-to-buffer-other-window)
		      ((get-file-buffer src) 'goto-buffer-window)
		      (t 'YaTeX-switch-to-buffer))
		     src)
		    (or (YaTeX-get-builtin "!")
			YaTeX-parent-file
			(setq YaTeX-parent-file parent))
		    t))
		 ((symbolp cmd)
		  (cond
		   ((symbol-function cmd)
		    (funcall cmd src other)))
		  t)))))))
)

(defun YaTeX-on-section-command-p (command)
  "Check if point is on the LaTeX command: COMMAND(regexp).
Return nil if point is not on it.  Otherwise return the
number of argument position.
Section command name is stored in match-data #1."
  (let ((p (point)) md (parg 0) (argc 1) word (grouping 0) (i 0))
    (while (setq i (string-match "\\\\(" command i))
      (setq grouping (1+ grouping) i (+ i 2)))
    (save-excursion
      (if (looking-at YaTeX-ec-regexp) nil
	(catch 'found			;caught value has no meaning
	  (while t
	    (if (bobp) (throw 'found nil))
	    (cond
	     ((looking-at YaTeX-ec-regexp) (throw 'found t))
	     ((looking-at "[[{]") nil)
	     ((looking-at "[]}]")(condition-case nil (up-list -1) (error nil)))
	     (t (skip-chars-backward " \t\r\n")))
	    (skip-chars-backward (concat "^ \t\r\n{}[]" YaTeX-ec-regexp))
	    (or (bobp) (forward-char -1)))))
      (if (and
	   (looking-at (concat YaTeX-ec-regexp "\\(" command "\\)"
			       "\\(\\(\\[[^]]+\\]\\)*\\)"	;optional arg
			       ;"[ \t\n\r]*{[^}]+}")) ;arg braces
			       "[ \t\n\r]*{[^}]*}")) ;arg braces
	   (not (YaTeX-lookup-table
		 (setq word (YaTeX-match-string 1)) 'singlecmd)))
	  (progn
	    (setq md (match-data))
	    (skip-chars-forward "^{")
	    (if (<= (point) p) (setq parg (1+ parg)))
	    (setq argc
		  (or (car (cdr (YaTeX-lookup-table word 'section)))
		      argc))
	    (while (and (>= (setq argc (1- argc)) 0)
			(progn (skip-chars-forward " \t\n\r")
			       (looking-at "{")))
	      (forward-list 1)
	      (if (<= (point) p) (setq parg (1+ parg))))
	    (store-match-data md)
	    (setq i (+ 2 grouping))
	    (if (and (match-beginning i)
		     (>= p (match-beginning i)) (< p (match-end i)))
		-1			;return -1 if point is on optional arg
	      (if (< p (point)) parg))
	    ))))
)

(defun YaTeX-on-maketitle-p ()
  "Check if point is on maketitle type commands.
Call this function after YaTeX-on-section-command-p."
  (let ((p (point)))
    (save-excursion
      (or (= (char-after (point)) ?\\ )
	  (progn
	    (skip-chars-backward
	     (concat "^" YaTeX-ec-regexp) (point-beginning-of-line))
	    (or (bobp) (bolp) (backward-char 1))))
      (and (looking-at (concat YaTeX-ec-regexp YaTeX-TeX-token-regexp))
	   (<= (match-beginning 0) p)
	   (> (match-end 0) p)))))

(defun YaTeX-on-begin-end-p ()
  (save-excursion
    (beginning-of-line)
    (re-search-forward
     ;;"\\\\begin{\\([^}]+\\)}\\|\\\\end{\\([^}]+\\)}"
     (concat
      (YaTeX-replace-format-args
       (regexp-quote YaTeX-struct-begin)
       (concat "\\(" YaTeX-struct-name-regexp "\\)") "" "" "")
      "\\|"
      (YaTeX-replace-format-args
       (regexp-quote YaTeX-struct-end)
       (concat "\\(" YaTeX-struct-name-regexp "\\)") "" "" ""))
     (point-end-of-line) t))
)

(defun YaTeX-on-includes-p ()
  (save-excursion
    (beginning-of-line)
    (re-search-forward "\\(\\(include.*\\)\\|\\(input\\)\\){.*}"
		       (point-end-of-line) t))
)

(defun YaTeX-on-comment-p (&optional sw)
  "Return t if current line is commented out.
Optional argument SW t to treat all `%' lines as comment,
even if on `%#' notation."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "\\s ")
    (looking-at (if sw "%" "%[^#]")))
)

(defun YaTeX-on-BEGIN-END-p ()
  (save-excursion
    (let ((case-fold-search nil))
      (beginning-of-line)
      (re-search-forward "\\(%#BEGIN\\)\\|\\(%#END\\)" (point-end-of-line) t)))
)

(defun YaTeX-goto-corresponding-* (arg)
  "Parse current line and call suitable function."
  (interactive "P")
  (cond
   ((YaTeX-goto-corresponding-label arg))
   ((YaTeX-goto-corresponding-environment))
   ((YaTeX-goto-corresponding-file arg))
   ((YaTeX-goto-corresponding-file-processor arg))
   ((YaTeX-goto-corresponding-BEGIN-END))
   ((and (string-match
	  YaTeX-equation-env-regexp	;to delay loading
	  (or (YaTeX-inner-environment t) "document"))
	 (YaTeX-goto-corresponding-leftright)))
   (t (message "I don't know where to go.")))
)

(defun YaTeX-goto-corresponding-*-other-window (arg)
  "Parse current line and call suitable function."
  (interactive "P")
  (cond
   ((YaTeX-goto-corresponding-label arg t))
   ;;((YaTeX-goto-corresponding-environment))
   ((YaTeX-goto-corresponding-file t))
   ;;((YaTeX-goto-corresponding-BEGIN-END))
   (t (message "I don't know where to go.")))
)

(defun YaTeX-comment-region (alt-prefix)
  "Comment out region by '%'.
If you call this function on the 'begin{}' or 'end{}' line,
it comments out whole environment"
  (interactive "P")
  (if (not (YaTeX-on-begin-end-p))
      (comment-out-region
       (if alt-prefix
	   (read-string "Insert prefix: ")
	 YaTeX-comment-prefix))
    (YaTeX-comment-uncomment-env 'comment-out-region))
)

(defun YaTeX-uncomment-region (alt-prefix)
  "Uncomment out region by '%'."
  (interactive "P")
  (if (not (YaTeX-on-begin-end-p))
      (uncomment-region
       (if alt-prefix (read-string "Remove prefix: ")
	 YaTeX-comment-prefix)
       (region-beginning) (region-end) YaTeX-uncomment-once)
    (YaTeX-comment-uncomment-env 'uncomment-region))
)

(defun YaTeX-comment-uncomment-env (func)
  "Comment or uncomment out one LaTeX environment switching function by FUNC."
  (let (beg (p (point)))
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (YaTeX-goto-corresponding-environment)
      (beginning-of-line)
      (if (> p (point)) (setq beg (1+ beg)) (forward-char 1))
      (funcall func YaTeX-comment-prefix beg (point) YaTeX-uncomment-once)))
  (message "%sommented out current environment."
	   (if (eq func 'comment-region) "C" "Un-c"))
)

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
	  t)))
)

(defun YaTeX-end-of-environment (&optional limit-search-bound)
  "Goto the end of the current environment.
Optional argument LIMIT-SEARCH-BOUND non-nil limits the search bound
to most recent sectioning command."
  (interactive)
  (YaTeX-beginning-of-environment limit-search-bound t)
)

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
      (if (eobp) nil (forward-char 1))))
)

(defun YaTeX-comment-paragraph ()
  "Comment out current paragraph."
  (interactive)
  (save-excursion
    (cond
     ((YaTeX-on-begin-end-p)
      (beginning-of-line)
      (insert YaTeX-comment-prefix)
      (YaTeX-goto-corresponding-environment)
      (beginning-of-line)
      (insert YaTeX-comment-prefix))
     ((YaTeX-on-comment-p)
      (message "Already commented out."))
     (t
      (mark-paragraph)
      (if (looking-at paragraph-separate) (forward-line 1))
      (comment-out-region "%"))))
)

(defun YaTeX-uncomment-paragraph ()
  "Uncomment current paragraph."
  (interactive)
  (save-excursion
    (if (YaTeX-on-begin-end-p)
	(let ((p (make-marker)))
	  (set-marker p (point))
	  (YaTeX-goto-corresponding-environment)
	  (YaTeX-remove-prefix YaTeX-comment-prefix YaTeX-uncomment-once)
	  (goto-char (marker-position p))
	  (YaTeX-remove-prefix YaTeX-comment-prefix YaTeX-uncomment-once))
      (if (YaTeX-on-comment-p)
	  (let*((fill-prefix "")
		;;append `^%' to head of paragraph delimiter.
		(paragraph-start
		 (concat
		  "^$\\|^%\\(" YaTeX-paragraph-delimiter "\\)"))
		(paragraph-separate paragraph-start))
	    (mark-paragraph)
	    (if (not (bobp)) (forward-line 1))
	    (uncomment-region "%" nil nil YaTeX-uncomment-once))
	(message "This line is not a comment line."))))
)

(defun YaTeX-remove-prefix (prefix &optional once)
  "Remove prefix on current line as far as prefix detected. But
optional argument ONCE makes deletion once."
  (interactive "sPrefix:")
  (beginning-of-line)
  (while (re-search-forward (concat "^" prefix) (point-end-of-line) t)
    (replace-match "")
    (if once (end-of-line)))
)

(defun YaTeX-kill-option-string ()
  (if (and (eq predicate 'YaTeX-on-begin-end-p)
	   (looking-at "\\(\\[.*\\]\\)*\\({.*}\\)*"))
      (delete-region (match-beginning 0) (match-end 0)))  
)

(defun YaTeX-kill-some-pairs (predicate gofunc kill-contents)
  "Kill some matching pair.
This function assumes that pairs occupy each line where they resid."
  ;;(interactive)
  (if (not (funcall predicate)) nil
    (let ((beg (make-marker)) (end (make-marker)) (p (make-marker)))
      (set-marker end (match-end 0))
      (if (match-beginning 2)
	  (set-marker beg (match-beginning 2))
	(set-marker beg (match-beginning 1))
	(goto-char (match-end 0))
	(YaTeX-kill-option-string))
      (save-excursion
	(funcall gofunc)
	(delete-region (point-beginning-of-line) (match-end 0))
	(YaTeX-kill-option-string)
	(if (and (eolp) (not (eobp))) (delete-char 1))
	(set-marker p (point))
	(goto-char beg)
	(delete-region (point-beginning-of-line) end)
	(if (and (eolp) (not (eobp))) (delete-char 1))
	(if kill-contents (delete-region p (point))))
      t))
)

(defun YaTeX-kill-section-command (point kill-all)
  "Kill section-type command at POINT leaving its argument.
Non-nil for the second argument kill its argument too."
  (let (beg (end (make-marker)))
    (save-excursion
      (goto-char point)
      (or (looking-at YaTeX-ec-regexp)
	  (progn
	    (skip-chars-backward (concat "^" YaTeX-ec-regexp))
	    (forward-char -1)))
      (setq beg (point))
      (skip-chars-forward "^{")
      (forward-list 1)
      (set-marker end (point))
      (if kill-all (delete-region beg end)
	(goto-char beg)
	(delete-region
	 (point) (progn (skip-chars-forward "^{" end) (1+ (point))))
	(goto-char end)
	(backward-delete-char 1))))
)

(defun YaTeX-kill-paren (kill-contents)
  "Kill parentheses leaving its contents.
But kill its contents if the argument KILL-CONTENTS is non-nil."
  (save-excursion
    (let (p)
      (if (looking-at "\\s(\\|\\(\\s)\\)")
	  (progn
	    (if (match-beginning 1)
		(up-list -1))
	    (setq p (point))
	    (forward-list 1)
	    (if kill-contents (delete-region p (point))
	      (backward-delete-char 1)
	      (goto-char p)
	      (if (looking-at
		   (concat "{" YaTeX-ec-regexp
			   YaTeX-command-token-regexp "+"
			   "\\s +"))
		  (delete-region
		   (point)
		   (progn (re-search-forward "\\s +" nil t) (point)))
		(delete-char 1)))
	    t))))
)

(defvar YaTeX-read-environment-history nil "Holds history of environments.")
(put 'YaTeX-read-environment-history 'no-default t)
(defun YaTeX-read-environment (prompt &optional predicate must-match initial)
  "Read a LaTeX environment name with completion."
  (YaTeX-sync-local-table 'tmp-env-table)
  (completing-read-with-history
   prompt
   (append tmp-env-table user-env-table env-table)
   predicate must-match initial
   'YaTeX-read-environment-history)
)

(defvar YaTeX-read-section-history nil "Holds history of section-types.")
(put 'YaTeX-read-section-history 'no-default t)
(defun YaTeX-read-section (prompt &optional predicate initial)
  "Read a LaTeX section-type command with completion."
  (YaTeX-sync-local-table 'tmp-section-table)
  (let ((minibuffer-completion-table
	 (append tmp-section-table user-section-table section-table)))
    (read-from-minibuffer-with-history
     prompt initial YaTeX-section-completion-map nil
     'YaTeX-read-section-history))
)

(defun YaTeX-read-section-with-overview ()
  "Read sectioning command with overview.
This function refers a local variable `source-window' in YaTeX-make-section"
  (interactive)
  (require 'yatexsec)			;some case needs this
  (if (> (minibuffer-depth) 1)
      (error "Too many minibuffer levels for overview."))
  (let ((sw (selected-window))(enable-recursive-minibuffers t) sect)
    (unwind-protect
	(progn
	  (select-window source-window)
	  (setq sect (YaTeX-read-section-in-minibuffer
		      "Sectioning(Up=C-p, Down=C-n, Help=?): "
		      YaTeX-sectioning-level (YaTeX-section-overview))))
      (select-window sw))
    (if (eq (selected-window) (minibuffer-window))
	(erase-buffer))
    (insert sect)
    (exit-minibuffer)
    )
)

(defvar YaTeX-read-fontsize-history nil "Holds history of font designator.")
(put 'YaTeX-read-fontsize-history 'no-default t)
(defun YaTeX-read-fontsize (prompt &optional predicate must-match initial)
  "Read a LaTeX font changing command with completion."
  (YaTeX-sync-local-table 'tmp-fontsize-table)
  (completing-read-with-history
   prompt (append tmp-fontsize-table user-fontsize-table fontsize-table)
   predicate must-match initial 'YaTeX-read-fontsize-history)
)

(defun YaTeX-change-environment ()
  "Change the name of environment."
  (interactive)
  (if (not (YaTeX-on-begin-end-p)) nil
    (save-excursion
      (let (p env (m1 (match-beginning 1)) (m2 (match-beginning 2)))
	(setq env (if m1 (buffer-substring m1 (match-end 1))
		    (buffer-substring m2 (match-end 2))))
	(goto-char (match-beginning 0))
	(set-mark-command nil)
	(YaTeX-goto-corresponding-environment)
	(setq newenv (YaTeX-read-environment
		      (format "Change environment `%s' to: " env)))
	(cond
	 ((string= newenv "")	(message "Change environment cancelled."))
	 ((string= newenv env)	(message "No need to change."))
	 (t
	  (search-forward (concat "{" env) (point-end-of-line) t)
	  (replace-match (concat "{" newenv))
	  (exchange-point-and-mark)
	  (search-forward (concat "{" env) (point-end-of-line) t)
	  (replace-match (concat "{" newenv))))
	t)))
)

(defun YaTeX-change-section ()
  "Change section-type command."
  (interactive)
  (let*((where (YaTeX-on-section-command-p YaTeX-command-token-regexp))
	(p (point)) (cmd (YaTeX-match-string 1)) beg end old new)
    (if (null where) nil
      (cond

       ((equal where 0);;if point is on section command
	(setq beg (match-beginning 1)
	      end (match-end 1))
	(goto-char beg)			;beginning of the command
	(setq new (YaTeX-read-section (format "Change `%s' to: " cmd) nil)))

       ((= where -1);;if point is on a optional parameter
	(setq beg (match-beginning 2))
	(skip-chars-forward "^{")
	(setq end (point))
	(goto-char p)
	(setq new
	      (if (fboundp (intern-soft (concat YaTeX-addin-prefix cmd)))
		  (YaTeX-addin cmd)
		(concat "["
			(read-string (format "Change `%s' to: "
					     (buffer-substring
					      (1+ beg) (1- end))))
			"]"))))

       ((> where 0);;if point is in arguments' braces
	(or (looking-at "{")
	    (progn (skip-chars-backward "^{") (forward-char -1)))
	(setq beg (1+ (point)))
	(forward-list 1)
	(forward-char -1)
	(setq end (point)
	      old (buffer-substring beg end))
	(goto-char p)
	(if (> (length old) 40)
	    (setq old (concat (substring old 0 12) "..." (substring old -12))))
	(setq new
	      (if (intern-soft (concat "YaTeX::" cmd))
		  (funcall (intern-soft (concat "YaTeX::" cmd)) where)
		(read-string (format "Change `%s' to: " old)))))
       );cond
      (delete-region beg end)
      (goto-char beg)
      (insert-before-markers new)
      ;;(goto-char (marker-position p))
      new))
)

(defun YaTeX-change-fontsize ()
  "Change large-type command."
  (let ((lt (append tmp-fontsize-table user-fontsize-table fontsize-table))
	(p (point)) large old new beg end)
    ;;(and (looking-at "}") (up-list -1))
    ;;(and (looking-at "{") (forward-char 1))
    ;;Is above convenient?
    (save-excursion
      (or (looking-at YaTeX-ec-regexp)
	  (progn
	    (skip-chars-backward (concat "^" YaTeX-ec-regexp))
	    (forward-char -1)))
      (cond
       ((and
	 (looking-at
	  (concat YaTeX-ec-regexp "\\(" YaTeX-TeX-token-regexp "\\)"))
	 (< p (match-end 0))
	 (assoc (setq old (YaTeX-match-string 1)) lt))
	(goto-char p)
	(setq beg (match-beginning 1) end (match-end 1) ;save match position
	      new (completing-read
		   (format "Change font/size `%s' to : " old) lt))
	(delete-region beg end)
	(goto-char beg)
	(insert-before-markers new)
	new)
       (t nil)
       )))
)

(defun YaTeX-change-math-image ()
  "Change with image completion."
  (let (maketitle memberp beg end)
    (if (and (YaTeX-on-maketitle-p)
	     (progn
	       (setq maketitle (substring (YaTeX-match-string 0) 1))
	       (setq memberp (YaTeX-math-member-p maketitle))))
	(let ((last-command-char (string-to-char (car memberp))))
	  (setq beg (match-beginning 0) end (match-end 0))
	  (delete-region beg end)
	  (YaTeX-math-insert-sequence t (cdr memberp))))))

(defun YaTeX-kill-* (&optional arg)
  "Parse current line and call suitable function.
Non-nil for ARG kills its contents too."
  (interactive "P")
  (cond
   ((YaTeX-kill-some-pairs 'YaTeX-on-begin-end-p
			   'YaTeX-goto-corresponding-environment arg))
   ((YaTeX-kill-some-pairs 'YaTeX-on-BEGIN-END-p
			   'YaTeX-goto-corresponding-BEGIN-END arg))
   ((YaTeX-on-section-command-p YaTeX-command-token-regexp);on any command
    (YaTeX-kill-section-command (match-beginning 0) arg))
   ((YaTeX-kill-paren arg))
   (t (message "I don't know what to kill.")))
)

(defun YaTeX-change-* ()
  "Parse current line and call suitable function."
  (interactive)
  (cond
   ((YaTeX-change-environment))
   ((YaTeX-change-section))
   ((YaTeX-change-fontsize))
   ((YaTeX-change-math-image))
   (t (message "I don't know what to change.")))
)

;;;
;Check availability of add-in functions
;;;
(cond
 ((featurep 'yatexadd) nil)		;Already provided.
 ((progn (load "yatexadd" t) (featurep 'yatexadd)) nil)
 (t (message "YaTeX add-in functions not supplied.")))

(defun YaTeX-addin (name)
  "Check availability of addin function and call it if exists."
  (if (and (not (get 'YaTeX-generate 'disabled))
	   (intern-soft (concat YaTeX-addin-prefix name))
	   (fboundp (intern-soft (concat YaTeX-addin-prefix name))))
      (let ((s (funcall (intern (concat YaTeX-addin-prefix name)))))
	(if (stringp s) s ""))
    "") ;Add in function is not bound.
)

(defun YaTeX-on-item-p (&optional point)
  "Return t if POINT (default is (point)) is on \\item."
  (let ((p (or point (point))))
    (save-excursion
      (goto-char p)
      (end-of-line)
      (setq p (point))
      (re-search-backward YaTeX-paragraph-delimiter nil t)
      (re-search-forward YaTeX-item-regexp p t)))
)

(defun YaTeX-in-verb-p (&optional point)
  "Check if POINT is in verb or verb*.  Default of POINT is (point)."
  (setq point (or point (point)))
  (save-excursion
    (goto-char point)
    (if (not (re-search-backward
	      (concat YaTeX-ec-regexp
		      "\\(" YaTeX-verb-regexp "\\)"
		      "\\([^-A-Za-z_*]\\)")
	      (point-beginning-of-line) t))
	nil
      (goto-char (match-end 2))
      (skip-chars-forward
       (concat "^" (buffer-substring (match-beginning 2) (match-end 2))))
      (and (< (match-beginning 2) point) (< (1- point) (point)))))
)

(defun YaTeX-literal-p (&optional point)
  "Check if POINT is in verb or verb* or verbatime environment family.
Default of POINT is (point)."
  (cond
   ((equal YaTeX-ec "\\")		;maybe LaTeX
    (save-excursion
      (and point (goto-char point))
      (or (YaTeX-in-verb-p (point))
	  (and (not (looking-at "\\\\end{verb"))
	       (YaTeX-quick-in-environment-p YaTeX-verbatim-environments))))))
)

(defun YaTeX-in-environment-p (env)
  "Return if current LaTeX environment is ENV.
ENV is given in the form of environment's name or its list."
  (let ((md (match-data)) (nest 0) p envrx)
    (cond
     ((atom env)
      (setq envrx
	    (concat "\\("
		    (regexp-quote
		     (YaTeX-replace-format-args
		      YaTeX-struct-begin env "" ""))
		    "\\)\\|\\("
		    (regexp-quote
		     (YaTeX-replace-format-args
		      YaTeX-struct-end env "" ""))
		    "\\)"))
      (save-excursion
	(setq p (catch 'open
		  (while (YaTeX-re-search-active-backward
			  envrx YaTeX-comment-prefix nil t)
		    (if (match-beginning 2)
			(setq nest (1+ nest))
		      (setq nest (1- nest)))
		    (if (< nest 0) (throw 'open t)))))))
     ((listp env)
      (setq p
	    (or (YaTeX-in-environment-p (car env))
		(and (cdr env) (YaTeX-in-environment-p (cdr env)))))))
    (store-match-data md)
    p;(or p (YaTeX-in-verb-p (match-beginning 0)))
    )
)

(defun YaTeX-quick-in-environment-p (env)
  "Check quickly but unsure if current environment is ENV.
ENV is given in the form of environment's name or its list.
This function returns correct result only if ENV is NOT nested."
  (save-excursion
    (let ((md (match-data)) (p (point)) rc clfound)
      (cond
       ((listp env)
	(or (YaTeX-quick-in-environment-p (car env))
	    (and (cdr env) (YaTeX-quick-in-environment-p (cdr env)))))
       (t
	(if (YaTeX-search-active-backward
	     (YaTeX-replace-format-args YaTeX-struct-begin env "" "")
	     YaTeX-comment-prefix nil t)
	    (setq rc (not (YaTeX-search-active-forward
			   (YaTeX-replace-format-args
			    YaTeX-struct-end env)
			   YaTeX-comment-prefix p t nil))))
	(store-match-data md)
	rc))))
)

;; Filling \item
(defun YaTeX-remove-trailing-comment (start end)
  "Remove trailing comment from START to end."
  (save-excursion
    (let ((trcom (concat YaTeX-comment-prefix "$")))
      (goto-char start)
      (while (re-search-forward trcom end t)
	(if (/= (char-after (1- (match-beginning 0))) ?\\ )
	    (replace-match "\\1")))))
)

(defun YaTeX-get-item-info (&optional recent thisenv)
  "Return the list of the beginning of \\item and column of its item.
If it seems to be outside of itemizing environment, just return nil.
Non-nil for optional argument RECENT refers recent \\item.
Optional second argument THISENV omits calling YaTeX-inner-environment."
  (save-excursion
    (let* ((p (point)) env e0 c
	   (bndry (and (setq env (or thisenv (YaTeX-inner-environment t)))
		       (get 'YaTeX-inner-environment 'point)
		       )))
      (end-of-line)
      (if (if recent
	      (YaTeX-re-search-active-backward
	       YaTeX-item-regexp YaTeX-comment-prefix bndry t)
	    (goto-char bndry)
	    (YaTeX-re-search-active-forward
	     YaTeX-item-regexp YaTeX-comment-prefix p t))
	  (progn
	    (goto-char (match-end 0))
	    ;(setq c (current-column))
	    (if (string-match "desc" env)
		(setq c 6)
	      (if (equal (following-char) ?\[) (forward-list 1))
	      (setq c 0))
	    (skip-chars-forward " \t" (point-end-of-line))
	    (list (point-beginning-of-line) (+ c (current-column)))))))
)

(defun YaTeX-fill-item ()
  "Fill item in itemize environment."
  (interactive)
  (save-excursion
    (let* ((p (point))
	   (item-term (concat
		       "\\(^[ \t]*$\\)\\|" YaTeX-item-regexp "\\|\\("
		       YaTeX-ec-regexp "\\(begin\\|end\\)\\)"))
	   ;;This value depends on LaTeX.
	   fill-prefix start col
	   (info (YaTeX-get-item-info t)))
      (if (null info) nil		;not on \item, do nothing
	(setq start (car info)
	      col (car (cdr info)))
	(save-excursion
	  (if (re-search-backward "^\\s *$" start t)
	      ;;if separated from \item line, isolate this block
	      (progn
		(setq start (1+ (match-end 0)))
		(goto-char start)
		(skip-chars-forward " \t")
		(delete-region (point) start) ;is this your favor???
		(indent-to col))))
	(beginning-of-line)
	(if (<= (save-excursion
		 (re-search-forward
		  (concat "\\\\end{\\|\\\\begin{\\|^[ \t]*$") nil t)
		 (match-beginning 0))
	       p)
	    (progn  (message "Not on itemize.") nil)
	  (end-of-line)
	  (newline)
	  (indent-to col)
	  (setq fill-prefix
		(buffer-substring (point-beginning-of-line)(point)))
	  (beginning-of-line)
	  (delete-region (point) (progn (forward-line 1) (point)))
	  (re-search-forward item-term nil 1)
	  (YaTeX-remove-trailing-comment start (point))
	  (beginning-of-line)
	  (push-mark (point) t)
	  (goto-char start)
	  (forward-line 1)
	  (while (< (point) (mark))
	    (delete-region (point) (progn (skip-chars-forward " \t") (point)))
	    (forward-line 1))
	  (fill-region-as-paragraph start (mark))
	  (if NTT-jTeX
	      (while (progn(forward-line -1)(end-of-line) (> (point) start))
		(insert ?%)))
	  (pop-mark)))))
)

(defun YaTeX-fill-paragraph (arg)
  "YaTeX adjustment function for fill-paragraph.
*Protect \\verb from unexpected broken up."
  (interactive "P")
  (cond
   ((not (eq major-mode 'yatex-mode)) (fill-paragraph arg))
   ((YaTeX-quick-in-environment-p YaTeX-fill-inhibit-environments) nil)
   (t
    (save-excursion
      (let ((verbrex (concat YaTeX-ec-regexp
			     "\\(" YaTeX-verb-regexp "\\)" ;match#1
			     "\\(.\\).*\\(\\2\\)")) ;match #2 and #3
	    (p (point)) ii end poslist spacelist (fill-prefix fill-prefix))
	(cond
	 ((save-excursion (beginning-of-line) ;if point is on the first
			  (setq end (point))  ;non-whitespace char
			  (skip-chars-forward " \t")
			  (equal (point) p))
	  (setq fill-prefix (buffer-substring p end)))
	 ((and ;;(not YaTeX-emacs-19)
	       (string-match YaTeX-itemizing-env-regexp
			     (or (YaTeX-inner-environment t) "document"))
	       (setq ii (YaTeX-get-item-info)))
	  (save-excursion
	    (beginning-of-line)
	    (indent-to-column (car (cdr ii)))
	    (setq fill-prefix
		  (buffer-substring (point) (point-beginning-of-line)))
	    (delete-region (point) (progn (beginning-of-line) (point))))))
        (mark-paragraph)
	(save-restriction
	  (narrow-to-region (region-beginning) (region-end))
	  (YaTeX-remove-trailing-comment (point-min) (point-max))
	  (goto-char (point-min))
	  (while (YaTeX-re-search-active-forward
		  verbrex YaTeX-comment-prefix (point-max) t)
	    (setq end (match-beginning 3))
	    (goto-char (match-beginning 2))
	    (while (re-search-forward "\\s " end t)
	      (setq poslist (cons (make-marker) poslist)
		    spacelist (cons (preceding-char) spacelist))
	      (replace-match "_")
	      (set-marker (car poslist) (match-beginning 0))))
	  ;;(fill-paragraph arg)
	  (fill-region-as-paragraph (point-min) (point-max) arg)
	  (while spacelist
	    (goto-char (marker-position (car poslist)))
	    (delete-char 1)
	    (insert (car spacelist))
	    (setq spacelist (cdr spacelist) poslist (cdr poslist)))
	  (goto-char (point-min))
	  (forward-word 1)
	  (beginning-of-line)
	  (while (re-search-forward "\\\\\\(\\(page\\)?ref\\|cite\\){" nil t)
	    (if (< (point-end-of-line)
		   (save-excursion (forward-char -1) (forward-list 1) (point)))
		(progn (end-of-line) (insert YaTeX-comment-prefix))))
	  (goto-char (point-min))
	  (if (and NTT-jTeX (looking-at "[ \t]"))
	      (progn
		(goto-char (point-min))
		(while (not (eobp))
		  (end-of-line)
		  (or (bolp)
		      (save-excursion
			(backward-word 1)
			(looking-at "\\sw+")) ;is not japanese string
		      (progn (setq p (point)) (insert YaTeX-comment-prefix)))
		  (forward-line 1))
		(goto-char p)
		(delete-char 1)		;remove last inserted `%'
		)))))))
)

(if (fboundp 'YaTeX-saved-indent-new-comment-line) nil
  (fset 'YaTeX-saved-indent-new-comment-line
	(symbol-function 'indent-new-comment-line))
  (fset 'indent-new-comment-line 'YaTeX-indent-new-comment-line))

(defun YaTeX-indent-new-comment-line (&optional soft)
  "Tuned `indent-new-comment-line' function for yatex.
See the documentation of `YaTeX-saved-indent-new-comment-line'."
  (cond
   ((or (not (memq major-mode '(yatex-mode yahtml-mode)))
	(string-match
	 "document"
	 (or (and (boundp 'inenv) inenv)
	     (or (YaTeX-inner-environment t) "document"))))
    (apply 'YaTeX-saved-indent-new-comment-line (if soft (list soft))))
;   ((and (eq major-mode 'yahtml-mode)
;	 (string-match
;	  "^[Pp][Rr][Ee]" (yahtml-inner-environment-but "^[Aa]\\b" t)))
;    (yahtml-indent-new-commnet-line))
   ((YaTeX-in-math-mode-p) nil)		;1996/12/30
   (t (let (fill-prefix)
	(apply 'YaTeX-saved-indent-new-comment-line (if soft (list soft))))))
)

(defun YaTeX-fill-* ()
  "Fill paragraph according to its condition."
  (interactive)
  (cond
   ((YaTeX-fill-item))
   )
)

;; Accent completion
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

;; Indentation
(defun YaTeX-current-indentation ()
  "Return the indentation of current environment."
  (save-excursion
    ;;(beginning-of-line)
    (if (YaTeX-beginning-of-environment t)
	(goto-char (get 'YaTeX-inner-environment 'point))
      (forward-line -1)
      (beginning-of-line)
      (skip-chars-forward " \t"))
    (current-column))
)

(defun YaTeX-previous-line-indentation ()
  (save-excursion
    (forward-line -1)
    (skip-chars-forward " \t")
    (current-column)))

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

(defun YaTeX-indent-line ()
  "Indent corrent line referrin current environment."
  (interactive)
  (let ((indent-relative
	 (function
	  (lambda (&optional additional)
	    (YaTeX-reindent
	     (+ (YaTeX-current-indentation)
		(or additional 0)
		YaTeX-environment-indent)))))
	depth iteminfo (p (point)) pp (peol (point-end-of-line))
	;;inenv below is sometimes defined in YaTeX-indent-new-comment-line
	(inenv (or (and (boundp 'inenv) inenv) (YaTeX-inner-environment t))))
    ;;(if NTT-jTeX		;;Do you need this section?
    ;;	(save-excursion
    ;;  (end-of-line)
    ;;  (let ((p (point)))
    ;;    (forward-line -1)
    ;;    (end-of-line)
    ;;    (or (= p (point))
    ;;	(progn (backward-char (length YaTeX-comment-prefix))
    ;;	       (not (looking-at (regexp-quote YaTeX-comment-prefix))))
    ;;	(progn
    ;;	  (skip-chars-backward YaTeX-comment-prefix)
    ;;	  (kill-line))))))
    (or inenv (setq inenv "document"))	;is the default environment
    (cond
     ((and (YaTeX-on-begin-end-p) (match-beginning 2)) ;if \end
      (save-excursion
	(beginning-of-line)
	(YaTeX-reindent (YaTeX-current-indentation))))
     ((string-match YaTeX-equation-env-regexp inenv)
      (YaTeX-indent-line-equation))	;autoload-ed from yatex.env
     (;(YaTeX-in-environment-p '("itemize" "enumerate" "description" "list"))
      (string-match YaTeX-itemizing-env-regexp inenv)
      ;;(YaTeX-on-item-p) ??
      ;;(setq iteminfo (YaTeX-get-item-info t))
      (if (save-excursion
	    (beginning-of-line)
	    (re-search-forward YaTeX-item-regexp peol t))
	  (save-excursion
	    (goto-char (1+ (match-beginning 0)))
	    (setq depth
		  (* YaTeX-environment-indent
		     (cond
		      ((looking-at "subsubsub")	3)
		      ((looking-at "subsub")	2)
		      ((looking-at "sub")	1)
		      (t			0))))
	    (funcall indent-relative depth))
	(YaTeX-reindent (or (car (cdr (YaTeX-get-item-info nil inenv)))
			    (+ (save-excursion
				 (beginning-of-line)
				 (YaTeX-current-indentation))
			       YaTeX-environment-indent))))
      )
     ((YaTeX-literal-p)			;verbatims
      (tab-to-tab-stop))
     ((and inenv (not (equal "document" inenv)))
      (funcall indent-relative))
     ((YaTeX-on-section-command-p YaTeX-sectioning-regexp)
      (require 'yatexsec)		;to know YaTeX-sectioning-level
      (YaTeX-reindent
       (* (max
	   (1-				;I want chapter to have indentation 0
	    (or (cdr (assoc (YaTeX-match-string 1) YaTeX-sectioning-level))
		0))
	   0)
	  YaTeX-environment-indent)))
     ;;Default movement
     ((and (bolp) fill-prefix) (insert fill-prefix))
     (t (save-excursion
	  (beginning-of-line)
	  (skip-chars-forward " \t")
	  (indent-relative-maybe))
	(skip-chars-forward " \t")))
    ;;if current line is \begin, re-indent \end too
    (if (and (YaTeX-on-begin-end-p) (match-beginning 1))
	(save-excursion
	  ;;(beginning-of-line)
	  ;;(search-forward "\\begin")
	  (goto-char (match-beginning 0))
	  (setq depth (current-column))
	  (YaTeX-goto-corresponding-environment)
	  (YaTeX-reindent depth)))
    (if (or
	 (and NTT-jTeX
	      (save-excursion (beginning-of-line) (looking-at "[ \t]")))
	 (save-excursion
	   (beginning-of-line)
	   (backward-char 1)
	   (and
	    (re-search-backward
	     "\\\\\\(\\(page\\)?ref\\|cite\\){" (point-beginning-of-line) t)
	    (goto-char (1- (match-end 0)))
	    (> (save-excursion
		 (condition-case ()
		     (progn (forward-list 1) (point))
		   (error (point-max))))
	       (point-end-of-line)))))
	(save-excursion
	  (end-of-line)
	  (let ((p (point)))
	    (forward-line -1)
	    (end-of-line)
	    (or (= p (point))
		(looking-at (regexp-quote YaTeX-comment-prefix))
		(bobp) (bolp)
		(save-excursion
		  (backward-word 1)
		  (looking-at "\\sw+")) ;is not japanese string
		(insert YaTeX-comment-prefix))))))
)

(defun YaTeX-local-table-symbol (symbol)
  "Return the lisp symbol which keeps local completion table of SYMBOL."
  (intern (concat "YaTeX$"
		  default-directory
		  (symbol-name symbol)))
)

(defun YaTeX-sync-local-table (symbol)
  "Synchronize local variable SYMBOL.
Copy its corresponding directory dependent completion table to SYMBOL."
  (if (boundp (YaTeX-local-table-symbol symbol))
      (set symbol (symbol-value (YaTeX-local-table-symbol symbol))))
)

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
    (set-buffer curbuf))
)

(defun YaTeX-reload-dictionary ()
  "Reload local dictionary.
Use this function after editing ./.yatexrc."
  (interactive)
  (let ((YaTeX-user-table-is-read nil))
    (YaTeX-read-user-completion-table t))
)

(defun YaTeX-lookup-table (word type)
  "Lookup WORD in completion table whose type is TYPE.
This function refers the symbol tmp-TYPE-table, user-TYPE-table, TYPE-table.
Typically, TYPE is one of 'env, 'section, 'fontsize, 'singlecmd."
  (if (symbolp type) (setq type (symbol-name type)))
  (or (assoc word (symbol-value (intern (concat "tmp-" type "-table"))))
      (assoc word (symbol-value (intern (concat "user-" type "-table"))))
      (assoc word (symbol-value (intern (concat type "-table"))))))

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
	 "`%s' is not in table. Register into: U)serDic L)ocalDic N)one D)iscard"
	 (car vallist))
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
	       (cons vallist (symbol-value default-table))))))))
)

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
    word)
)

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
      (message "Updating %s dictionary...Done" (or type "local"))))
)

;; --------------- General sub functions ---------------
(defun point-beginning-of-line ()
  (save-excursion (beginning-of-line)(point))
)

(defun point-end-of-line ()
  (save-excursion (end-of-line)(point))
)


(provide 'yatex)
(defvar yatex-mode-load-hook nil
  "*List of functions to be called when yatex.el is loaded.")
(if (and YaTeX-emacs-19 window-system (not (featurep 'yatex19)))
    (load "yatex19"))
(load "yatexhks" t)

;;-------------------- Final hook jobs --------------------
(substitute-all-key-definition
 'fill-paragraph 'YaTeX-fill-paragraph YaTeX-mode-map)
(run-hooks 'yatex-mode-load-hook)

;; `History' was moved to ChangeLog
;----------------------------- End of yatex.el -----------------------------

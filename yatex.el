;;; -*- Emacs-Lisp -*-
;;; Yet Another tex-mode for emacs.
;;; yatex.el rev.1.27
;;; (c)1991 by Hirose Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Thu Jun  4 20:03:06 1992 on figaro

(provide 'yatex-mode)
(defconst YaTeX-revision-number "1.27"
  "Revision number of running yatex.el"
)

;---------- Local variables ----------
;;;
;; Initialize local variable for yatex-mode.
;; Preserving user preferred definitions.
;; ** Check all of these defvar-ed values **
;; ** and setq other values more suitable **
;; ** for your site, if nedded.           **
;;;
(defvar YaTeX-prefix "\^C"
  "Prefix key to trigger YaTeX functions.
You can select favorite prefix key by setq in your ~/.emacs."
)
(defvar YaTeX-open-lines 1
  "Blank lines between text and \??{??}"
)
(defvar YaTeX-fill-prefix "\t"
  "fill-prefix used for auto-fill-mode.
The defalut value is single TAB."
)
(defvar YaTeX-user-completion-table "~/.yatexrc"
  "Default filename in which user completion table is saved."
)
(defvar tex-command "jlatex"
  "Default command for compiling LaTeX text."
)
(defvar dvi2-command		;previewer command for your site
  (concat
   "xdvi -geo +0+0 -s 4 -display "
   (getenv "DISPLAY"))
  "Default previewer command including its option.
This default value is for X window system.  If you want to use this
default, you have to make sure the environment variable DISPLAY is
correctly set."
)
(defvar latex-warning-regexp "line.* [0-9]*"
  "Regular expression of line number of warning message by latex command."
)
(defvar latex-error-regexp "l\\.[1-9][0-9]*"
  "Regular expression of line number of latex error.  Perhaps your latex
command stops at this error message with line number of LaTeX source text."
)
(defvar latex-dos-emergency-message "Emergency stop"
  "Because Demacs (GNU Emacs on DOS) cannot have pararell process, the
latex command which is stopping on a LaTeX error, is terminated by Demacs.
Many latex command on DOS display some message when it is terminated by
other process, user or OS.  Define this variable a message string of your
latex command on DOS shows at abnormal termination.
  Remember Demacs's call-process function is not oriented for interactive
process."
)

;------------ Completion table ------------
; Set tex-section-like command possible completion
(setq section-table
      '(("part") ("section") ("subsection") ("subsubsection")
	("author") ("documentstyle") ("pagestyle")
	("documentstyle[10pt]") ("documentstyle[11pt]")
	("documentstyle[12pt]")
	("item[]") ("title") ("chapter") ("part") ("paragraph")
	("subparagraph") ("underline") ("label") ("footnote")
	("hspace*") ("vspace*") ("bibliography") ("bibitem[]") ("cite[]")
	("nocite") ("input") ("include") ("includeonly") ("mbox") ("hbox")
	("caption") ("newcommand") ("setlength") ("addtolength")
	("newenvironment") ("newtheorem")
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
))
(defvar user-article-table nil)

; Set tex-environment possible completion
(setq env-table
      '(("quote") ("quotation") ("center") ("verse") ("document")
	("verbatim") ("itemize") ("enumerate") ("description")
	("list{}") ("tabular") ("table") ("titlepage")
	("sloppypar") ("ref") ("quotation") ("quote") ("picture")
	("eqnarray") ("figure") ("equation") ("abstract") ("array")
	("thebibliography") ("theindex")
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
      '(("maketitle") ("sloppy")
	("alpha") ("beta") ("gamma") ("delta") ("epsilon") ("varepsilon")
	("zeta") ("eta") ("theta")("vartheta") ("iota") ("kappa")
	("lambda") ("mu") ("nu") ("xi") ("pi") ("varpi") ("rho") ("varrho")
	("sigma") ("varsigma") ("tau") ("upsilon") ("phi") ("varphi")
	("chi") ("psi") ("omega") ("Gamma") ("Delta") ("Theta") ("Lambda")
	("Xi") ("Pi") ("Sigma") ("Upsilon") ("Phi") ("Psi") ("Omega")
	("LaTeX") ("TeX")
))
(defvar user-singlecmd-table nil)

;---------- Key mode map ----------
;;;
;; Create new key map: YaTeX-mode-map
;; Do not change this section.
;;;
(defvar YaTeX-mode-map nil
  "Keymap used in YaTeX mode."
)
(defvar YaTeX-compilation-mode-map nil
  "Keymap userd in YaTeX compilation buffer."
)

;---------- Define deafult key bindings on YaTeX mode map ----------
;;;
;; Define key table
;;;
(if YaTeX-mode-map 
    nil
  (global-unset-key (concat YaTeX-prefix "b"))
  (setq YaTeX-mode-map (make-sparse-keymap))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "tj")
    'YaTeX-compile)
  (define-key YaTeX-mode-map (concat YaTeX-prefix "tx")
    'YaTeX-preview)
  (define-key YaTeX-mode-map (concat YaTeX-prefix "tv")
    'YaTeX-view-error)
  (define-key YaTeX-mode-map (concat YaTeX-prefix "tb")
    '(lambda () (interactive) (YaTeX-insert-string "\\")))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "'")
    'YaTeX-prev-error)
  (define-key YaTeX-mode-map (concat YaTeX-prefix " ")
    'YaTeX-do-completion)
  (define-key YaTeX-mode-map (concat YaTeX-prefix "v")
    'YaTeX-version)

  (define-key YaTeX-mode-map "\"" 'YaTeX-insert-quote)
  (define-key YaTeX-mode-map (concat YaTeX-prefix "{")
    'YaTeX-insert-braces)
  (define-key YaTeX-mode-map (concat YaTeX-prefix "}")
    'YaTeX-insert-braces-region)
  (define-key YaTeX-mode-map (concat YaTeX-prefix "d")
    'YaTeX-insert-dollar)
  (define-key YaTeX-mode-map (concat YaTeX-prefix "\\")
    '(lambda () (interactive) (YaTeX-insert-string "$\\backslash$")))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "bd")
    '(lambda (arg) (interactive "P")
       (YaTeX-insert-begin-end "document" arg)))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "Bd")
    '(lambda () (interactive)
       (YaTeX-insert-begin-end "document" t)))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "bD")
    '(lambda (arg) (interactive "P")
       (YaTeX-insert-begin-end "description" arg)))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "BD")
    '(lambda () (interactive)
       (YaTeX-insert-begin-end "description" t)))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "be")
    '(lambda (arg) (interactive "P")
       (YaTeX-insert-begin-end "enumerate" arg)))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "Be")
    '(lambda () (interactive)
       (YaTeX-insert-begin-end "enumerate" t)))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "bi")
    '(lambda (arg) (interactive "P")
       (YaTeX-insert-begin-end "itemize" arg)))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "Bi")
    '(lambda () (interactive)
       (YaTeX-insert-begin-end "itemize" t)))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "bt")
    '(lambda (arg) (interactive "P")
       (YaTeX-insert-begin-end "tabbing" arg)))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "Bt")
    '(lambda () (interactive)
       (YaTeX-insert-begin-end "tabbing" t)))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "bT")
    '(lambda (arg) (interactive "P")
       (YaTeX-insert-begin-end "tabular" arg)))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "BT")
    '(lambda () (interactive)
       (YaTeX-insert-begin-end "tabular" t)))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "bq")
    '(lambda (arg) (interactive "P")
       (YaTeX-insert-begin-end "quote" arg)))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "Bq")
    '(lambda () (interactive)
       (YaTeX-insert-begin-end "quote" t)))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "bQ")
    '(lambda (arg) (interactive "P")
       (YaTeX-insert-begin-end "quotation" arg)))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "BQ")
    '(lambda () (interactive)
       (YaTeX-insert-begin-end "quotation" t)))
  (define-key YaTeX-mode-map (concat YaTeX-prefix "b ")
    'YaTeX-make-begin-end)
  (define-key YaTeX-mode-map (concat YaTeX-prefix "B ")
    'YaTeX-make-begin-end-region)
  (define-key YaTeX-mode-map (concat YaTeX-prefix "s")
    'YaTeX-make-section)
  (define-key YaTeX-mode-map (concat YaTeX-prefix "l")
    'YaTeX-make-fontsize)
  (define-key YaTeX-mode-map (concat YaTeX-prefix "L")
    'YaTeX-make-fontsize-region)
  (define-key YaTeX-mode-map (concat YaTeX-prefix "m")
    'YaTeX-make-singlecmd)
  (define-key YaTeX-mode-map (concat YaTeX-prefix "\C-m")
    '(lambda () (interactive) (YaTeX-insert-string "\\\\")))
  (if (eq system-type 'ms-dos)
      (define-key YaTeX-mode-map (concat YaTeX-prefix "\^L")
	'(lambda () (interactive)
	   (set-screen-height 24) (recenter))))
)

(if YaTeX-compilation-mode-map nil
  (setq YaTeX-compilation-mode-map (make-keymap))
  (suppress-keymap YaTeX-compilation-mode-map t)
  (define-key YaTeX-compilation-mode-map " "
    'YaTeX-jump-error-line)
)

;---------- Customize as you like above ----------

;---------- Kanji code selection ----------
(if (eq system-type 'ms-dos)
    (setq YaTeX-kanji-code 1)
  (setq YaTeX-kanji-code 2))

(setq kanji-display-code YaTeX-kanji-code
      kanji-fileio-code  YaTeX-kanji-code)
;---------- Define other variable ----------
(defvar env-name "document")		;Initial tex-environment completion
(defvar section-name "documentstyle[12pt]") ;Initial tex-section completion
(defvar fontsize-name "large")		;Initial fontsize completion
(defvar single-command "maketitle")	;Initial LaTeX single command
(defvar YaTeX-user-table-has-read nil
  "Flag that means whether user completion table has read or not."
)
(defvar yatex-mode-hook nil
  "List of functions to be called after .tex file is read
and yatex-mode starts.")

;---------- Produce YaTeX-mode ----------
;;;
;; Major mode definition
;;;
(defun yatex-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'YaTeX-mode)
  (setq mode-name "やてふもーど")
  (turn-on-auto-fill)
  (make-local-variable 'fill-column)
  (make-local-variable 'fill-prefix)
  (setq fill-column 72
	fill-prefix YaTeX-fill-prefix)
  (use-local-map YaTeX-mode-map)
  (if (eq system-type 'ms-dos)
      (set-screen-height 24))
  (if YaTeX-user-table-has-read nil
    (YaTeX-read-user-completion-table)
    (setq YaTeX-user-table-has-read t))
  (run-hooks 'yatex-mode-hook)
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
	(insert "\\begin{" env "}\n")
	(exchange-point-and-mark)
	(insert "\\end{" env "}\n"))
    (delete-blank-lines)
    (insert "\\begin{" env "}\n")
    (newline (1+ (* 2 YaTeX-open-lines)))
    (insert "\\end{" env "}\n")
    (previous-line (+ 2 YaTeX-open-lines)))
)

(defun YaTeX-exist-completion-table (elm table)
  "Return nil, if single list element:elm was
not found in possible completion table."
  (while (not (or (null table) (equal elm (car table))))
    (setq table (cdr table)))
  table
)

(defun YaTeX-make-begin-end (arg)
  "Make LaTeX environment command of \\begin{env.} ... \\end{env.}
by completing read.
 If you invoke this command with universal argument,
\(C-u or ESC-1 is typical prefix to invoke commands with ARG.\)
you can put REGION into that environment between \\begin and \\end."
  (interactive "P")
  (let*
      ((mode (if arg " region" ""))
       (env
	(completing-read
	 (format "Begin environment%s(default %s): " mode env-name)
	 (append user-env-table env-table) nil nil)))
    (if (string= env "")
	(setq env env-name))
    (setq env-name env)
    (if (not (YaTeX-exist-completion-table
	      (list env-name) (append user-env-table env-table)))
	(setq user-env-table (cons (list env-name) user-env-table)))
    (YaTeX-insert-begin-end env-name arg))
)

(defun YaTeX-make-begin-end-region ()
  "Call YaTeX-make-begin-end with ARG to specify region mode."
  (interactive)
  (YaTeX-make-begin-end t)
)

(defun YaTeX-make-section (arg)
  "Make LaTeX \\section{} type command with completing read.
With ARG of numeric, you can specify the number of argument of
LaTeX command.
  For example, if you want to produce LaTeX command

	\\addtolength{\\topmargin}{8mm}

which has two argument.  You can produce that sequence by typing...
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
    (if (not (YaTeX-exist-completion-table
	      (list section-name) (append user-section-table section-table)))
	(setq user-section-table
	      (cons (list section-name) user-section-table)))
    (insert "\\" section-name "{" title "}")
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
\(C-u or ESC-1 are default key bindings of universal-argument.\)"
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
    (if (not (YaTeX-exist-completion-table
	      (list fontsize-name)
	      (append user-fontsize-table fontsize-table)))
	(setq user-fontsize-table
	    (cons (list fontsize-name) user-fontsize-table)))
    (if arg
	(save-excursion
	  (if (> (point) (mark)) (exchange-point-and-mark))
	  (insert "{\\" fontsize-name " ")
	  (exchange-point-and-mark)
	  (insert "}"))
      (insert "{\\" fontsize-name " }")
      (forward-char -1)))
)

(defun YaTeX-make-fontsize-region ()
  "Call functino:YaTeX-make-fontsize with ARG to specify region mode."
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
  (if (not (YaTeX-exist-completion-table
	    (list single-command)
	    (append user-singlecmd-table singlecmd-table)))
      (setq user-singlecmd-table
	    (cons (list single-command) user-singlecmd-table)))
  (insert "\\" single-command " ")
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
    ((= (preceding-char) ?\\ ) ?\")
    ((= (preceding-char) ?\( ) ?\")
    ((= (preceding-char) 32)  "``")
    ((= (preceding-char) 9)   "``")
    ((= (preceding-char) ?\n) "``")
    ((bobp) "``")
    (t  "''")
)))


(defun YaTeX-insert-braces-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (insert "}")
    (goto-char beg)
    (insert "{"))
)

(defun YaTeX-insert-braces ()
  (interactive)
  (insert "{}")
  (forward-char -1)
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
   (concat "Yet Another TeX mode 「野鳥」 Revision "
	   YaTeX-revision-number))
)

(defun YaTeX-compile-sentinel (proc mes)
  (cond ((null (buffer-name (process-buffer proc)))
         ;; buffer killed
         (set-process-buffer proc nil))
        ((memq (process-status proc) '(signal exit))
         (let* ((obuf (current-buffer)))
           ;; save-excursion isn't the right thing if
           ;;  process-buffer is current-buffer
           (unwind-protect
               (progn
                 ;; Write something in *compilation* and hack its mode line
                 (set-buffer (process-buffer proc))
                 (goto-char (point-max))
                 (insert ?\n "jlatex compilation " mes)
                 (forward-char -1)
                 (insert " at "
                         (substring (current-time-string) 0 -5))
		 (insert "\n * Hit any key to return * ")
                 (forward-char 1)
                 (setq mode-line-process
                       (concat ": "
                               (symbol-name (process-status proc))))
                 ;; If buffer and mode line will show that the process
                 ;; is dead, we can delete it now.  Otherwise it
                 ;; will stay around until M-x list-processes.
                 (delete-process proc)
		 )
             (setq YaTeX-compilation-process nil)
             ;; Force mode line redisplay soon
             (set-buffer-modified-p (buffer-modified-p))
	     )
	   (set-buffer obuf)
)))
)

(defvar YaTeX-compilation-process nil
  "Process identifier for jlatex"
)

(defun YaTeX-compile ()
  "Execute jlatex (or other) to LaTeX compile."
  (interactive)
  (basic-save-buffer)
  (if YaTeX-compilation-process
   (if (eq (process-status YaTeX-compilation-process) 'run)
	(progn (interrupt-process YaTeX-compilation-process)
	       (sit-for 1)
	       (delete-process YaTeX-compilation-process))
      nil) nil)
;  (compile1 (concat tex-command " " (buffer-name))
;	    "TeX error" "*TeX compilation*")
  (setq YaTeX-compilation-process nil)
  (if (eq system-type 'ms-dos)				;if MS-DOS
      (with-output-to-temp-buffer "*YaTeX-compilation*"
	(message (concat "Compiling " (buffer-name) "..."))
	(call-process shell-file-name nil "*YaTeX-compilation*" nil
		      "/c " tex-command (buffer-name) ))
    (setq YaTeX-compilation-process			;if UNIX
	  (with-output-to-temp-buffer "*YaTeX-compilation*"
	    (start-process "LaTeX" "*YaTeX-compilation*" shell-file-name "-c"
			   (concat tex-command " "(buffer-name) ""))
	    ))
    (set-process-sentinel YaTeX-compilation-process 'YaTeX-compile-sentinel))
  (setq current-TeX-buffer (buffer-name))
  (other-window 1)
  (use-local-map YaTeX-compilation-mode-map)
  (set-kanji-process-code YaTeX-kanji-code)
  (message "Type SPC to continue.")
  (goto-char (point-max))
  (sit-for 30)
  (read-char)	;hit any key
  (other-window -1)
)

(defun YaTeX-preview (preview-command preview-file)
  "Execute xdvi (or other) to tex-preview."
  (interactive
   (list (read-string "Preview command: " dvi2-command)
	 (read-string "Prefiew file[.dvi]: "
		      (substring (buffer-name) 0 -4)
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
  (setq cur-buf (buffer-name)
	YaTeX-error-line nil)
  (if (null (get-buffer "*YaTeX-compilation*"))
      (message "There is no output buffer of compilation.")
    (pop-to-buffer "*YaTeX-compilation*")
    (if (eq system-type 'ms-dos)
	(if (search-backward latex-dos-emergency-message nil t)
	    (progn (goto-char (point-max))
		   (setq error-regexp latex-error-regexp))
	  (beginning-of-line)
	  (forward-char -1)
	  (setq error-regexp latex-warning-regexp))
      (if YaTeX-compilation-process      ; if jlatex on UNIX
	  (if (eq (process-status YaTeX-compilation-process) 'run)
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
	  (skip-chars-forward "[0-9]")
	  (narrow-to-region (point) (mark))
	  (goto-char (point-min))
	  (setq YaTeX-error-line (read (current-buffer))))
      (message "No more error on %s" cur-buf)
      (ding)
      )
    (other-window -1)
    (switch-to-buffer cur-buf)
    (if (null YaTeX-error-line)
	nil
      (goto-line YaTeX-error-line)
      (message "latex error or warning at line: %d" YaTeX-error-line)
      (other-window 1)
      (skip-chars-backward "[0-9]")
      (recenter (/ (window-height) 2))
      (sit-for 3)
      (forward-line -1)
      (other-window -1)
      ))
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
	(narrow-to-region (match-beginning 0) (match-end 0))
	(goto-char (point-min))
	(let ((error-line (read (current-buffer))))
	  (other-window -1)
	  (switch-to-buffer current-TeX-buffer)
	  (goto-line error-line)))))
)

(defun YaTeX-view-error ()
  (interactive)
  (other-window 1)
  (goto-char (point-max))
  (other-window -1)
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
  (message "Saving user table in %s" YaTeX-user-completion-table)
  (find-file (expand-file-name YaTeX-user-completion-table))
  (erase-buffer)
;  (prin1-to-string user-section-table)
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
)

(defun append-to-hook (hook hook-list)
  "Add hook-list to certain emacs's hook correctly.
Argument hook-list is the list of function int the form to be called
Call this function with argument as next example,
	(append-to-hook '((ding))) ;If one function to add.
	(append-to-hook '((func1)(func2 arg)))."
  (if (null (eval hook))   			;Not defined
      (set hook
	   (append '(lambda ()) hook-list))
    (if (listp (eval hook))
	(if (eq (car (eval hook)) 'lambda)	;'(lambda () ....)
	    (set hook
		 (append (eval hook) hook-list))
	  (if (eq hook 'kill-emacs-hook)	;'(hook1 hook2 ...)
	      (progn				; this format is not
		(ding)				; for kill-emacs-hook
		(message
		 "Caution!! you have wrong format of kill-emacs-hook"))
	    (while (not (null hook-list))
	      (set hook
		   (append (eval hook) (car hook-list)))
	      (setq hook-list (cdr hook-list))))
	  )
      (set hook					;'hook
	   (append '(lambda ())
		   (cons (list (eval hook)) hook-list)))))
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
; 1.27 |     1/21 | Default name on completing-read,
;------+----------+---------------------------------------------------------
;
;----------------------------- End of yatex.el -----------------------------

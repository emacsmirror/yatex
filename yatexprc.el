;;; -*- Emacs-Lisp -*-
;;; YaTeX process handler.
;;; yatexprc.el rev.1.44
;;; (c)1993 by HIROSE Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Mon Oct 25 17:48:39 1993 on figaro
;;; $Id$

(require 'yatex)

(defvar YaTeX-typeset-process nil
  "Process identifier for jlatex"
)
(defvar YaTeX-typeset-buffer "*YaTeX-typesetting*"
  "Process buffer for jlatex")

(defvar YaTeX-typeset-buffer-syntax nil
  "*Syntax table for typesetting buffer")

(if YaTeX-typeset-buffer-syntax nil
  (setq YaTeX-typeset-buffer-syntax
	(make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?\{ "w" YaTeX-typeset-buffer-syntax)
  (modify-syntax-entry ?\} "w" YaTeX-typeset-buffer-syntax)
  (modify-syntax-entry ?\[ "w" YaTeX-typeset-buffer-syntax)
  (modify-syntax-entry ?\] "w" YaTeX-typeset-buffer-syntax)
)

(defun YaTeX-typeset (command buffer)
  "Execute jlatex (or other) to LaTeX typeset."
  (interactive)
  (let ((window (selected-window)))
    (if (and YaTeX-typeset-process
	     (eq (process-status YaTeX-typeset-process) 'run))
	;; if tex command is halting.
	(YaTeX-kill-typeset-process YaTeX-typeset-process))
    (YaTeX-visit-main t);;execution directory
    ;;Select under-most window if there are more than 2 windows and
    ;;typeset buffer isn't seen.
    (YaTeX-showup-buffer
     buffer (function (lambda (x) (nth 3 (window-edges x)))))
    (with-output-to-temp-buffer buffer
      (if (eq system-type 'ms-dos)	;if MS-DOS
	  (progn
	    (message (concat "Typesetting " (buffer-name) "..."))
	    (YaTeX-put-nonstopmode)
	    (call-process shell-file-name
			  nil buffer nil "/c" command)
	    (YaTeX-remove-nonstopmode))
	(setq YaTeX-typeset-process	;if UNIX
	      (start-process "LaTeX" buffer shell-file-name "-c"
			     command))
	(set-process-sentinel YaTeX-typeset-process 'YaTeX-typeset-sentinel)))
    (setq current-TeX-buffer (buffer-name))
    (select-window (get-buffer-window buffer))
    (use-local-map YaTeX-typesetting-mode-map)
    (set-syntax-table YaTeX-typeset-buffer-syntax)
    (setq mode-name "typeset")
    (if YaTeX-typeset-process		; if process is running (maybe on UNIX)
	(cond ((boundp 'MULE)
	       (set-current-process-coding-system
		YaTeX-latex-message-code YaTeX-coding-system))
	      ((boundp 'NEMACS)
	       (set-kanji-process-code YaTeX-latex-message-code))))
    (message "Type SPC to continue.")
    (goto-char (point-max))
    (if (eq system-type 'ms-dos) (message "Done.")
      (insert (message " "))
      (set-marker (process-mark YaTeX-typeset-process) (1- (point))))
    (if (bolp) (forward-line -1))
    (recenter -1)
    (select-window window))
)

(defun YaTeX-typeset-sentinel (proc mes)
  (cond ((null (buffer-name (process-buffer proc)))
         ;; buffer killed
         (set-process-buffer proc nil))
        ((memq (process-status proc) '(signal exit))
         (let* ((obuf (current-buffer)) (pbuf (process-buffer proc))
		(owin (selected-window)) win)
           ;; save-excursion isn't the right thing if
           ;;  process-buffer is current-buffer
           (unwind-protect
               (progn
                 ;; Write something in *typesetting* and hack its mode line
		 (YaTeX-pop-to-buffer pbuf)
		 (set-buffer (process-buffer proc))
                 (goto-char (point-max))
		 (recenter -3)
                 (insert ?\n "latex typesetting " mes)
                 (forward-char -1)
                 (insert " at " (substring (current-time-string) 0 -5) "\n")
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
	   (select-window owin)
	   (set-buffer obuf))))
)

(defvar YaTeX-texput-file "texput.tex"
  "*File name for temporary file of typeset-region."
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
	 (cmd (concat (YaTeX-get-latex-command nil) " " YaTeX-texput-file))
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
      (YaTeX-visit-main t)
      (setq opoint (point))
      (goto-char (point-min))
      (setq
       preamble
       (if (re-search-forward "^[ 	]*\\\\begin.*{document}" nil t)
	   (buffer-substring (point-min) (match-end 0))
	 (concat "\\documentstyle{" YaTeX-default-document-style "}\n"
		 "\\begin{document}")))
      (goto-char opoint)
      ;;(set-buffer buffer)		;for clarity
      (find-file YaTeX-texput-file)
      (erase-buffer)
      (if YaTeX-need-nonstop
	  (insert "\\nonstopmode{}\n"))
      (insert preamble "\n")
      (insert region)
      (insert "\\typeout{" typeout end "}\n") ;Notice the selected method.
      (insert "\n\\end{document}\n")
      (basic-save-buffer)
      (kill-buffer (current-buffer))
      (YaTeX-visit-main t)
      (YaTeX-typeset cmd YaTeX-typeset-buffer)
      (put 'dvi2-command 'region t)))
)

(defun YaTeX-typeset-buffer ()
  "Typeset whole buffer.  If %#! usage says other buffer is main text,
visit main buffer to confirm if its includeonly list contains current
buffer's file.  And if it doesn't contain editing text, ask user which
action want to be done, A:Add list, R:Replace list, %:comment-out list."
  (interactive)
  (YaTeX-save-buffers)
  (let*((me (substring (buffer-name) 0 (rindex (buffer-name) ?.)))
	(mydir (file-name-directory (buffer-file-name)))
	(cmd (YaTeX-get-latex-command t)))
    (if (YaTeX-main-file-p) nil
      (save-excursion
	(YaTeX-visit-main t)	;search into main buffer
	(save-excursion
	  (push-mark (point) t)
	  (goto-char (point-min))
	  (if (and (re-search-forward "^[ 	]*\\\\begin{document}" nil t)
		   (re-search-backward "^[ 	]*\\\\includeonly{" nil t))
	      (let*
		  ((b (progn (skip-chars-forward "^{") (point)))
		   (e (progn (skip-chars-forward "^}") (1+ (point))))
		   (s (buffer-substring b e)) c
		   (pardir (file-name-directory (buffer-file-name))))
		(if (string-match (concat "[{,/]" me "[,}]") s)
		    nil ; Nothing to do when it's already in includeonly.
		  (ding)
		  (switch-to-buffer (current-buffer));Display this buffer.
		  (setq
		   me	  ;;Rewrite my name(me) to contain sub directory name.
		   (concat
		    (if (string-match pardir mydir) ;if mydir is child of main
			(substring mydir (length pardir)) ;cut absolute path
		      mydir) ;else concat absolute path name.
		    me))
		  (message
		   "`%s' is not in \\includeonly. A)dd R)eplace %%)comment? "
		   me)
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
	))
    (YaTeX-typeset cmd YaTeX-typeset-buffer)
    (put 'dvi2-command 'region nil))
)

(defun YaTeX-call-command-on-file (base-cmd buffer)
  (YaTeX-save-buffers)
  (YaTeX-typeset
   (read-string "Call command: "
		(concat base-cmd " " (YaTeX-get-preview-file-name)))
   buffer)
)

(defun YaTeX-bibtex-buffer (cmd)
  "Pass the bibliography data of editing file to bibtex."
  (interactive)
  (YaTeX-save-buffers)
  (YaTeX-call-command-on-file cmd "*YaTeX-bibtex*" )
)

(defun YaTeX-kill-typeset-process (proc)
  "Kill process PROC after sending signal to PROC.
PROC should be process identifier."
  (cond
   ((eq system-type 'ms-dos)
    (error "MS-DOS can't have concurrent process."))
   ((or (null proc) (not (eq (process-status proc) 'run)))
    (error "No typesetting process."))
   (t (interrupt-process proc)
      (delete-process proc)))
)

(defun YaTeX-system (command buffer)
  "Execute some command on buffer.  Not a official function."
  (save-excursion
    (with-output-to-temp-buffer buffer
      (if (eq system-type 'ms-dos)
	  (call-process shell-file-name nil buffer nil "/c " command)
	(start-process "system" buffer shell-file-name "-c " command))))
)

(defun YaTeX-preview (preview-command preview-file)
  "Execute xdvi (or other) to tex-preview."
  (interactive
   (list (read-string "Preview command: " dvi2-command)
	 (read-string "Prefiew file[.dvi]: "
		      ;;(substring (buffer-name) 0 -4)
		      (if (get 'dvi2-command 'region)
			  (substring YaTeX-texput-file
				     0 (rindex YaTeX-texput-file ?.))
			(YaTeX-get-preview-file-name))
		      )))
  (setq dvi2-command preview-command)
  (save-excursion
    (YaTeX-visit-main t)
    (with-output-to-temp-buffer "*dvi-preview*"
      (if (eq system-type 'ms-dos)
	  (progn (send-string-to-terminal "\e[2J")	;if MS-DOS
		 (call-process shell-file-name "con" "*dvi-preview*" nil
			       "/c " dvi2-command preview-file)
		 (redraw-display))
	(start-process "preview" "*dvi-preview*" shell-file-name "-c"
		       (concat dvi2-command " " preview-file)) ;if UNIX
	(message
	 (concat "Starting " dvi2-command " to preview " preview-file)))))
)

(defun YaTeX-prev-error ()
  "Visit previous error.  The reason why not NEXT-error is to
avoid make confliction of line numbers by editing."
  (interactive)
  (let ((cur-buf (buffer-name)) (cur-win (selected-window))
	YaTeX-error-line typeset-win error-buffer error-win)
    (if (null (get-buffer YaTeX-typeset-buffer))
	(message "There is no output buffer of typesetting.")
      (YaTeX-pop-to-buffer YaTeX-typeset-buffer)
      (setq typeset-win (selected-window))
      (if (eq system-type 'ms-dos)
	  (if (search-backward latex-dos-emergency-message nil t)
	      (progn (goto-char (point-max))
		     (setq error-regexp latex-error-regexp))
	    (beginning-of-line)
	    (forward-char -1)
	    (setq error-regexp latex-warning-regexp))
	(if YaTeX-typeset-process      ; if jlatex on UNIX
	    (if (eq (process-status YaTeX-typeset-process) 'run)
		(progn
		  (goto-char (point-max))
		  (setq error-regexp latex-error-regexp))
	      (beginning-of-line)
	      (setq error-regexp latex-warning-regexp))))
      (if (re-search-backward error-regexp nil t)
	  (setq YaTeX-error-line
		(string-to-int
		 (buffer-substring
		  (progn (goto-char (match-beginning 0))
			 (skip-chars-forward "^0-9")
			 (point))
		  (progn (skip-chars-forward "0-9") (point)))))
	(message "No more error on %s" cur-buf)
	(ding))
      (setq error-buffer (YaTeX-get-error-file cur-buf)); arg. is default buf.
      (setq error-win (get-buffer-window error-buffer))
      (select-window cur-win)
      (if (or (null YaTeX-error-line) (equal 0 YaTeX-error-line))
	  nil
	;; if warning or error found
	(if error-win (select-window error-win)
	  (select-window (get-lru-window))
	  (YaTeX-switch-to-buffer error-buffer)
	  (setq error-win (selected-window)))
	(goto-line YaTeX-error-line)
	(message "latex error or warning in '%s' at line: %d"
		 error-buffer YaTeX-error-line)
	(select-window typeset-win)
	(skip-chars-backward "[0-9]")
	(recenter (/ (window-height) 2))
	(sit-for 3)
	(forward-char -1)
	(select-window error-win)
	)))
)

(defun YaTeX-jump-error-line ()
  "Jump corresponding line on latex command's error message."
  (interactive)
  (let ((p (point))
	(end (progn (end-of-line) (point)))
	(begin (progn (beginning-of-line)(point))))
    (if (null (re-search-forward "l[ ines]*\\.*[1-9][0-9]*" end t))
	(if (save-excursion (end-of-line) (eobp))
	    (progn (goto-char p) (insert (this-command-keys)))
	  (message "No line number expression"))
      (goto-char (match-beginning 0))
      (re-search-forward "[1-9][0-9]*" end t)
      (save-restriction
	(let ((error-line
	       (string-to-int (buffer-substring (match-beginning 0)
						(match-end 0))))
	      (error-file (YaTeX-get-error-file current-TeX-buffer)))
	  ;;(goto-char (match-beginning 0))
	  (other-window -1)
	  (message "errors in %s" error-file)
	  ;(switch-to-buffer current-TeX-buffer)
	  (if (not (YaTeX-switch-to-buffer error-file))
	      (error "%s is not found in this directory."))
	  (goto-line error-line)))))
)

(defun YaTeX-send-string ()
  "Send string to current typeset process."
  (interactive)
  (if (and (eq (process-status YaTeX-typeset-process) 'run)
	   (>= (point) (process-mark YaTeX-typeset-process)))
      (let ((b (process-mark YaTeX-typeset-process))
	    (e (point-end-of-line)))
	(goto-char b)
	(skip-chars-forward " \t" e)
	(setq b (point))
	(process-send-string
	 YaTeX-typeset-process (concat (buffer-substring b e) "\n"))
	(goto-char e)
	(insert "\n")
	(set-marker (process-mark YaTeX-typeset-process) (point))
	(insert " "))
    (ding))
)

(defun YaTeX-view-error ()
  (interactive)
  (if (null (get-buffer YaTeX-typeset-buffer))
      (message "No typeset buffer found.")
    (let ((win (selected-window)))
      (YaTeX-pop-to-buffer YaTeX-typeset-buffer)
      (goto-char (point-max))
      (recenter -1)
      (select-window win)))
)

(defun YaTeX-get-error-file (default)
  "Get current processing file from typesetting log."
  (save-excursion
    (let(s)
      (condition-case () (up-list -1)
	(error
	 (let ((list 0) found)
	   (while
	       (and (<= list 0) (not found)
		    (re-search-backward "\\((\\)\\|\\()\\)" nil t))
	     (if (equal (match-beginning 0) (match-beginning 2)) ;close paren.
		 (setq list (1- list)) ;open paren
	       (setq list (1+ list))
	       (if (= list 1)
		   (if (looking-at "\\([^,{}%]+\.\\)tex\\|sty")
		       (setq found t)
		     (setq list (1- list)))))))))
      (setq s
	    (buffer-substring
	     (progn (forward-char 1) (point))
	     (progn (skip-chars-forward "-A-Za-z0-9_/\.\\" (point-end-of-line))
		    (point))))
      (if (string= "" s) default s)))
)
      
(defun YaTeX-put-nonstopmode ()
  (if YaTeX-need-nonstop
      (if (re-search-backward "\\nonstopmode{}" (point-min) t)
	  nil                    ;if already written in text then do nothing
	(save-excursion
	  (YaTeX-visit-main t)
	  (goto-char (point-min))
	  (insert "\\nonstopmode{}%_YaTeX_%\n")))
    )
)

(defun YaTeX-remove-nonstopmode ()
  (if YaTeX-need-nonstop ;for speed
      (save-excursion
	(YaTeX-visit-main t)
	(goto-char (point-min))
	(forward-line 1)
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(delete-matching-lines "^\\\\nonstopmode\\{\\}%_YaTeX_%$")
	(widen)))
)

(defun YaTeX-get-preview-file-name ()
  "Get file name to preview by inquiring YaTeX-get-latex-command"
  (let* ((latex-cmd (YaTeX-get-latex-command t))
	 (rin (rindex latex-cmd ? ))
	 (fname (if (> rin -1) (substring latex-cmd (1+ rin)) ""))
	 (period))
    (if (string= fname "")
	(setq fname (substring (buffer-name) 0 -4))
      (setq period (rindex fname ?.))
      (setq fname (substring fname 0 (if (eq -1 period) nil period)))
      ))
)

(defun YaTeX-get-latex-command (&optional switch)
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
	    (skip-chars-forward "^ 	" (point-end-of-line)) ;Skip command
	    (skip-chars-forward " 	" (point-end-of-line))
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
     cmd 
     (YaTeX-replace-format
      cmd "f"
      (if (or arg (not (string-match "%f" cmd)))
	      ""
	    (YaTeX-replace-format
	     dviprint-from-format
	     "b"
	     (if (string=
		  (setq from (read-string "From page(default 1): ")) "")
		 "1" from))))
       )
    (setq
     cmd
     (YaTeX-replace-format
      cmd "t"
      (if (or arg (not (string-match "%t" cmd))
	      (string= 
	       (setq to (read-string "To page(default none): ")) ""))
	  ""
	(YaTeX-replace-format dviprint-to-format "e" to)))
     )
    (setq cmd (read-string "Edit command line: "
			   (format cmd (YaTeX-get-preview-file-name))))
    (save-excursion
      (YaTeX-visit-main t) ;;change execution directory
      (with-output-to-temp-buffer "*dvi-printing*"
	(if (eq system-type 'ms-dos)
	    (call-process shell-file-name "con" "*dvi-printing*" nil
			  "/c " cmd)
	  (start-process "print" "*dvi-printing*" shell-file-name "-c" cmd)
	  (message (concat "Starting " cmd " to printing "
			   (YaTeX-get-preview-file-name))))
    )))
)

(defun YaTeX-main-file-p ()
  "Return if current buffer is main LaTeX source."
  (string-match (concat "^" (YaTeX-get-preview-file-name) ".tex")(buffer-name))
)

(defun YaTeX-visit-main (&optional setbuf)
  "Switch to buffer main LaTeX source.  Use set-buffer instead of
switch-to-buffer if optional second argument SETBUF is t(Use it only
in Emacs-Lisp program)."
  (interactive)
  (let ((main-file (YaTeX-get-preview-file-name)))
    (if (string-match (concat "^" main-file ".tex") (buffer-name))
	(if (interactive-p) (message "I think this is main LaTeX source.") nil)
      (cond
       ((YaTeX-switch-to-buffer (setq main-file (concat main-file ".tex"))
				setbuf))
       ((and (file-exists-p (setq main-file (concat "../" main-file)))
	     (y-or-n-p (concat (expand-file-name main-file)
			       " is main file?:")))
	(YaTeX-switch-to-buffer main-file setbuf))
       (t (find-file (read-file-name "Enter your main text: " nil nil 1)))
	)))
  nil
)

(defun YaTeX-visit-main-other-window ()
  "Switch to buffer main LaTeX source in other window."
  (interactive)
  (if (YaTeX-main-file-p) (message "I think this is main LaTeX source.")
      (YaTeX-switch-to-buffer-other-window
       (concat (YaTeX-get-preview-file-name) ".tex")))
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

(defun YaTeX-save-buffers ()
  "Save buffers which is in yatex-mode."
  (basic-save-buffer)
  (save-excursion
    (mapcar '(lambda (buf)
	       (set-buffer buf)
	       (if (and (buffer-file-name buf)
			(eq major-mode 'yatex-mode)
			(buffer-modified-p buf)
			(y-or-n-p (format "Save %s" (buffer-name buf))))
		   (save-buffer buf)))
	    (buffer-list)))
)

(defun YaTeX-pop-to-buffer (buffer &optional win)
  (if (setq win (get-buffer-window buffer))
      (select-window win)
    (pop-to-buffer buffer))
)

(defun YaTeX-showup-buffer (buffer &optional func)
  "Make BUFFER show up in certain window (but current window)
that gives the maximum value by the FUNC.  FUNC should take an argument
of its window object"
  (or (get-buffer-window buffer)
      (< (length (YaTeX-window-list)) 3)
      (let ((window (selected-window)) (list (YaTeX-window-list)) win w (x 0))
	(while list
	  (setq w (car list))
	    (if (and (not (eq window w))
		     (> (funcall func w) x))
		(setq win w x (funcall func w)))
	    (setq list (cdr list)))
	(select-window win)
	(switch-to-buffer buffer)
	(select-window window)))
)

(defun YaTeX-window-list ()
  (let*((curw (selected-window)) (win curw) (wlist (list curw)))
    (while (not (eq curw (setq win (next-window win))))
      (setq wlist (cons win wlist)))
    wlist)
)
(provide 'yatexprc)

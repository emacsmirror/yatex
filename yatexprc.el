;;; -*- Emacs-Lisp -*-
;;; YaTeX process handler.
;;; yatexprc.el
;;; (c )1993-1995 by HIROSE Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Sat Jan 28 01:01:44 1995 on VFR
;;; $Id$

(require 'yatex)

(defvar YaTeX-typeset-process nil
  "Process identifier for jlatex"
)
(defvar YaTeX-typeset-buffer "*YaTeX-typesetting*"
  "Process buffer for jlatex")

(defvar YaTeX-typeset-buffer-syntax nil
  "*Syntax table for typesetting buffer")

(defvar YaTeX-current-TeX-buffer nil
  "Keeps the buffer on which recently typeset run.")

(defvar YaTeX-shell-command-option
  (or (and (boundp 'shell-command-option) shell-command-option)
      (if YaTeX-dos "/c" "-c"))
  "Shell option for command execution.")

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
  (save-excursion
    (let ((p (point)) (window (selected-window)) execdir (cb (current-buffer)))
      (if (and YaTeX-typeset-process
	       (eq (process-status YaTeX-typeset-process) 'run))
	  ;; if tex command is halting.
	  (YaTeX-kill-typeset-process YaTeX-typeset-process))
      (YaTeX-visit-main t) ;;execution directory
      (setq execdir default-directory)
      ;;Select lower-most window if there are more than 2 windows and
      ;;typeset buffer not seen.
      (YaTeX-showup-buffer
       buffer (function (lambda (x) (nth 3 (window-edges x)))))
      (set-buffer (get-buffer-create buffer))
      (setq default-directory execdir)
      (cd execdir)
      (erase-buffer)
      (cond
       (YaTeX-dos			;if MS-DOS
	(YaTeX-put-nonstopmode)
	(call-process
	 shell-file-name nil buffer nil YaTeX-shell-command-option command)
	(YaTeX-remove-nonstopmode))
       (t				;if UNIX
	(set-process-buffer
	 (setq YaTeX-typeset-process
	       (start-process "LaTeX" buffer shell-file-name
			      YaTeX-shell-command-option command))
	 (get-buffer buffer))
	(set-process-sentinel YaTeX-typeset-process 'YaTeX-typeset-sentinel)))
      (message (format "Calling `%s'..." command))
      (setq YaTeX-current-TeX-buffer (buffer-name))
      (use-local-map YaTeX-typesetting-mode-map)
      (set-syntax-table YaTeX-typeset-buffer-syntax)
      (setq mode-name "typeset")
      (if YaTeX-typeset-process		; if process is running (maybe on UNIX)
	  (cond ((boundp 'MULE)
		 (set-current-process-coding-system
		  YaTeX-latex-message-code YaTeX-coding-system))
		((boundp 'NEMACS)
		 (set-kanji-process-code YaTeX-latex-message-code))))
      (if YaTeX-dos (message "Done.")
	(insert " ")
	(set-marker (process-mark YaTeX-typeset-process) (1- (point))))
      (if (bolp) (forward-line -1))	;what for?
      (if (and YaTeX-emacs-19 window-system)
	  (let ((win (get-buffer-window buffer t)) owin)
	    (select-frame (window-frame win))
	    (setq owin (selected-window))
	    (select-window win)
	    (goto-char (point-max))
	    (recenter -1)
	    (select-window owin))
	(select-window (get-buffer-window buffer))
	(goto-char (point-max))
	(recenter -1))
      (select-window window)
      (switch-to-buffer cb)))
)

(defun YaTeX-typeset-sentinel (proc mes)
  (cond ((null (buffer-name (process-buffer proc)))
         ;; buffer killed
         (set-process-buffer proc nil))
        ((memq (process-status proc) '(signal exit))
         (let* ((obuf (current-buffer)) (pbuf (process-buffer proc))
		(pwin (get-buffer-window pbuf))
		(owin (selected-window)) win)
           ;; save-excursion isn't the right thing if
           ;;  process-buffer is current-buffer
           (unwind-protect
               (progn
                 ;; Write something in *typesetting* and hack its mode line
		 (if pwin
		     (select-window pwin)
		   (set-buffer pbuf))
		 ;;(YaTeX-showup-buffer pbuf nil t)
                 (goto-char (point-max))
		 (if pwin (recenter -3))
                 (insert ?\n "latex typesetting " mes)
                 (forward-char -1)
                 (insert " at " (substring (current-time-string) 0 -5) "\n")
                 (forward-char 1)
                 (setq mode-line-process
                       (concat ": "
                               (symbol-name (process-status proc))))
		 (message "latex typesetting %s."
			  (if (eq (process-status proc) 'exit)
			      "done" "ceased"))
                 ;; If buffer and mode line shows that the process
                 ;; is dead, we can delete it now.  Otherwise it
                 ;; will stay around until M-x list-processes.
                 (delete-process proc)
		 )
             (setq YaTeX-typeset-process nil)
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
  "Paste the region to the file `texput.tex' and execute typesetter.
The region is specified by the rule:
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
	 (texput YaTeX-texput-file)
	 (cmd (concat (YaTeX-get-latex-command nil) " " texput))
	 (buffer (current-buffer)) opoint preamble (subpreamble "") main
	 (hilit-auto-highlight nil)	;for Emacs19 with hilit19
	 reg-begin reg-end lineinfo)

      (save-excursion
	(if (search-backward "%#BEGIN" nil t)
	    (progn
	      (setq typeout "--- Region from BEGIN to "
		    end "the end of the buffer ---"
		    reg-begin (match-end 0))
	      (if (search-forward "%#END" nil t)
		  (setq reg-end (match-beginning 0)
			end "END ---")
		(setq reg-end (point-max))))
	  (setq typeout "=== Region from (point) to (mark) ==="
		reg-begin (point) reg-end (mark)))
	(goto-char (min reg-begin reg-end))
	(setq lineinfo (count-lines (point-min) (point-end-of-line)))
	(goto-char (point-min))
	(while (search-forward "%#REQUIRE" nil t)
	  (setq subpreamble
		(concat subpreamble
			(cond
			 ((eolp)
			  (buffer-substring
			   (match-beginning 0)
			   (point-beginning-of-line)))
			 (t (buffer-substring
			     (match-end 0)
			     (point-end-of-line))))
			"\n"))
	  (goto-char (match-end 0))))
      (YaTeX-visit-main t)
      (setq main (current-buffer))
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
      (let ((hilit-auto-highlight nil))
	(set-buffer (find-file-noselect texput)))
      ;;(find-file YaTeX-texput-file)
      (erase-buffer)
      (if YaTeX-need-nonstop
	  (insert "\\nonstopmode{}\n"))
      (insert preamble "\n" subpreamble "\n")
      (setq lineinfo (list (count-lines 1 (point-end-of-line)) lineinfo))
      (insert-buffer-substring buffer reg-begin reg-end)
      (insert "\\typeout{" typeout end "}\n") ;Notice the selected method.
      (insert "\n\\end{document}\n")
      (basic-save-buffer)
      (kill-buffer (current-buffer))
      (set-buffer main)		;return to parent file or itself.
      (YaTeX-typeset cmd YaTeX-typeset-buffer)
      (switch-to-buffer buffer)		;for Emacs-19
      (put 'dvi2-command 'region t)
      (put 'dvi2-command 'file buffer)
      (put 'dvi2-command 'offset lineinfo)
      ))
)

(defun YaTeX-typeset-buffer ()
  "Typeset whole buffer.  If %#! usage says other buffer is main text,
visit main buffer to confirm if its includeonly list contains current
buffer's file.  And if it doesn't contain editing text, ask user which
action wants to be done, A:Add list, R:Replace list, %:comment-out list."
  (interactive)
  (YaTeX-save-buffers)
  (let*((me (substring (buffer-name) 0 (rindex (buffer-name) ?.)))
	(mydir (file-name-directory (buffer-file-name)))
	(cmd (YaTeX-get-latex-command t)) (cb (current-buffer)))
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
	  (exchange-point-and-mark)))
      (switch-to-buffer cb))		;for 19
    (YaTeX-typeset cmd YaTeX-typeset-buffer)
    (put 'dvi2-command 'region nil))
)

(defvar YaTeX-call-command-history nil
  "Holds history list of YaTeX-call-command-on-file.")
(put 'YaTeX-call-command-history 'no-default t)
(defun YaTeX-call-command-on-file (base-cmd buffer)
  (YaTeX-save-buffers)
  (YaTeX-typeset
   (read-string-with-history
    "Call command: "
    (concat base-cmd " " (YaTeX-get-preview-file-name))
    'YaTeX-call-command-history)
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
   (YaTeX-dos
    (error "MS-DOS can't have concurrent process."))
   ((or (null proc) (not (eq (process-status proc) 'run)))
    (message "Typesetting process is not running."))
   (t
    (save-excursion
      (set-buffer (process-buffer proc))
      (save-excursion
	(goto-char (point-max))
	(beginning-of-line)
	(if (looking-at "\\? +$")
	    (let ((mp (point-max)))
	      (process-send-string proc "x\n")
	      (while (= mp (point-max)) (sit-for 1))))))
    (if (eq (process-status proc) 'run)
	(progn
	  (interrupt-process proc)
	  (delete-process proc)))))
)

(defun YaTeX-system (command buffer)
  "Execute some command on buffer.  Not a official function."
  (save-excursion
    (YaTeX-showup-buffer
     buffer (function (lambda (x) (nth 3 (window-edges x)))))
    (set-buffer (get-buffer-create buffer))
    (erase-buffer)
    (if YaTeX-dos
	(call-process
	 shell-file-name nil buffer nil YaTeX-shell-command-option command)
      (set-process-buffer
       (start-process
	"system" buffer shell-file-name YaTeX-shell-command-option command)
       (get-buffer buffer))))
)

(defvar YaTeX-preview-command-history nil
  "Holds minibuffer history of preview command.")
(put 'YaTeX-preview-command-history 'no-default t)
(defvar YaTeX-preview-file-history nil
  "Holds minibuffer history of file to preview.")
(put 'YaTeX-preview-file-history 'no-default t)
(defun YaTeX-preview (preview-command preview-file)
  "Execute xdvi (or other) to tex-preview."
  (interactive
   (list
    (read-string-with-history
     "Preview command: " dvi2-command 'YaTeX-preview-command-history)
    (read-string-with-history
     "Preview file[.dvi]: "
     (if (get 'dvi2-command 'region)
	 (substring YaTeX-texput-file
		    0 (rindex YaTeX-texput-file ?.))
       (YaTeX-get-preview-file-name))
     'YaTeX-preview-file-history)))
  (setq dvi2-command preview-command)	;`dvi2command' is buffer local
  (save-excursion
    (YaTeX-visit-main t)
    (let ((pbuffer "*dvi-preview*") (dir default-directory))
      (YaTeX-showup-buffer
       pbuffer (function (lambda (x) (nth 3 (window-edges x)))))
      (set-buffer (get-buffer-create pbuffer))
      (erase-buffer)
      (setq default-directory dir)	;for 18
      (cd dir)				;for 19
      (cond
       (YaTeX-dos			;if MS-DOS
	(send-string-to-terminal "\e[2J\e[>5h") ;CLS & hide cursor
	(call-process shell-file-name "con" "*dvi-preview*" nil
		      YaTeX-shell-command-option preview-command preview-file)
	(send-string-to-terminal "\e[>5l") ;show cursor
	(redraw-display))
       (t				;if UNIX
	(set-process-buffer
	 (start-process "preview" "*dvi-preview*" shell-file-name
			YaTeX-shell-command-option
			(concat preview-command " " preview-file))
	 (get-buffer pbuffer))
	(message
	 (concat "Starting " preview-command
		 " to preview " preview-file))))))
)

(defun YaTeX-set-virtual-error-position (file-sym line-sym)
  "Replace the value of FILE-SYM, LINE-SYM by virtual error position."
  (cond
   ((and (get 'dvi2-command 'region)
	 (> (symbol-value line-sym) (car (get 'dvi2-command 'offset))))
    (set file-sym (get 'dvi2-command 'file))
    (set line-sym
	 (+ (- (apply '- (get 'dvi2-command 'offset)))
	    (symbol-value line-sym)
	    -1)))))

(defun YaTeX-prev-error ()
  "Visit previous typeset error.
  To avoid making confliction of line numbers by editing, jump to
error or warning lines in reverse order."
  (interactive)
  (let ((cur-buf (buffer-name)) (cur-win (selected-window))
	error-line typeset-win error-buffer error-win)
    (if (null (get-buffer YaTeX-typeset-buffer))
	(error "There is no typesetting buffer."))
    (YaTeX-showup-buffer YaTeX-typeset-buffer nil t)
    (setq typeset-win (selected-window))
    (if (re-search-backward
	 (concat "\\(" latex-error-regexp "\\)\\|\\("
		 latex-warning-regexp "\\)")
	 nil t)
	nil
      (select-window cur-win)
      (error "No more erros on %s" cur-buf))
    (goto-char (match-beginning 0))
    (skip-chars-forward "^0-9" (match-end 0))
    (setq error-line
	  (string-to-int
	   (buffer-substring
	    (point)
	    (progn (skip-chars-forward "0-9" (match-end 0)) (point))))
	  error-buffer (YaTeX-get-error-file cur-buf))
    (if (or (null error-line) (equal 0 error-line))
	(error "Can't detect error position."))
    (YaTeX-set-virtual-error-position 'error-buffer 'error-line)
    (setq error-win (get-buffer-window error-buffer))
    (select-window cur-win)
    (cond
     (error-win (select-window error-win))
     ((eq (get-lru-window) typeset-win)
      (YaTeX-switch-to-buffer error-buffer))
     (t (select-window (get-lru-window))
	(YaTeX-switch-to-buffer error-buffer)))
    (setq error-win (selected-window))
    (goto-line error-line)
    (message "LaTeX %s in `%s' on line: %d."
	     (if (match-beginning 1) "error" "warning")
	     error-buffer error-line)
    (select-window typeset-win)
    (skip-chars-backward "0-9")
    (recenter (/ (window-height) 2))
    (sit-for 3)
    (goto-char (match-beginning 0))
    (select-window error-win))
)

(defun YaTeX-jump-error-line ()
  "Jump to corresponding line on latex command's error message."
  (interactive)
  (let (error-line error-file error-buf)
    (save-excursion
      (beginning-of-line)
      (setq error-line (re-search-forward "l[ ines]*\\.?\\([1-9][0-9]*\\)"
					  (point-end-of-line) t)))
    (if (null error-line)
	(if (eobp) (insert (this-command-keys))
	  (error "No line number expression."))
      (goto-char (match-beginning 0))
      (setq error-line (string-to-int
			(buffer-substring (match-beginning 1) (match-end 1)))
	    error-file (YaTeX-get-error-file YaTeX-current-TeX-buffer))
      (YaTeX-set-virtual-error-position 'error-file 'error-line)
      (setq error-buf (YaTeX-switch-to-buffer error-file t)))
      (if (null error-buf)
	  (error "`%s' is not found in this directory." error-file))
      (YaTeX-showup-buffer error-buf nil t)
      (goto-line error-line))
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
      (YaTeX-showup-buffer YaTeX-typeset-buffer nil t)
      ;; Next 3 lines are obsolete because YaTeX-typesetting-buffer is
      ;; automatically scrolled up at typesetting.
      ;;(goto-char (point-max))
      ;;(forward-line -1)
      ;;(recenter -1)
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
	     (progn (skip-chars-forward "^ \n" (point-end-of-line))
		    (point))))
      (if (string= "" s) default s)))
)
      
(defun YaTeX-put-nonstopmode ()
  (if YaTeX-need-nonstop
      (if (re-search-backward "\\\\nonstopmode{}" (point-min) t)
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
	(setq fname (substring (file-name-nondirectory
				(buffer-file-name))
			       0 -4))
      (setq period (rindex fname ?.))
      (setq fname (substring fname 0 (if (eq -1 period) nil period)))
      ))
)

(defun YaTeX-get-latex-command (&optional switch)
  "Specify the latex-command name and its argument.
If there is a line which begins with string: \"%#!\", the following
strings are assumed to be the latex-command and arguments.  The
default value of latex-command is:
	tex-command FileName
and if you write \"%#!jlatex\" in the beginning of certain line.
	\"jlatex \" FileName
will be the latex-command,
and you write \"%#!jlatex main.tex\" on some line and argument SWITCH
is non-nil, then
	\"jlatex main.tex\"

will be given to the shell."
  (let (magic command target)
    (setq parent
	  (cond
	   (YaTeX-parent-file YaTeX-parent-file)
	   (t (save-excursion
		(YaTeX-visit-main t)
		(file-name-nondirectory (buffer-file-name)))))
	  magic (YaTeX-get-builtin "!"))
    (cond
     (magic
      (cond
       (switch (if (string-match "\\s " magic) magic
		 (concat magic " " parent)))
       (t (concat (substring magic 0 (string-match "\\s " magic)) " "))))
     (t (concat tex-command " " (if switch parent)))))
)

(defvar YaTeX-lpr-command-history nil
  "Holds command line history of YaTeX-lpr.")
(put 'YaTeX-lpr-command-history 'no-default t)
(defun YaTeX-lpr (arg)
  "Print out.  If prefix arg ARG is non nil, call print driver without
page range description."
  (interactive "P")
  (let*((cmd (or (YaTeX-get-builtin "LPR") dviprint-command-format))
	from to (lbuffer "*dvi-printing*") dir)
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
    (setq cmd
	  (read-string-with-history
	   "Edit command line: "
	   (format cmd
		   (if (get 'dvi2-command 'region)
		       (substring YaTeX-texput-file
				  0 (rindex YaTeX-texput-file ?.))
		     (YaTeX-get-preview-file-name)))
	   'YaTeX-lpr-command-history))
    (save-excursion
      (YaTeX-visit-main t) ;;change execution directory
      (setq dir default-directory)
      (YaTeX-showup-buffer
       lbuffer (function (lambda (x) (nth 3 (window-edges x)))))
      (set-buffer (get-buffer-create lbuffer))
      (erase-buffer)
      (cd dir)				;for 19
      (cond
       (YaTeX-dos
	(call-process shell-file-name "con" "*dvi-printing*" nil
		      YaTeX-shell-command-option cmd))
       (t
	(set-process-buffer
	 (start-process "print" "*dvi-printing*" shell-file-name
			YaTeX-shell-command-option cmd)
	 (get-buffer lbuffer))
	(message "Starting printing command: %s..." cmd)))
      ))
)

(defun YaTeX-main-file-p ()
  "Return if current buffer is main LaTeX source."
  (cond
   (YaTeX-parent-file
    (eq (get-file-buffer YaTeX-parent-file) (current-buffer)))
   ((YaTeX-get-builtin "!")
    (string-match (YaTeX-guess-parent (YaTeX-get-builtin "!")) (buffer-name)))
   (t
    (save-excursion
      (let ((latex-main-id (concat "^\\s *" YaTeX-ec-regexp "documentstyle")))
	(or (re-search-backward latex-main-id nil t)
	    (re-search-forward latex-main-id nil t))))))
)

(defun YaTeX-visit-main (&optional setbuf)
  "Switch buffer to main LaTeX source.
Use set-buffer instead of switch-to-buffer if the optional second argument
SETBUF is t(Use it only from Emacs-Lisp program)."
  (interactive "P")
  (if (and (interactive-p) setbuf) (setq YaTeX-parent-file nil))
  (let (b-in main-file)
    (if (setq b-in (YaTeX-get-builtin "!"))
	(setq main-file (YaTeX-guess-parent b-in)))
    (if YaTeX-parent-file
	(setq main-file ;;(get-file-buffer YaTeX-parent-file)
	      YaTeX-parent-file))
    (if (YaTeX-main-file-p)
	(if (interactive-p) (message "I think this is main LaTeX source.") nil)
      (cond
       ((and (interactive-p) main-file (get-file-buffer main-file))
	(goto-buffer-window main-file))
       ((and main-file (YaTeX-switch-to-buffer main-file setbuf)))
       ((and main-file
	     (file-exists-p (setq main-file (concat "../" main-file)))
	     (y-or-n-p (concat (expand-file-name main-file)
			       " is main file?:")))
	(setq YaTeX-parent-file main-file)
	(YaTeX-switch-to-buffer main-file setbuf))
       (t (setq main-file (read-file-name "Enter your main text: " nil nil 1))
	  (setq YaTeX-parent-file main-file)
	  (YaTeX-switch-to-buffer main-file setbuf))
       )))
  nil
)


(defun YaTeX-guess-parent (command-line)
  (setq command-line
	(if (string-match ".*\\s " command-line)
	    (substring command-line (match-end 0))
	  (file-name-nondirectory (buffer-file-name)))
	command-line
	(concat (if (string-match "\\(.*\\)\\." command-line)
		    (substring command-line (match-beginning 1) (match-end 1))
		  command-line)
		".tex"))
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
    (if (and (re-search-forward
	      (concat "^" (regexp-quote (concat "%#" key))) nil t)
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

(provide 'yatexprc)

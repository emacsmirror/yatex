;;; -*- Emacs-Lisp -*-
;;; YaTeX helper with LaTeX commands and macros.
;;; yatexhlp.el
;;; (c )1994 by HIROSE Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Thu May  5 16:09:42 1994 on 98fa
;;; $Id$

(let ((help-file
       (concat "YATEXHLP."
	       (cond (YaTeX-japan "jp")
		     (t "eng")))))
  (defvar YaTeX-help-file
    (expand-file-name help-file exec-directory)
    "*Help file of LaTeX/TeX commands or macros.")
  (defvar YaTeX-help-file-private
    (expand-file-name (concat "~/" help-file))
    "*Private help file of LaTeX/TeX macros.")
)
(defvar YaTeX-help-delimiter "\C-_" "Delimiter of each help entry.")
(defvar YaTeX-help-entry-map (copy-keymap YaTeX-mode-map)
  "Key map used in help entry.")
(defvar YaTeX-help-file-current nil
  "Holds help file name to which the description in current buffer should go.")
(defvar YaTeX-help-command-current nil
  "Holds command name on which the user currently write description.")
(defvar YaTeX-help-saved-config nil
  "Holds window configruation before the editing of manual.")
(defvar YaTeX-help-synopsis
  (cond (YaTeX-japan "ÅyèëéÆÅz")
	(t "[[ Synopsis ]]"))
  "Section header of synopsis.")
(defvar YaTeX-help-description
  (cond (YaTeX-japan "Åyê‡ñæÅz")
	(t "[[ Description ]]"))
  "Section header of description.")

(defun YaTeX-refer-help (command help-file)
  "Refer the COMMAND's help into HELP-FILE.
\[Help-file format\]
<DELIM><LaTeX/TeX command without escape character(\\)><NL>
<Synopsis><NL>
<Documentation><TERM>
Where:	<DELIM> is the value of YaTeX-help-delimiter.
	<NL> is newline.
	<TERM> is newline or end of buffer."
  (let ((hfbuf (find-file-noselect help-file))
	(hbuf (get-buffer-create "** YaTeX HELP **"))
	(curwin (selected-window))
	sb se db de)
    (set-buffer hfbuf)
    (goto-char (point-min))
    (if (null
	 (re-search-forward
	  (concat (regexp-quote YaTeX-help-delimiter)
		  (regexp-quote command)
		  "$") nil t))
	nil				;if not found, return nil
      (forward-line 1)
      (setq sb (point)
	    se (progn (forward-line 1) (point))
	    db (point)
	    de (progn
		 (re-search-forward (regexp-quote YaTeX-help-delimiter) nil 1)
		 (1- (match-beginning 0))))
      (YaTeX-showup-buffer hbuf)
      (pop-to-buffer hbuf)
      (erase-buffer)
      (insert YaTeX-help-synopsis "\n")
      (insert-buffer-substring hfbuf sb se)
      (insert "\n" YaTeX-help-description "\n")
      (insert-buffer-substring hfbuf db de)
      (goto-char (point-min))
      (select-window curwin)
      t))
)
(defun YaTeX-help-newline (&optional arg)
  (interactive "P")
  (if (and (= (current-column) 1) (= (preceding-char) ?.) (eolp))
      (let ((cbuf (current-buffer)))
	(beginning-of-line)
	(kill-line 1)
	(save-excursion
	  (YaTeX-help-add-entry
	   YaTeX-help-command-current YaTeX-help-file-current))
	(set-window-configuration YaTeX-help-saved-config)
	(bury-buffer cbuf))
    (newline arg))
)
(defun YaTeX-help-add-entry (command help-file)
  (let ((hfbuf (find-file-noselect help-file))
	(dbuf (current-buffer)) beg end)
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote YaTeX-help-synopsis)))
    (forward-line 1)	(setq beg (point))
    (end-of-line)	(setq end (point))
    (set-buffer hfbuf)
    (goto-char (point-min))
    (insert YaTeX-help-delimiter command "\n")
    (insert-buffer-substring dbuf beg end)
    (insert "\n")
    (set-buffer dbuf)
    (re-search-forward (concat "^" (regexp-quote YaTeX-help-description)))
    (forward-line 1)
    (setq beg (point))
    (setq end (point-max))
    (set-buffer hfbuf)
    (insert-buffer-substring dbuf beg end)
    (insert "\n\n")
    (forward-line -1)
    (delete-blank-lines)
    (let ((make-backup-files t))
      (basic-save-buffer))
    (bury-buffer hfbuf))
)
(defun YaTeX-help-prepare-entry (command help-file)
  "Read help description on COMMAND and add it to HELP-FILE."
  (let ((buf (get-buffer-create "**Description**"))
	(conf (current-window-configuration)))
    (YaTeX-showup-buffer buf)
    (pop-to-buffer buf)
    (make-local-variable 'YaTeX-help-file-current)
    (make-local-variable 'YaTeX-help-command-current)
    (make-local-variable 'YaTeX-help-saved-config)
    (setq YaTeX-help-file-current help-file
	  YaTeX-help-command-current command
	  YaTeX-help-saved-config conf
	  mode-name "Text"
	  major-mode 'text)
    (erase-buffer)
    (insert YaTeX-help-synopsis "\n\n" YaTeX-help-description "\n\n")
    (define-key YaTeX-help-entry-map "\r" 'YaTeX-help-newline)
    (use-local-map YaTeX-help-entry-map)
    (message
     (cond (YaTeX-japan "ì¸óÕÇèIÇ¶ÇΩÇÁ . ÇÃÇ›ì¸óÕÇµÇƒRET")
	   (t "Type only `.' and RET to exit."))))
)
(defun YaTeX-enrich-help (command)
  "Add the COMMAND's help to help file."
  (if (y-or-n-p (format "No help on `%s'. Create help?" command))
      (YaTeX-help-prepare-entry
       command
       (if (y-or-n-p "Add help to global documentation?")
	   YaTeX-help-file YaTeX-help-file-private)))
)

(defun YaTeX-help-sort (&optional help-file)
  "Sort help file HELP-FILE.
If HELP-FILE is nil or called interactively, sort current buffer
as a help file."
  (interactive)
  (if help-file (set-buffer (find-file-noselect help-file)))
  (sort-regexp-fields
   nil "\\(\\sw+\\)\\([^]+\\|\\s'\\)" "\\1" (point-min) (point-max))
)

;;;###autoload
(defun YaTeX-help ()
  "Show help buffer of LaTeX/TeX commands or macros."
  (interactive)
  (let (p beg end command)
    (save-excursion
      (if (looking-at YaTeX-ec-regexp)
	  (goto-char (match-end 0)))
      (setq p (point))			;remember current position.
      (cond
       ((YaTeX-on-begin-end-p)
	;;if on \begin or \end, extract its environment.
	(setq command
	      (cond ((match-beginning 1)
		     (buffer-substring (match-beginning 1) (match-end 1)))
		    ((match-beginning 2)
		     (buffer-substring (match-beginning 2) (match-end 2))))))
       ((search-backward YaTeX-ec (point-beginning-of-line) t)
	(goto-char (setq beg (match-end 0)))
	(re-search-forward "\\sw+")
	(setq end (point))
	(if (and (<= beg p) (<= p end))
	    (setq command (buffer-substring beg end)))))
      (if (or (string= command "begin") (string= command "end"))
	  (progn
	    (search-forward "{" (point-end-of-line))
	    (setq beg (point))
	    (search-forward "}" (point-end-of-line))
	    (setq command (buffer-substring beg (match-beginning 0)))))
      (setq command
	    (completing-read
	     "Describe (La)TeX command: "
	     (append
	      section-table user-section-table tmp-section-table
	      article-table user-article-table
	      env-table     user-env-table     tmp-env-table
	      singlecmd-table user-singlecmd-table tmp-singlecmd-table)
	     nil nil command))	;no-predicate, not require match
      );end excursion
    (or (YaTeX-refer-help command YaTeX-help-file)
	(YaTeX-refer-help command YaTeX-help-file-private)
	(YaTeX-enrich-help command)))
)

;;; -*- Emacs-Lisp -*-
;;; YaTeX sectioning browser.
;;; yatexsec.el
;;; (c ) 1994 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Fri Nov 25 04:46:42 1994 on VFR
;;; $Id$

(defvar YaTeX-sectioning-level
  '(("part" . 0)
    ("chapter" . 1)
    ("section" . 2)
    ("subsection" . 3)
    ("subsubsection" . 4)
    ("paragraph" . 5)
    ("subparagraph" . 6))
  "*Alist of LaTeX's sectioning command and its level.
This value must be written in numerically ascending order and consecutive.
Needn't define the level of `*' commands such as `section*'.")

(defvar YaTeX-sectioning-max-level
  (cdr (nth (1- (length YaTeX-sectioning-level)) YaTeX-sectioning-level))
  "*The heighest(numerically) level of sectioning command.
This must be the heighest number in YaTeX-sectioning-level.")

(defun YaTeX-sectioning-map-hide (map)
  (let ((ch ?0))
    (while (<= ch ?9)
      (define-key map (char-to-string ch) 'YaTeX-sectioning-hide)
      (setq ch (1+ ch))))
)

(defvar YaTeX-sectioning-minibuffer-map nil
  "Key map used in minibuffer for sectioning.")
(if YaTeX-sectioning-minibuffer-map nil
  (setq YaTeX-sectioning-minibuffer-map
	(copy-keymap minibuffer-local-completion-map))
  (define-key YaTeX-sectioning-minibuffer-map "\C-p"
    'YaTeX-sectioning-up)
  (define-key YaTeX-sectioning-minibuffer-map "\C-e"
    'YaTeX-sectioning-up)
  (define-key YaTeX-sectioning-minibuffer-map "\C-i"
    'YaTeX-minibuffer-complete)
  (define-key YaTeX-sectioning-minibuffer-map " "
    'YaTeX-minibuffer-complete)
  (define-key YaTeX-sectioning-minibuffer-map "\C-n"
    'YaTeX-sectioning-down)
  (define-key YaTeX-sectioning-minibuffer-map "\C-x"
    'YaTeX-sectioning-down)
  (define-key YaTeX-sectioning-minibuffer-map "\C-v"
    'YaTeX-sectioning-scroll-up)
  (define-key YaTeX-sectioning-minibuffer-map "\C-c"
    'YaTeX-sectioning-scroll-up)
  (define-key YaTeX-sectioning-minibuffer-map "\M-v"
    'YaTeX-sectioning-scroll-down)
  (define-key YaTeX-sectioning-minibuffer-map "\C-r"
    'YaTeX-sectioning-scroll-down)
  (define-key YaTeX-sectioning-minibuffer-map "\C-w"
    '(lambda () (interactive) (YaTeX-sectioning-scroll-down 1)))
  (define-key YaTeX-sectioning-minibuffer-map "\C-z"
    '(lambda () (interactive) (YaTeX-sectioning-scroll-up 1)))
  (define-key YaTeX-sectioning-minibuffer-map "\C-l"
    'YaTeX-sectioning-recenter)
  (define-key YaTeX-sectioning-minibuffer-map "?"
    'YaTeX-sectioning-help)
  (YaTeX-sectioning-map-hide YaTeX-sectioning-minibuffer-map)
)

(defvar YaTeX-sectioning-buffer-map nil
  "Key map used in YaTeX-sectioning-buffer.")
(if YaTeX-sectioning-buffer-map nil
  (setq YaTeX-sectioning-buffer-map (make-sparse-keymap))
  (define-key YaTeX-sectioning-buffer-map " "
    'YaTeX-sectioning-buffer-jump)
  (define-key YaTeX-sectioning-buffer-map (concat YaTeX-prefix "\C-c")
    'YaTeX-sectioning-buffer-jump)
  (YaTeX-sectioning-map-hide YaTeX-sectioning-buffer-map)
)

(defvar YaTeX-sectioning-buffer-parent nil)
(defun YaTeX-sectioning-buffer-jump ()
  (interactive)
  (if (and YaTeX-sectioning-buffer-parent
	   (get-buffer YaTeX-sectioning-buffer-parent))
      (let (ptn)
	(beginning-of-line)
	(if (re-search-forward YaTeX-sectioning-regexp)
	    (progn (setq ptn (buffer-substring
			      (1- (match-beginning 0))
			      (progn (skip-chars-forward "^}") (1+ (point)))))
		   (YaTeX-showup-buffer YaTeX-sectioning-buffer-parent nil t)
		   (goto-char (point-max))
		   (search-backward ptn))
	  (message "No line number expression."))))
)

(defun YaTeX-sectioning-hide-under (n)
  "Hide sectioning commands under level N."
  (let ((cw (selected-window)))
    (YaTeX-showup-buffer YaTeX-sectioning-buffer nil t)
    (if (>= n YaTeX-sectioning-max-level)
	(progn
	  (set-selective-display nil)
	  (message "Show all."))
      (set-selective-display (1+ n))
      (if (rassq n YaTeX-sectioning-level)
	  (message "Hide lower than %s" (car (rassq n YaTeX-sectioning-level)))
	(message "")))
    (if (numberp selective-display)
	(setq mode-name (format "level %d" (1- selective-display)))
      (setq mode-name (format "all")))
    (select-window cw))
)
(defun YaTeX-sectioning-hide ()
  "Call YaTeX-sectioning-hide-under with argument according to pressed key."
  (interactive)
  (YaTeX-sectioning-hide-under (- last-command-char ?0)))

(defun YaTeX-sectioning-help ()
  "Show help of sectioning."
  (interactive)
  (let ((cw (selected-window)) sb (hb (get-buffer-create "*Help*")))
    (unwind-protect
	(progn
	  (other-window 1)
	  (setq sb (current-buffer))
	  (switch-to-buffer hb)
	  (erase-buffer)
	  (insert "===== View sectioning =====
C-p	Up sectioning level.			0	Show only \\part, 
C-n	Down sectioning level.			1	 and \\chapter,
C-v	Scroll up *Sectioning line* buffer.	2	 and \\section,
M-v	Scroll down *Sectioning line* buffer.	3	 and \\subsection,
C-z	Scroll up by 1 line.			4	 and \\subsubsection,
C-w	Scroll down by 1 line.			5	 and \\paragraph.
SPC	Complete word.				6	Show all.
TAB	Complete word.
C-l	Recenter recent line.
RET	Select.
==== End of HELP =====
")
	  (set-buffer-modified-p nil)
	  (goto-char (point-min))
	  (momentary-string-display "" (point-min)))
      (bury-buffer hb)
      (switch-to-buffer sb)
      (select-window cw)))
)

(defun YaTeX-sectioning-up (n)
  "Up section level.
Refers the YaTeX-read-section-in-minibuffer's local variable minibuffer-start."
  (interactive "p")
  (if (eq (selected-window) (minibuffer-window))
      (let*((command (buffer-string))
	    (aster (equal (substring command -1) "*"))
	    (command (if aster (substring command 0 -1) command))
	    (alist YaTeX-sectioning-level)
	    (level 0))
	(or (assoc command alist) (error "No such sectioning command."))
	(while (not (string= (car (nth level alist)) command))
	  (setq level (1+ level)))	;I want to use `member'....
	(setq level (- level n))
	(if (or (< level 0) (>= level (length alist)))
	    (ding)
	  (erase-buffer)
	  (insert (concat (car (nth level alist)) (if aster "*" ""))))))
)

(defun YaTeX-sectioning-down (n)
  "Down section level."
  (interactive "p")
  (YaTeX-sectioning-up (- n))
)

(defun YaTeX-sectioning-scroll-up (n)
  (interactive "P")
  (let ((section-buffer YaTeX-sectioning-buffer)
	(cw (selected-window)))
    (YaTeX-showup-buffer section-buffer nil t)
    (unwind-protect
	(scroll-up (or n (- (window-height) 2)))
      (select-window cw)))
)

(defun YaTeX-sectioning-scroll-down (n)
  (interactive "P")
  (let ((section-buffer YaTeX-sectioning-buffer)
	(cw (selected-window)))
    (YaTeX-showup-buffer section-buffer nil t)
    (unwind-protect
	(scroll-down (or n (- (window-height) 2)))
      (select-window cw)))
)

(defun YaTeX-sectioning-recenter (arg)
  "Recenter `<<--' line"
  (interactive "P")
  (let ((cw (selected-window)))
    (unwind-protect
	(progn
	  (YaTeX-showup-buffer YaTeX-sectioning-buffer nil t)
	  (or (search-forward "<<--" nil t)
	      (search-backward "<<--" nil))
	  (recenter (or arg (/ (window-height) 2))))
      (select-window cw)))
)

(defvar YaTeX-sectioning-minibuffer " *sectioning*"
  "Miniuffer used for sectioning")
;;;###autoload
(defun YaTeX-read-section-in-minibuffer (prompt table &optional default delim)
  (interactive)
  (let ((minibuffer-completion-table table))
    (read-from-minibuffer
     prompt default YaTeX-sectioning-minibuffer-map))
)

(defun YaTeX-get-sectioning-level ()
  "Get section-level on the cursor."
   (cdr-safe (assoc (buffer-substring
		     (point)
		     (progn (skip-chars-forward "a-z") (point)))
		     YaTeX-sectioning-level))
)

(defvar YaTeX-sectioning-buffer "*Sectioning lines*")
(defun YaTeX-colloect-sections ()
  "Collect all the lines which contains sectioning command."
  (let ((cw (selected-window)) level indent begp (prevp 1) (prevl 1)
	(pattern (concat YaTeX-ec-regexp
			 "\\(" YaTeX-sectioning-regexp "\\)\\*?{"))
	(cb (current-buffer)))
    (save-excursion
      (YaTeX-showup-buffer YaTeX-sectioning-buffer) ;show buffer
      (goto-char (point-min))
      (with-output-to-temp-buffer YaTeX-sectioning-buffer
	(while (re-search-forward pattern nil t)
	  (goto-char (1+ (match-beginning 0)))
	  (setq level (YaTeX-get-sectioning-level)
		begp (match-beginning 0))
	  ;;(beginning-of-line)
	  ;;(skip-chars-forward " \t")
	  (setq indent (format "%%%ds" level))
	  (princ (format indent ""))
	  (if (YaTeX-on-comment-p) (princ "%"))
	  (princ (buffer-substring begp (progn (forward-list 1) (point))))
	  (setq prevl (+ prevl (count-lines prevp (point)) -1)
		prevp (point))
	  (princ (format " (line:%d)" prevl))
	  (princ "\n")))
      (set-buffer YaTeX-sectioning-buffer)
      (make-local-variable 'YaTeX-sectioning-buffer-parent)
      (use-local-map YaTeX-sectioning-buffer-map)
      (setq YaTeX-sectioning-buffer-parent cb)
      (if (numberp selective-display)
	  (setq mode-name (format "level %d" (1- selective-display))))
      YaTeX-sectioning-buffer))
)

(defun YaTeX-section-overview ()
  "Show section overview.  Return the nearest sectioning command."
  (interactive)
  (let ((cw (selected-window)) (ln (count-lines (point-min) (point)))
	(pattern "(line:\\([0-9]+\\))")
	(secbuf YaTeX-sectioning-buffer) (command ""))
    (save-excursion
      (setq secbuf (YaTeX-colloect-sections))
      (YaTeX-showup-buffer secbuf nil t)
      (goto-char (point-max))
      (while (re-search-backward pattern nil t)
	(if (< ln (string-to-int (YaTeX-match-string 1))) nil
	  (beginning-of-line)
	  (search-forward YaTeX-ec)
	  (looking-at YaTeX-TeX-token-regexp)
	  (setq command (YaTeX-match-string 0))
	  (end-of-line)
	  (insert "  <<--")
	  (setq pattern (concat "HackyRegexp" "ForFailure"))))
      (set-buffer-modified-p nil)
      (forward-line 1)
      (if (eobp) (recenter -1) (recenter -3))
      (select-window cw)
      command))
)

;;;###autoload
(defun YaTeX-make-section-with-overview ()
  "Input sectining command with previous overview."
  (interactive)
  (insert
   YaTeX-ec
   (YaTeX-read-section-in-minibuffer
    "Sectioning(Up=C-p, Down=C-n, Help=?): "
    YaTeX-sectioning-level (YaTeX-section-overview))
   "{}")
  (forward-char -1)
)

(provide 'yatexsec)

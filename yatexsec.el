;;; -*- Emacs-Lisp -*-
;;; YaTeX sectioning browser.
;;; yatexsec.el
;;; (c ) 1994 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Thu Jul  7 01:26:26 1994 on 98fa
;;; $Id$

(defvar YaTeX-sectioning-level
  '(("part" . 0) ("chapter" . 1) ("section" . 2) ("subsection" . 3)
    ("subsubsection" . 4) ("paragraph" . 5) ("subparagraph" . 6))
  "Sectioning level.")

(defun YaTeX-sectioning-map-hide (map)
  (let ((ch ?0))
    (while (<= ch ?9)
      (define-key map (char-to-string ch) 'YaTeX-sectioning-hide)
      (setq ch (1+ ch))))
)

(defvar YaTeX-minibuffer-sectioning-map nil
  "Key map used in minibuffer for sectioning.")
(if YaTeX-minibuffer-sectioning-map nil
  (setq YaTeX-minibuffer-sectioning-map
	(copy-keymap minibuffer-local-completion-map))
  (define-key YaTeX-minibuffer-sectioning-map "\C-p"
    'YaTeX-sectioning-up)
  (define-key YaTeX-minibuffer-sectioning-map "\C-e"
    'YaTeX-sectioning-up)
  (define-key YaTeX-minibuffer-sectioning-map "\C-i"
    'YaTeX-minibuffer-complete)
  (define-key YaTeX-minibuffer-sectioning-map " "
    'YaTeX-minibuffer-complete)
  (define-key YaTeX-minibuffer-sectioning-map "\C-n"
    'YaTeX-sectioning-down)
  (define-key YaTeX-minibuffer-sectioning-map "\C-x"
    'YaTeX-sectioning-down)
  (define-key YaTeX-minibuffer-sectioning-map "\C-v"
    'YaTeX-sectioning-scroll-up)
  (define-key YaTeX-minibuffer-sectioning-map "\C-c"
    'YaTeX-sectioning-scroll-up)
  (define-key YaTeX-minibuffer-sectioning-map "\M-v"
    'YaTeX-sectioning-scroll-down)
  (define-key YaTeX-minibuffer-sectioning-map "\C-r"
    'YaTeX-sectioning-scroll-down)
  (define-key YaTeX-minibuffer-sectioning-map "\C-w"
    '(lambda () (interactive) (YaTeX-sectioning-scroll-down 1)))
  (define-key YaTeX-minibuffer-sectioning-map "\C-z"
    '(lambda () (interactive) (YaTeX-sectioning-scroll-up 1)))
  (define-key YaTeX-minibuffer-sectioning-map "\C-l"
    'YaTeX-sectioning-recenter)
  (define-key YaTeX-minibuffer-sectioning-map "?"
    'YaTeX-sectioning-help)
  (YaTeX-sectioning-map-hide YaTeX-minibuffer-sectioning-map)
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
    (if (>= n (1- (length YaTeX-sectioning-level)))
	(progn
	  (set-selective-display nil)
	  (message "Show all."))
      (set-selective-display (1+ n))
      (if (nth n YaTeX-sectioning-level)
	  (message "Hide lower than %s" (car (nth n YaTeX-sectioning-level)))
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
SPC	Complete word.				4	 and \\subsubsection,
TAB	Complete word.				5	 and \\paragraph.
C-l	Recenter recent line.			6	Show all.
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
	    (alist YaTeX-sectioning-level)
	    (level (cdr-safe (assoc command alist))))
	(or level (error "No such sectioning command."))
	(setq level (- level n))
	(if (or (< level 0) (>= level (length alist)))
	    (ding)
	  (erase-buffer)
	  (insert (car (nth level alist))))
    ))
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
	      (search-backward "<<--" nil t))
	  (recenter (or arg (/ (window-height) 2))))
      (select-window cw)))
)

(defvar YaTeX-sectioning-minibuffer " *sectioning*"
  "Miniuffer used for sectioning")
(defun YaTeX-read-section-in-minibuffer (prompt table &optional default delim)
  (interactive)
  (let ((minibuffer-completion-table table))
    (read-from-minibuffer
     prompt default YaTeX-minibuffer-sectioning-map))
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
;;(YaTeX-define-key "o" 'YaTeX-make-section-with-overview)

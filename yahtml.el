;;; -*- Emacs-Lisp -*-
;;; (c ) 1994-1997 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Mon Apr  7 16:58:32 1997 on crx
;;; $Id$

;;;[Installation]
;;; 
;;; First, you have to install YaTeX and make sure it works fine.  Then
;;; put these expressions into your ~/.emacs
;;; 
;;; 	(setq auto-mode-alist
;;; 		(cons (cons "\\.html$" 'yahtml-mode) auto-mode-alist))
;;; 	(autoload 'yahtml-mode "yahtml" "Yet Another HTML mode" t)
;;; 	(setq yahtml-www-browser "netscape")
;;;      ;Write your favorite browser.  But netscape is advantageous.
;;; 	(setq yahtml-path-url-alist
;;; 	      '(("/home/yuuji/public_html" . "http://www.mynet/~yuuji")
;;; 		("/home/staff/yuuji/html" . "http://www.othernet/~yuuji")))
;;;      ;Write correspondence alist from ABSOLUTE unix path name to URL path.
;;; 
;;;[Commentary]
;;;
;;; It is assumed you are already familiar with YaTeX.  The following
;;; completing featureas are available: ([prefix] means `C-c' by default)
;;;
;;;  * [prefix] b X	Complete environments such as `H1' which
;;;			normally requires closing tag `</H1>
;;;			<a href=foo> ... </a> is also classified into
;;;			this group
;;;			When input `href=...', you can complete file
;;;			name or label(href="#foo") by typing TAB.
;;;  * [prefix] s	Complete declarative notations such as
;;;			`<img src="foo.gif">'
;;;			`<input name="var" ...>'
;;;  * [prefix] l	Complete typeface-changing commands such as
;;;			`<i> ... </i>' or `<samp> ... </samp>'
;;; 			This completion can be used to make in-line
;;; 			tags which is normally completed with [prefix] b.
;;;  * [prefix] m	Complete single commands such as
;;;			`<br>' or `<hr> or <li>...'
;;;  * M-RET		Intelligent newline; if current TAG is one of
;;;			ul, ol, or  dl. insert newline and <li> or
;;;			<dt> or <dd> suitable for current condition.
;;;  * menu-bar yahtml	Complete all by selecting a menu item (Though I
;;;			hate menu, this is most useful)
;;;  * [prefix] g	Goto corresponding Tag or HREF such as
;;; 			<dl> <-> </dl>  or href="xxx".
;;;			Or invoke image viewer if point is on <img src=...>.
;;;  * [prefix] k	Kill html tags on the point.  If you provide
;;; 			universal-argument, kill surrounded contents too.
;;;  * [prefix] c	Change html tags on the point.
;;;			When typeing [prefix] c on `href="xxx"', you can 
;;;			change the reference link with completion.
;;;  * [prefix] t j	Call weblint on current file.
;;;  * [prefix] t p	View current html with WWW browser
;;; 			(To activate this, never fail to set the lisp
;;; 			 variable yahtml-www-browser.  Recommended value
;;; 			 is "netscape")
;;;  * [prefix] a	YaTeX's accent mark's equivalent of yahtml.
;;;			This function can input $lt, $gt or so.
;;; 


(require 'yatex)
;;; --- customizable variable starts here ---
(defvar yahtml-image-viewer "xv" "*Image viewer program")
(defvar yahtml-www-browser "netscape"
  "*WWW Browser command")
(defvar yahtml-kanji-code 2
  "Kanji coding system of html file; 1=sjis, 2=jis, 3=euc")
;;(defvar yahtml-www-server "www" "*Host name of your domain's WWW server")
(defvar yahtml-path-url-alist nil
  "*Alist of unix path name vs. URL name of WWW server.
Ex.
'((\"/usr/home/yuuji/http\" . \"http://www.comp.ae.keio.ac.jp/~yuuji\")
  (\"/usr/home/yuuji/darts/http\" . \"http://inspire.comp.ae.keio.ac.jp/~darts\"))")
(defvar yahtml-directory-index "index.html"
  "*Directory index file name;
Consult your site's WWW administrator.")

(defvar yahtml-environment-indent YaTeX-environment-indent
  "*Indentation depth of HTML's listing environment")

(defvar yahtml-lint-program (if YaTeX-japan "jweblint" "weblint")
  "*Program name to lint HTML file")
(defvar yahtml-hate-too-deep-indentation nil
  "*Non-nil for this variable suppress deep indentation in listing environments.")

;;; --- customizable variable ends here ---

(defvar yahtml-prefix-map nil)
(defvar yahtml-mode-map nil "Keymap used in yahtml-mode.")
(defvar yahtml-lint-buffer-map nil "Keymap used in lint buffer.")
(defvar yahtml-shell-command-option
  (or (and (boundp 'shell-command-option) shell-command-option)
      (if (eq system-type 'ms-dos) "/c" "-c")))


(defun yahtml-define-begend-key-normal (key env &optional map)
  "Define short cut yahtml-insert-begend key."
  (YaTeX-define-key
   key
   (list 'lambda '(arg) '(interactive "P")
	 (list 'yahtml-insert-begend 'arg env))
   map))

(defun yahtml-define-begend-region-key (key env &optional map)
  "Define short cut yahtml-insert-begend-region key."
  (YaTeX-define-key key (list 'lambda nil '(interactive)
			      (list 'yahtml-insert-begend t env)) map))

(defun yahtml-define-begend-key (key env &optional map)
  "Define short cut key for begin type completion both for
normal and region mode.  To customize yahtml, user should use this function."
  (yahtml-define-begend-key-normal key env map)
  (if YaTeX-inhibit-prefix-letter nil
    (yahtml-define-begend-region-key
     (concat (upcase (substring key 0 1)) (substring key 1)) env map)))


(if yahtml-mode-map nil
  (setq yahtml-mode-map (make-sparse-keymap)
	yahtml-prefix-map (make-sparse-keymap))
  (define-key yahtml-mode-map YaTeX-prefix yahtml-prefix-map)
  (define-key yahtml-mode-map "\M-\C-@" 'yahtml-mark-begend)
  (if (and (boundp 'window-system) (eq window-system 'x) YaTeX-emacs-19)
      (define-key yahtml-mode-map [?\M-\C- ] 'yahtml-mark-begend))
  (define-key yahtml-mode-map "\M-\C-a" 'YaTeX-beginning-of-environment)
  (define-key yahtml-mode-map "\M-\C-e" 'YaTeX-end-of-environment)
  (define-key yahtml-mode-map "\M-\C-m" 'yahtml-intelligent-newline)
  (define-key yahtml-mode-map "\C-i" 'yahtml-indent-line)
  (define-key yahtml-mode-map YaTeX-prefix yahtml-prefix-map)
  (let ((map yahtml-prefix-map))
    (YaTeX-define-key "^" 'yahtml-visit-main map)
    (YaTeX-define-key "4^" 'yahtml-visit-main-other-window map)
    (YaTeX-define-key "4g" 'yahtml-goto-corresponding-*-other-window map)
    (YaTeX-define-key "44" 'YaTeX-switch-to-window map)
    (and YaTeX-emacs-19 window-system
	 (progn
	   (YaTeX-define-key "5^" 'yahtml-visit-main-other-frame map)
	   (YaTeX-define-key "5g" 'yahtml-goto-corresponding-*-other-frame map)
	   (YaTeX-define-key "55" 'YaTeX-switch-to-window map)))
    (YaTeX-define-key "v" 'YaTeX-version map)
    (YaTeX-define-key "}" 'YaTeX-insert-braces-region map)
    (YaTeX-define-key "]" 'YaTeX-insert-brackets-region map)
    (YaTeX-define-key ")" 'YaTeX-insert-parens-region map)
    (YaTeX-define-key "s" 'yahtml-insert-form map)
    (YaTeX-define-key "l" 'yahtml-insert-tag map)
    (YaTeX-define-key "L" 'yahtml-insert-tag-region map)
    (YaTeX-define-key "m" 'yahtml-insert-single map)
    (YaTeX-define-key "n" '(lambda () (interactive) (insert (if yahtml-prefer-upcases "<BR>" "<br>"))) map)
    (YaTeX-define-key "-" '(lambda () (interactive) (insert (if yahtml-prefer-upcases "<HR>" "<hr>") "\n")) map)
    (if YaTeX-no-begend-shortcut
	(progn
	  (YaTeX-define-key "B" 'yahtml-insert-begend-region map)
	  (YaTeX-define-key "b" 'yahtml-insert-begend map))
      (yahtml-define-begend-key "bh" "html" map)
      (yahtml-define-begend-key "bH" "head" map)
      (yahtml-define-begend-key "bt" "title" map)
      (yahtml-define-begend-key "bT" "table" map)
      (yahtml-define-begend-key "bb" "body" map)
      (yahtml-define-begend-key "bc" "center" map)
      (yahtml-define-begend-key "bd" "dl" map)
      (yahtml-define-begend-key "bu" "ul" map)
      (yahtml-define-begend-key "b1" "h1" map)
      (yahtml-define-begend-key "b2" "h2" map)
      (yahtml-define-begend-key "b3" "h3" map)
      (yahtml-define-begend-key "ba" "a" map)
      (yahtml-define-begend-key "bf" "form" map)
      (yahtml-define-begend-key "bs" "select" map)
      (YaTeX-define-key "b " 'yahtml-insert-begend map)
      (YaTeX-define-key "B " 'yahtml-insert-begend-region map)
      )
    (YaTeX-define-key "e" 'YaTeX-end-environment map)
    (YaTeX-define-key ">" 'yahtml-comment-region map)
    (YaTeX-define-key "<" 'yahtml-uncomment-region map)
    (YaTeX-define-key "g" 'yahtml-goto-corresponding-* map)
    (YaTeX-define-key "k" 'yahtml-kill-* map)
    (YaTeX-define-key "c" 'yahtml-change-* map)
    (YaTeX-define-key "t" 'yahtml-browse-menu map)
    (YaTeX-define-key "a" 'yahtml-complete-mark map)
    (YaTeX-define-key "'" 'yahtml-prev-error map)
    ;;;;;(YaTeX-define-key "i" 'yahtml-fill-item map)
    ))

(if yahtml-lint-buffer-map nil
  (setq yahtml-lint-buffer-map (make-keymap))
  (define-key yahtml-lint-buffer-map " " 'yahtml-jump-to-error-line))


(defvar yahtml-paragraph-start
  (concat
   "^$\\|<!--\\|^[ \t]*</?\\(h[1-6]\\|p\\|d[ldt]\\|[bhtd][rdh]\\|li\\|body\\|html\\|head\\|title\\|ul\\|ol\\|dl\\|pre\\|table\\|center\\|blockquote\\)\\b")
  "*Regexp of html paragraph separater")
(defvar yahtml-paragraph-separate
  (concat
   "^$\\|<!--\\|^[ \t]*</?\\(h[1-6]\\|p\\|[bhtd][ldt]\\|li\\|body\\|html\\|head\\|title\\|ul\\|ol\\|dl\\|pre\\|table\\|center\\|blockquote\\|!--\\)\\b")
  "*Regexp of html paragraph separater")
(defvar yahtml-syntax-table nil
  "*Syntax table for typesetting buffer")

(if yahtml-syntax-table nil
  (setq yahtml-syntax-table
	(make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?\< "(>" yahtml-syntax-table)
  (modify-syntax-entry ?\> ")<" yahtml-syntax-table)
  (modify-syntax-entry ?\n " " yahtml-syntax-table)
)
(defvar yahtml-command-regexp "[A-Za-z0-9]+"
  "Regexp of constituent of html commands.")

;;; Completion tables for `form'
(defvar yahtml-form-table
  '(("img") ("input")))
(defvar yahtml-user-form-table nil)
(defvar yahtml-tmp-form-table nil)

(defvar yahtml-env-table
  '(("html") ("head") ("title") ("body") ("dl") ("ul") ("ol") ("pre")
    ("a") ("form") ("select") ("center") ("textarea") ("blockquote")
    ("OrderedList" . "ol")
    ("UnorderedList" . "ul")
    ("DefinitionList" . "dl")
    ("Preformatted" . "pre")
    ("table") ("tr") ("th") ("td")
    ("h1") ("h2") ("h3") ("h4") ("h5") ("h6")
    ("p")))

(defvar yahtml-itemizing-regexp
  "\\(ul\\|ul\\|dl\\)"
  "Regexp of itemizing forms")

(defvar yahtml-user-env-table nil)
(defvar yahtml-tmp-env-table nil)

;;; Completion tables for typeface designator
(defvar yahtml-typeface-table
  (append
   '(("dfn") ("em") ("cite") ("code") ("kbd") ("samp")
     ("strong") ("var") ("b") ("i") ("tt") ("u") ("address"))
   yahtml-env-table)
  "Default completion table of typeface designator")
(defvar yahtml-user-typeface-table nil)
(defvar yahtml-tmp-typeface-table nil)
(defvar yahtml-last-typeface-cmd "address")

(defvar yahtml-single-cmd-table
  '(("hr") ("br") ("option") ("p")
    ("HorizontalLine" . "hr")
    ("BreakLine" . "br")
    ("Paragraph" . "p")
    ("Item" . "li")
    ("DefineTerm" . "dt")
    ("Description" . "dd")
    ("dd") ("dt") ("li")
    )
  "Default completion table of HTML single command.")
(defvar yahtml-user-single-cmd-table nil)
(defvar yahtml-tmp-single-cmd-table nil)
(defvar yahtml-last-single-cmd nil)

(defvar yahtml-prefer-upcases nil)

;(defvar yahtml-struct-name-regexp
;  "\\<\\(h[1-6]\\|[uod]l\\|html\\|body\\|title\\|head\\|table\\|t[rhd]\\|pre\\|a\\|form\\|select\\|center\\|blockquote\\)\\b")
(defvar yahtml-struct-name-regexp
  (concat
   "\\<\\("
   (mapconcat (function (lambda (x) (car x))) yahtml-typeface-table "\\|")
   "\\)\\b")
  "Regexp of structure beginning.")

(defun yahtml-mode ()
  (interactive)
  (yatex-mode)
  (cond
   ((boundp 'MULE)
    (set-file-coding-system
     (cdr (assq yahtml-kanji-code YaTeX-kanji-code-alist))))
   ((boundp 'NEMACS)
    (make-local-variable 'kanji-fileio-code)
    (setq kanji-fileio-code yahtml-kanji-code)))
  (setq major-mode 'yahtml-mode
	mode-name "yahtml")
  (make-local-variable 'YaTeX-kanji-code)
  (make-local-variable 'YaTeX-ec) (setq YaTeX-ec "")
  (make-local-variable 'YaTeX-struct-begin)
  (setq YaTeX-struct-begin "<%1%2")
  (make-local-variable 'YaTeX-struct-end) (setq YaTeX-struct-end "</%1>")
  (make-local-variable 'YaTeX-struct-name-regexp)
  (setq YaTeX-struct-name-regexp yahtml-struct-name-regexp)
  (make-local-variable 'YaTeX-prefix-map)
  (make-local-variable 'YaTeX-command-token-regexp)
  (setq YaTeX-command-token-regexp yahtml-command-regexp)
  (make-local-variable 'YaTeX-comment-prefix)
  (setq YaTeX-comment-prefix "<!--")
  ;;(make-local-variable 'YaTeX-environment-indent)
  ;;(setq YaTeX-environment-indent 0)
  (make-local-variable 'fill-prefix)
  (setq fill-prefix nil)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-start  yahtml-paragraph-start
	paragraph-separate yahtml-paragraph-separate)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start "<!-- " comment-end " -->")
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'yahtml-indent-line)
  (make-local-variable 'YaTeX-item-regexp)
  (setq YaTeX-item-regexp "<\\(li\\|d[td]\\)>")
  (make-local-variable 'YaTeX-typesetting-mode-map)
  (setq YaTeX-typesetting-mode-map yahtml-lint-buffer-map)
  (set-syntax-table yahtml-syntax-table)
  (use-local-map yahtml-mode-map)
  (run-hooks 'yahtml-mode-hook))

(defun yahtml-define-menu (keymap bindlist)
  (mapcar
   (function
    (lambda (bind)
      (define-key keymap (vector (car bind)) (cdr bind))))
   bindlist))

(defvar yahtml-menu-map nil "Menu map of yahtml")
(defvar yahtml-menu-map-sectioning nil "Menu map of yahtml(sectioning)")
(defvar yahtml-menu-map-listing nil "Menu map of yahtml(listing)")
(defvar yahtml-menu-map-logical nil "Menu map of yahtml(logical tags)")
(defvar yahtml-menu-map-typeface nil "Menu map of yahtml(typeface tags)")

;;; Variables for mosaic url history
(defvar yahtml-urls nil "Alist of global history")
(defvar yahtml-url-history-file "~/.mosaic-global-history"
  "File name of url history")

(cond
 ((and YaTeX-emacs-19 (null yahtml-menu-map))
  (setq yahtml-menu-map (make-sparse-keymap "yahtml menu"))
  (setq yahtml-menu-map-sectioning (make-sparse-keymap "sectioning menu"))
  (yahtml-define-menu
   yahtml-menu-map-sectioning
   (nreverse
    '((1 "H1" . (lambda () (interactive) (yahtml-insert-begend nil "H1")))
      (2 "H2" . (lambda () (interactive) (yahtml-insert-begend nil "H2")))
      (3 "H3" . (lambda () (interactive) (yahtml-insert-begend nil "H3")))
      (4 "H4" . (lambda () (interactive) (yahtml-insert-begend nil "H4")))
      (5 "H5" . (lambda () (interactive) (yahtml-insert-begend nil "H5")))
      (6 "H6" . (lambda () (interactive) (yahtml-insert-begend nil "H6")))
      )))
  (setq yahtml-menu-map-logical (make-sparse-keymap "logical tags"))
  (yahtml-define-menu
   yahtml-menu-map-logical
   (nreverse
    '((em	"Embolden" .
	  (lambda () (interactive) (yahtml-insert-tag nil "EM")))
      (dfn	"Define a word" .
	(lambda () (interactive) (yahtml-insert-tag nil "DFN")))
      (cite	"Citation" .
	(lambda () (interactive) (yahtml-insert-tag nil "CITE")))
      (code	"Code" .
	(lambda () (interactive) (yahtml-insert-tag nil "CODE")))
      (kbd	"Keyboard" .
	(lambda () (interactive) (yahtml-insert-tag nil "KBD")))
      (samp	"Sample display" .
	(lambda () (interactive) (yahtml-insert-tag nil "SAMP")))
      (strong	"Strong" .
	(lambda () (interactive) (yahtml-insert-tag nil "STRONG")))
      (VAR	"Variable notation" .
	(lambda () (interactive) (yahtml-insert-tag nil "VAR")))
      )))
  (setq yahtml-menu-map-typeface (make-sparse-keymap "typeface tags"))
  (yahtml-define-menu
   yahtml-menu-map-typeface
   (nreverse
    '((b	"Bold" .
	  (lambda () (interactive) (yahtml-insert-tag nil "B")))
      (i	"Italic" .
	(lambda () (interactive) (yahtml-insert-tag nil "I")))
      (tt	"Typewriter" .
	(lambda () (interactive) (yahtml-insert-tag nil "TT")))
      (u	"Underlined" .
	(lambda () (interactive) (yahtml-insert-tag nil  "U")))
      )))
  (setq yahtml-menu-map-listing (make-sparse-keymap "listing"))
  (yahtml-define-menu
   yahtml-menu-map-listing
   (nreverse
    '((ul	"Unordered" .
		(lambda () (interactive) (yahtml-insert-begend nil "UL")))
      (ol	"Ordered" .
		(lambda () (interactive) (yahtml-insert-begend nil "OL")))
      (dl	"Definition" .
		(lambda () (interactive) (yahtml-insert-begend nil "DL")))
      )))
  (setq yahtml-menu-map-item (make-sparse-keymap "item"))
  (yahtml-define-menu
   yahtml-menu-map-item
   (nreverse
    '((li	"Simple item" .
		(lambda () (interactive) (yahtml-insert-single "li")))
      (dt	"Define term" .
		(lambda () (interactive) (yahtml-insert-single "dt")))
      (dd	"Description of term" .
		(lambda () (interactive) (yahtml-insert-single "dd")))
      )))
  (define-key yahtml-mode-map [menu-bar yahtml]
    (cons "yahtml" yahtml-menu-map))
  (let ((keys (where-is-internal 'fill-paragraph global-map)))
    (while keys
      (define-key yahtml-mode-map (car keys) 'yahtml-fill-paragraph)
      (setq keys (cdr keys))))
  (yahtml-define-menu
   yahtml-menu-map
   (nreverse
    (list
     (cons (list 'sect "Sectioning")
	   (cons "sectioning" yahtml-menu-map-sectioning))
     (cons (list 'list "Listing")
	   (cons "Listing" yahtml-menu-map-listing))
     (cons (list 'item "Item")
	   (cons "Itemizing" yahtml-menu-map-item));;; 
     (cons (list 'logi "Logical tags")
	   (cons "logical" yahtml-menu-map-logical))
     (cons (list 'type "Typeface tags")
	   (cons "typeface" yahtml-menu-map-typeface))
     )))
  ))

(defun yahtml-collect-url-history ()
  "Collect urls from global history file."
  (interactive)
  (save-excursion
    (set-buffer
     (find-file-noselect (expand-file-name yahtml-url-history-file)))
    (goto-char (point-min))
    (setq yahtml-urls)
    (message "Collecting global history...")
    (while (re-search-forward "^[A-Za-z]+:" nil t)
      (setq yahtml-urls
	    (cons (list
		   (buffer-substring
		    (progn (beginning-of-line) (point))
		    (progn (skip-chars-forward "^ ") (point))))
		  yahtml-urls)))
    (message "Collecting global history...Done")))

;;; ----------- Completion ----------
(defvar yahtml-last-begend "html")
(defun yahtml-insert-begend (&optional region env)
  "Insert <cmd> ... </cmd>."
  (interactive "P")
  (let*((completion-ignore-case t)
	(cmd
	 (or env
	     (YaTeX-cplread-with-learning
	      (format "Environment(default %s): " yahtml-last-begend)
	      'yahtml-env-table 'yahtml-user-env-table 'yahtml-tmp-env-table)))
	(bolp (save-excursion
		(skip-chars-backward " \t" (point-beginning-of-line)) (bolp)))
	(cc (current-column)))
    (if (string< "" cmd) (setq yahtml-last-begend cmd))
    (setq yahtml-last-begend
	  (or (cdr (assoc yahtml-last-begend yahtml-env-table))
	      yahtml-last-begend))
    (setq cmd yahtml-last-begend)
    (if yahtml-prefer-upcases (setq cmd (upcase cmd)))
    (if region
	(let ((beg (region-beginning))
	      (end (region-end))
	      (addin (yahtml-addin cmd)))
	  (goto-char end)
	  (insert (format "</%s>%s" cmd (if bolp "\n" "")))
	  (goto-char beg)
	  (insert (format "<%s%s>%s" cmd addin (if bolp "\n" ""))))
      (insert (format "<%s%s>" cmd (yahtml-addin cmd)))
      (save-excursion
	(insert "\n")
	(indent-to-column cc)
	(insert (format "</%s>" cmd)))
      (yahtml-intelligent-newline nil))))

(defun yahtml-insert-begend-region ()
  "Call yahtml-insert-begend in the region mode."
  (interactive)
  (yahtml-insert-begend t))


(defun yahtml-insert-form (&optional form)
  "Insert <FORM option=\"argument\">."
   (interactive)
   (or form
       (let ((completion-ignore-case t))
	 (setq form
	       (YaTeX-cplread-with-learning
		"Form: "
		'yahtml-form-table 'yahtml-user-form-table
		'yahtml-tmp-form-table))))
   (let ((p (point)) q)
     (if yahtml-prefer-upcases (setq form (upcase form)))
     (insert (format "<%s%s>" form (yahtml-addin form)))
     ;;(indent-relative-maybe)
     (if (cdr (assoc form yahtml-form-table))
	 (save-excursion (insert (format "</%s>" form))))
     (if (search-backward "\"\"" p t) (forward-char 1))))

;;; ---------- Add-in ----------
(defun yahtml-addin (form)
  "Check add-in function's existence and call it if exists."
   (let ((addin (concat "yahtml:" (downcase form))) s)
     (if (and (intern-soft addin) (fboundp (intern-soft addin))
	      (stringp (setq s (funcall (intern addin))))
	      (string< "" s))
	 (concat " " s)
       "")))


(defvar yahtml-completing-buffer nil)
(defun yahtml-collect-labels (&optional file)
  "Collect current buffers label (<a name=...>).
If optional argument FILE is specified collect labels in FILE."
  (let (list bound)
    (save-excursion
      (set-buffer yahtml-completing-buffer)
      (if file (let (hilit-auto-highlight)
		 (set-buffer (find-file-noselect file))))
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "<a\\b" nil t)
	  (setq bound (match-end 0))
	  (search-forward ">" nil t)
	  (if (and (re-search-backward "\\(name\\|id\\)=" bound t)
		   (goto-char (match-end 0))
		   (skip-chars-forward " \t\n")
		   (looking-at "\"?#?\\([^\">]+\\)\"?\\b"))
	      (setq list (cons
			  (list (concat "#" (YaTeX-match-string 1)))
			  list))))
	list)))
  )

(defvar yahtml-url-completion-map nil "Key map used in URL completion buffer")
(if yahtml-url-completion-map nil
  (setq yahtml-url-completion-map
	(copy-keymap minibuffer-local-completion-map))
  (define-key yahtml-url-completion-map "\t"	'yahtml-complete-url)
  (define-key yahtml-url-completion-map " "	'yahtml-complete-url)
)

(defun yahtml-complete-url ()
  "Complete external URL from history or local file name."
  (interactive)
  (let (initial i2 cmpl path dir file listfunc beg labels (p (point)))
    (setq initial (buffer-string))
    (cond
     ((string-match "^http:" initial)
      (setq cmpl (try-completion initial yahtml-urls)
	    listfunc (list 'lambda nil
			   (list 'all-completions initial 'yahtml-urls))
	    beg (point-min)))
     ((setq beg (string-match "#" initial))
      (or (equal beg 0)			;begin with #
	  (progn
	    (setq path (substring initial 0 beg))
	    (if (string-match "^/" path)
		(setq path (yahtml-url-to-path path)))))
      (setq initial (substring initial beg))
      (setq labels (yahtml-collect-labels path)
	    cmpl (try-completion initial labels)
	    listfunc (list 'lambda ()
			   (list 'all-completions
				 initial (list 'quote labels)))
	    beg (+ (point-min) beg)))
     (t
      (setq path (if (string-match "^/" initial)
		     (yahtml-url-to-path initial)
		   initial))
      (setq dir (or (file-name-directory path) ".")
	    file (file-name-nondirectory path)
	    initial file
	    cmpl (file-name-completion file dir)
	    listfunc (list 'lambda nil
			   (list 'file-name-all-completions
				 file dir))
	    beg (save-excursion (skip-chars-backward "^/") (point)))))
    (cond
     ((stringp cmpl)
      (if (string= initial cmpl)
	  (with-output-to-temp-buffer "*Completions*"
	    (princ "Possible completinos are:\n")
	    (princ
	     (mapconcat '(lambda (x) x)  (funcall listfunc) "\n")))
	(delete-region (point) beg)
	(insert cmpl)))
     ((null cmpl)
      (ding))
     ((eq t cmpl)
      (save-excursion
	(unwind-protect
	    (progn
	      (goto-char p)
	      (insert " [Sole completion]"))
	  (delete-region p (point-max))))))))
  
(defun yahtml:a ()
  "Add-in function for <a>"
  (let ((l yahtml-prefer-upcases))
    (or yahtml-urls (yahtml-collect-url-history))
    (setq yahtml-completing-buffer (current-buffer))
;   (concat "href=\""
;	   (completing-read "href: " yahtml-urls)
;	   "\"")
    (message "(H)ref  (N)ame?")
    (cond
     ((string-match "[nN]" (char-to-string (read-char)))
      (concat (if l "NAME" "name") "=\"" (read-string "name: ") "\""))
     (t
      (concat (if l "HREF" "href") "=\""
	      (read-from-minibuffer "href: " "" yahtml-url-completion-map)
	      "\"")))))

(defvar yahtml-parameters-completion-alist
  '(("align" ("top") ("middle") ("bottom") ("left") ("right") ("center"))
    ("src" . file)
    ("method" ("POST") ("GET"))))

(defun yahtml-read-parameter (par)
  (let* ((alist (cdr-safe (assoc (downcase par)
				yahtml-parameters-completion-alist)))
	 (prompt (concat par ": "))
	 v)
    (cond
     ((eq alist 'file)
      (read-file-name prompt "" nil nil ""))
     (alist
      (completing-read prompt alist))
     (t 
      (read-string prompt)))))
      
  
(defun yahtml:img ()
  "Add-in function for <img>"
  (or yahtml-urls (yahtml-collect-url-history))
  (let ((src (yahtml-read-parameter "src"))
	(alg (yahtml-read-parameter "align"))
	(alt (yahtml-read-parameter "alt"))
	(l yahtml-prefer-upcases))
    (concat (if l "SRC" "src") "=\"" src "\""
	    (if (string< "" alg)
		(concat " " (if l "ALIGN" "align") "=\"" alg "\""))
	    (if (string< "" alt)
		(concat " " (if l "ALT" "alt") "=\"" alt "\"")))))

(defun yahtml:form ()
  "Add-in function `form' input format"
  (concat
   " " (if yahtml-prefer-upcases "METHOD" "method=")
   (completing-read "Method: " '(("POST") ("GET")) nil t)
   " " (if yahtml-prefer-upcases "ACTION" "action") "=\""
   (read-string "Action: ") "\""
   ))

(defun yahtml:select ()
  "Add-in function for `select' input format"
  (setq yahtml-last-single-cmd "option")
  (concat " " (if yahtml-prefer-upcases "NAME" "name") "=\""
	  (read-string "name: ") "\""))

(defun yahtml:ol ()
  (setq yahtml-last-single-cmd "li") "")
(defun yahtml:ul ()
  (setq yahtml-last-single-cmd "li") "")
(defun yahtml:dl ()
  (setq yahtml-last-single-cmd "dt") "")
(defun yahtml:dt ()
  (setq yahtml-last-single-cmd "dd") "")

(defun yahtml:p ()
  (let ((alg (yahtml-read-parameter "align")))
    (if (string< "" alg)
	(setq alg (concat "align=" alg)
	      alg (if yahtml-prefer-upcases (upcase alg) (downcase alg)))
      "")))

(defvar yahtml-input-types
  '(("text") ("password") ("checkbox") ("radio") ("submit")
    ("reset") ("image") ("hidden") ("file")))

(defun yahtml:input ()
  "Add-in function for `input' form"
  (let ((size "") name type value checked (maxlength "")
	(l yahtml-prefer-upcases))
    (setq name (read-string "name: ")
	  type (completing-read "type (default=text): "
				yahtml-input-types nil t)
	  value (read-string "value: "))
    (if (string-match "text\\|password\\|^$" type)
	(setq size (read-string "size: ")
	      maxlength (read-string "maxlength: ")))
    (concat
     (if l "NAME" "name") "=\"" name "\""
     (if (string< "" type)
	 (concat " " (if l "TYPE" "type") "=\"" type "\""))
     (if (string< "" value)
	 (concat " " (if l "VALUE" "value") "=\"" value "\""))
     (if (string< "" size)
	 (concat " " (if l "SIZE" "size") "=\"" size "\""))
     (if (string< "" maxlength)
	 (concat " " (if l "MAXLENGTH" "maxlength") "=\"" maxlength "\""))
    )))

(defun yahtml:textarea ()
  "Add-in function for `textarea'"
  (interactive)
  (let (name rows cols)
    (setq name (read-string "Name: ")
	  cols (read-string "Columns: ")
	  rows (read-string "Rows: "))
    (concat
     (concat (if yahtml-prefer-upcases "NAME=" "name=")
	     "\"" name "\"")
     (if (string< "" cols)
	 (concat " " (if yahtml-prefer-upcases "COLS" "cols") "=" cols))
     (if (string< "" rows)
	 (concat " " (if yahtml-prefer-upcases "ROWS" "rows") "=" rows)))))


;;; ---------- Simple tag ----------
(defun yahtml-insert-tag (region-mode &optional tag)
  "Insert <TAG> </TAG> and put cursor inside of them."
  (interactive "P")
  (or tag
      (let ((completion-ignore-case t))
	(setq tag
	      (YaTeX-cplread-with-learning
	       (format "Tag %s(default %s): "
		       (if region-mode "region: " "") yahtml-last-typeface-cmd)
	       'yahtml-typeface-table 'yahtml-user-typeface-table
	       'yahtml-tmp-typeface-table))))
  (if (string= "" tag) (setq tag yahtml-last-typeface-cmd))
  (setq tag (or (cdr (assoc tag yahtml-typeface-table)) tag))
  (setq yahtml-last-typeface-cmd tag
	tag (funcall (if yahtml-prefer-upcases 'upcase 'downcase) tag))
  (if region-mode
      (if (if (string< "19" emacs-version) (mark t) (mark))
	  (save-excursion
	    (if (> (point) (mark)) (exchange-point-and-mark))
	    (insert (format "<%s%s>" tag (yahtml-addin tag)))
	    (exchange-point-and-mark)
	    (insert "</" tag ">"))
	(message "No mark set now"))
    (insert (format "<%s%s>" tag (yahtml-addin tag)))
    (save-excursion (insert (format "</%s>" tag)))))

(defun yahtml-insert-tag-region (&optional tag)
  "Call yahtml-insert-tag with region mode."
  (interactive)
  (yahtml-insert-tag t tag))

(defun yahtml-insert-single (cmd)
  "Insert <CMD>."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (YaTeX-cplread-with-learning
       (format "Command%s: "
	       (if yahtml-last-single-cmd
		   (concat "(default " yahtml-last-single-cmd ")") ""))
       'yahtml-single-cmd-table 'yahtml-user-single-cmd-table
       'yahtml-tmp-single-cmd-table))))
  (if (string= "" cmd) (setq cmd yahtml-last-single-cmd))
  (setq yahtml-last-single-cmd
	(or (cdr (assoc cmd yahtml-single-cmd-table)) cmd))
  (setq cmd (funcall (if yahtml-prefer-upcases 'upcase 'downcase)
		     yahtml-last-single-cmd))
  (insert (format "<%s>" cmd)))

;;; ---------- Jump ----------
(defun yahtml-on-href-p ()
  "Check if point is on href clause."
  (let ((p (point)) e cmd (case-fold-search t))
    (save-excursion
      (and (string= (YaTeX-inner-environment t) "a")
	   (save-excursion
	     (search-forward "</a>" nil t)
	     (setq e (point)))
	   (goto-char (get 'YaTeX-inner-environment 'point))
	   (search-forward "href" e t)
	   (search-forward "=" e t)
	   (skip-chars-forward " \t\n")
	   (looking-at "\"?\\([^\"> \t\n]+\\)\"?")
	   (< p (match-end 0))
	   (YaTeX-match-string 1)
	   ))))

(defun yahtml-netscape-sentinel (proc mes)
  (cond
   ((null (buffer-name (process-buffer proc)))
    (set-process-buffer proc nil))
   ((eq (process-status proc) 'exit)
    (let ((cb (current-buffer)))
      (set-buffer (process-buffer proc))
      (goto-char (point-min))
      (if (search-forward "not running" nil t)
	  (progn
	    (message "Starting netscape...")
	    (start-process
	     "browser" (process-buffer proc)
	     shell-file-name yahtml-shell-command-option
	     (format "%s \"%s\"" yahtml-www-browser
		     (get 'yahtml-netscape-sentinel 'url)))
	    (message "Starting netscape...Done")))
      (set-buffer cb)))))

(defvar yahtml-browser-process nil)

(defun yahtml-browse-html (href)
  "Call WWW Browser to see HREF."
  (let ((pb "* WWW Browser *") (cb (current-buffer)))
    (cond
     ((and (string-match "[Nn]etscape" yahtml-www-browser)
	   (not (eq system-type 'windows-nt)))
      (if (get-buffer pb)
	  (progn (set-buffer pb) (erase-buffer) (set-buffer cb)))
      (put 'yahtml-netscape-sentinel 'url href)
      (set-process-sentinel
       (setq yahtml-browser-process
	     (start-process
	      "browser" pb shell-file-name yahtml-shell-command-option ;"-c"
	      (format "%s -remote \"openURL(%s)\"" yahtml-www-browser href)))
       'yahtml-netscape-sentinel))
     ((and (string= "w3" yahtml-www-browser) (fboundp 'w3-fetch))
      (w3-fetch href))
     ((stringp yahtml-www-browser)
      (if (and yahtml-browser-process
	       (eq (process-status yahtml-browser-process) 'run))
	  (message "%s is already running" yahtml-www-browser)
	(setq yahtml-browser-process
	      (start-process
	       "browser" "* WWW Browser *"
	       shell-file-name yahtml-shell-command-option
	       (format "%s \"%s\"" yahtml-www-browser href)))))
     (t
      (message "Sorry, jump across http is not supported.")))))

(defun yahtml-goto-corresponding-href (&optional other)
  "Go to corresponding name."
  (let ((href (yahtml-on-href-p)) file name)
    (if href
	(cond
	 ((string-match "^\\(ht\\|f\\)tp:" href)
	  (yahtml-browse-html href))
	 (t (setq file (substring href 0 (string-match "#" href)))
	    (if (string-match "#" href)
		(setq name (substring href (1+ (string-match "#" href)))))
	    (if (string< "" file)
		(progn
		  (if (string-match "/$" file)
		      (setq file (concat file yahtml-directory-index)))
		  (if (string-match "^/" file)
		      (setq file (yahtml-url-to-path file)))
		  (if other (YaTeX-switch-to-buffer-other-window file)
		    (YaTeX-switch-to-buffer file))))
	    (if name
		(progn (set-mark-command nil) (yahtml-jump-to-name name)))
	    t)))))

(defun yahtml-jump-to-name (name)
  "Jump to html's named tag."
  (setq name (format "name\\s *=\\s *\"?%s\"?" name))
  (or (and (re-search-forward name nil t) (goto-char (match-beginning 0)))
      (and (re-search-backward name nil t) (goto-char (match-beginning 0)))
      (message "Named tag `%s' not found" (substring href 1))))

(defun yahtml-on-begend-p (&optional p)
  "Check if point is on begend clause."
  (let ((p (or p (point))) cmd (case-fold-search t))
    (save-excursion
      (goto-char p)
      (if (equal (char-after (point)) ?<) (forward-char 1))
      (if (and (re-search-backward "<" nil t)
	       (looking-at
		(concat "<\\(/?" yahtml-struct-name-regexp "\\)\\b"))
	       (condition-case nil
		   (forward-list 1)
		 (error nil))
	       (< p (point)))
	  (YaTeX-match-string 1)))))

(defun yahtml-goto-corresponding-begend (&optional noerr)
  "Go to corresponding opening/closing tag.
Optional argument NOERR causes no error for unballanced tag."
  (let ((cmd (yahtml-on-begend-p)) m0
	(p (point)) (case-fold-search t) func str (nest 0))
    (cond
     (cmd
      (setq m0 (match-beginning 0))
      (if (= (aref cmd 0) ?/)		;on </cmd> line
	      (setq cmd (substring cmd 1)
		    str (format "\\(<%s\\)\\|\\(</%s\\)" cmd cmd)
		    func 're-search-backward)
	    (setq str (format "\\(</%s\\)\\|\\(<%s\\)" cmd cmd)
		  func 're-search-forward))
      (while (and (>= nest 0) (funcall func str nil t))
	(if (equal m0 (match-beginning 0))
	    nil
	  (setq nest (+ nest (if (match-beginning 1) -1 1)))))
      (if (< nest 0)
	  (goto-char (match-beginning 0))
	(funcall
	 (if noerr 'message 'error)
	 "Corresponding tag of `%s' not found." cmd)
	(goto-char p)
	nil))
     (t nil))))

(defun yahtml-current-tag ()
  "Return the current tag name."
  (save-excursion
    (let ((p (point)) b tag)
      (or (bobp)
	  (looking-at "<")
	  (progn (skip-chars-backward "^<") (forward-char -1)))
      (setq b (point))
      (skip-chars-forward "<")
      (setq tag (buffer-substring
		 (point) (progn (skip-chars-forward "^ \t\n") (point))))
      (goto-char b)
      (forward-list 1)
      (and (< p (point)) tag))))
      

(defun yahtml-goto-corresponding-img ()
  "View image on point"
  (let ((tag (yahtml-current-tag)) image (p (point)) (case-fold-search t))
    (if (and tag
	     (string-match "img" tag)
	     (save-excursion
	       (re-search-backward "<\\s *img" nil t)
	       (re-search-forward "src=\"?\\([^\"> ]+\\)\"?")
	       (match-beginning 1)
	       (setq image
		     (buffer-substring (match-beginning 1) (match-end 1)))))
	(progn
	  (message "Invoking %s %s..." yahtml-image-viewer image)
	  (start-process
	   "Viewer" " * Image Viewer *"
	   shell-file-name yahtml-shell-command-option ;"-c"
	   (concat yahtml-image-viewer " " image))
	  (message "Invoking %s %s...Done" yahtml-image-viewer image)))))

(defun yahtml-goto-corresponding-* (&optional other)
  "Go to corresponding object."
  (interactive)
  (cond
   ((yahtml-goto-corresponding-href other))
   ((yahtml-goto-corresponding-img))
   ((yahtml-goto-corresponding-begend))
   ))

(defun yahtml-goto-corresponding-*-other-window ()
  "Go to corresponding object."
  (interactive)
  (yahtml-goto-corresponding-* t))

;;; ---------- killing ----------
(defun yahtml-kill-begend (&optional whole)
  (let ((tag (yahtml-on-begend-p)) (p (make-marker)) (q (make-marker)))
    (if tag
	(progn
	  (or (looking-at "<")
	      (progn (skip-chars-backward "^<") (forward-char -1)))
	  (set-marker p (point))
	  (yahtml-goto-corresponding-begend)
	  (or (looking-at "<")
	      (progn (skip-chars-backward "^<") (forward-char -1)))
	  (delete-region (point) (progn (forward-list 1) (point)))
	  (set-marker q (point))
	  (beginning-of-line)
	  (if (looking-at "^\\s *$")
	      (delete-region (point) (progn (forward-line 1) (point))))
	  (goto-char p)
	  (delete-region (point) (progn (forward-list 1) (point)))
	  (if (looking-at "^\\s *$")
	      (delete-region (point) (progn (forward-line 1) (point))))
	  (if whole (delete-region p q))
	  tag))))

(defun yahtml-kill-* (whole)
  "Kill current position's HTML tag (set)."
  (interactive "P")
  (cond
   ((yahtml-kill-begend whole))
   ))


;;; ---------- changing ----------
(defun yahtml-on-assignment-p ()
  "Return if current point is on parameter assignment.
If so, return parameter name, otherwise nil.
This function should be able to treat white spaces in value, but not yet."
  (let ((p (point)))
    (save-excursion
      (put 'yahtml-on-assignment-p 'region nil)
      (skip-chars-backward "^ \t")
      (and (looking-at "\\([A-Za-z0-9]+\\)\\s *=\\s *\"?\\([^ \t\"]+\\)\"?")
	   (< p (match-end 0))
	   (>= p (1- (match-beginning 2)))
	   (put 'yahtml-on-assignment-p 'region
		(cons (match-beginning 2) (match-end 2)))
	   (YaTeX-match-string 1)))))

(defun yahtml-change-begend ()
  (let ((tag (yahtml-on-begend-p))
	(completion-ignore-case t)
	(case-fold-search t)
	(p (point)) (q (make-marker))
	(default (append yahtml-env-table yahtml-typeface-table))
	(user (append yahtml-user-env-table yahtml-user-typeface-table))
	(tmp (append yahtml-tmp-env-table yahtml-tmp-typeface-table))
	href b1 e1)
    (cond
     (tag
      (cond
       ((and (string-match "^a$" tag)
	     (save-excursion
	       (and
		(re-search-backward "<a\\b" nil t)
		(goto-char (match-end 0))
		(skip-chars-forward " \t\n")
		(setq b1 (point))
		(search-forward ">" nil t)
		(setq e1 (match-beginning 0))
		(goto-char b1)
		(re-search-forward "href\\s *=" e1 t)
		(>= p (point))
		(goto-char (match-end 0))
		(skip-chars-forward " \t\n")
		(looking-at "\"?\\([^\"> \t\n]+\\)\"?")
		(< p (match-end 0)))))
	(setq b1 (match-beginning 1) e1 (match-end 1)
	      yahtml-completing-buffer (current-buffer)
	      href (read-from-minibuffer
		    "Change href to: " "" yahtml-url-completion-map))
	(if (string< "" href)
	    (progn
	      ;;(setq href  ;??
		;;    (if yahtml-prefer-upcases (upcase href) (downcase href)))
	      (delete-region b1 e1)
	      (goto-char b1)
	      (insert href))))
       (t
	(save-excursion
	  (if (= (aref tag 0) ?/) (setq tag (substring tag 1)))
	  (or (= (char-after (point)) ?<) (skip-chars-backward "^<"))
	  (skip-chars-forward "^A-Za-z")
	  (set-marker q (point))
	  (setq p (point))
	  (yahtml-goto-corresponding-begend)
	  (or (= (char-after (point)) ?<)
	      (skip-chars-backward "^<"))
	  (skip-chars-forward "^A-Za-z")
	  (if (= (char-after (1- (point))) ?/)
	      (progn
		(set-marker q (point))
		(goto-char p)))
	  (setq tag (let ((completion-ignore-case t))
		      (YaTeX-cplread-with-learning
		       (format "Change `%s' to(default %s): "
			       tag yahtml-last-begend)
		       'default 'user 'tmp)))
	  (delete-region (point) (progn (skip-chars-forward "^>") (point)))
	  (if (string= "" tag) (setq tag yahtml-last-begend))
	  (setq yahtml-last-begend
		(or (cdr (assoc tag yahtml-env-table)) tag)
		tag yahtml-last-begend)
	  (setq tag (if yahtml-prefer-upcases (upcase tag) (downcase tag)))
	  (insert (format "%s%s" tag (yahtml-addin tag)))
	  (goto-char q)
	  (delete-region (point) (progn (skip-chars-forward "^>") (point)))
	  (insert tag))))
      t))))

(defun yahtml-change-command ()
  (let ((p (point)) (case-fold-search t) cmd par new
	(beg (make-marker)) (end (make-marker)))
    (skip-chars-backward "^<")
    (if (and
	 (looking-at yahtml-command-regexp)
	 (progn
	   (set-marker beg (match-beginning 0))
	   (set-marker end (match-end 0))
	   t)				;for further work
	 (progn
	   (forward-char -1)
	   (condition-case nil
	       (forward-list 1)
	     (error nil))
	   (< p (point))))
	(progn
	  (goto-char p)
	  (if (setq par (yahtml-on-assignment-p))
	      (progn
		(setq new (yahtml-read-parameter par))
		(set-marker beg (car (get 'yahtml-on-assignment-p 'region)))
		(set-marker end (cdr (get 'yahtml-on-assignment-p 'region))))
	    (setq new
		  (YaTeX-cplread-with-learning
		   "Change form to: "
		   'yahtml-form-table 'yahtml-user-form-table
		   'yahtml-tmp-form-table)))
	  (delete-region beg end)
	  (goto-char beg)
	  (insert new)
	  t)
      (goto-char p)
      nil)))

(defun yahtml-change-* ()
  "Change current position's HTML tag (set)."
  (interactive)
  (cond
   ((yahtml-change-begend))
   ((yahtml-change-command))
  ))

;;; ---------- commenting ----------
(defun yahtml-comment-region (beg end)
  (interactive "r")
  (comment-region beg end nil))

(defun yahtml-uncomment-region (beg end)
  (interactive "r")
  (comment-region beg end '(4)))



(defun yahtml-inner-environment-but (exclude &optional quick)
  "Return the inner environment but matches with EXCLUDE tag."
  (let (e)
    (save-excursion
      (while (and (setq e (YaTeX-inner-environment quick))
		  (string-match exclude e))
	(goto-char (get 'YaTeX-inner-environment 'point))))
    e))

;;; ---------- filling ----------

(defvar yahtml-saved-move-to-column (symbol-function 'move-to-column))
(defun yahtml-move-to-column (col &optional force)
  (beginning-of-line)
  (let ((ccol 0))
  (while (and (> col ccol) (not (eolp)))
    (if (eq (following-char) ?\<)
	(progn
	  (while (and (not (eq (following-char) ?\>))
		      (not (eolp)))
		   (forward-char))
	  (or (eolp) (forward-char)))
      (or (eolp) (forward-char))
      (if (eq (preceding-char) ?\t)
	  (let ((wd (- 8 (% (+ ccol 8) 8))))
	    (if (and force (< col (+ ccol wd)))
		(progn
		  (backward-char 1)
		  (insert-char ?\  (- col ccol))
		  (setq ccol col))
	      (setq ccol (+ ccol wd))))
	(setq ccol (1+ ccol)))
      (if (and YaTeX-japan
	       (string-match "[chj]" (char-category (preceding-char))))
	  (setq ccol (1+ ccol)))))
  (if (and force (> col ccol))
      (progn
	(insert-char ?\  (- col ccol))
	col)
    ccol)))

(defun yahtml-fill-paragraph (arg)
  (interactive "P")
  (let*((case-fold-search t) (p (point))
	(e (or (yahtml-inner-environment-but "^\\(a\\|p\\)\\b" t) "html"))
	(prep (string-match "^pre$" e))
	(ps1 (if prep (default-value 'paragraph-start)
	       paragraph-start))
	(ps2 (if prep (concat (default-value 'paragraph-start)
			      "\\|^\\s *</?pre>")
	       paragraph-start)))
    (save-excursion
      (unwind-protect
	  (progn
	    (if prep
		(fset 'move-to-column 'yahtml-move-to-column))
	    (fill-region-as-paragraph
	     (progn (re-search-backward paragraph-start nil t)
		    (or (save-excursion
			  (goto-char (match-end 0))
			  (skip-chars-forward " \t>")
			  (if (looking-at "[ \t]*$")
			      (progn (forward-line 1) (point))))
			(point)))
	     (progn (goto-char p)
		    (re-search-forward ps2 nil t)
		    (match-beginning 0))))
	(fset 'move-to-column yahtml-saved-move-to-column)))))

;(defun yahtml-indent-new-commnet-line ()
;  (unwind-protect
;      (progn
;	(fset 'move-to-column 'yahtml-move-to-column)
;	(apply 'YaTeX-saved-indent-new-comment-line (if soft (list soft))))
;    (fset 'move-to-column yahtml-saved-move-to-column)))

;;; 
;;; ---------- indentation ----------
;;; 
(defun yahtml-indent-line ()
  (interactive)
  (let ((envs "[uod]l\\|table\\|t[rhd]\\|select\\|blockquote")
	(itms "<\\(dt\\|dd\\|li\\|t[rdh]\\|option\\)\\b")
	inenv p col peol (case-fold-search t))
    (save-excursion
      (beginning-of-line)
      (setq inenv (or (yahtml-inner-environment-but "^\\(a\\|p\\)\\b" t)
		      "html")
	    col (get 'YaTeX-inner-environment 'indent)
	    p (get 'YaTeX-inner-environment 'point)
	    op))
    (save-excursion
      (cond
       ((string-match envs inenv)
	(save-excursion
	  (beginning-of-line)
	  (skip-chars-forward " \t")
	  (cond
	   ((looking-at (concat "</\\(" envs "\\)>"))
	    (YaTeX-reindent col))
	   ((or (looking-at itms)
		(and yahtml-hate-too-deep-indentation
		     (looking-at (concat "<" envs))))
	    (YaTeX-reindent (+ col yahtml-environment-indent)))
	   ((and (< p (point))
		 (save-excursion
		   (and
		    ;;(re-search-backward itms p t)
		    (setq op (point))
		    (goto-char p)
		    (re-search-forward itms op t)
		    ;(goto-char (match-end 0))
		    (skip-chars-forward "^>")
		    (skip-chars-forward ">")
		    (skip-chars-forward " \t")
		    (setq col (current-column)))))
	    (YaTeX-reindent col))
	   (t
	    (YaTeX-reindent (+ col yahtml-environment-indent)))))))
      (and (bolp) (skip-chars-forward " \t"))
      (if (and (setq inenv (yahtml-on-begend-p))
	       (string-match (concat "^\\(" envs "\\)") inenv))
	  (save-excursion
	    (setq peol (point-end-of-line))
	    (or (= (char-after (point)) ?<)
		(progn (skip-chars-backward "^<") (forward-char -1)))
	    (setq col (current-column))
	    (if (and (yahtml-goto-corresponding-begend t)
		     (> (point) peol))	;if on the different line
		(YaTeX-reindent col)))))
    (and (bolp) (skip-chars-forward " \t"))))

;(defun yahtml-fill-item ()
;  "Fill item HTML version"
;  (interactive)
;  (let (inenv p fill-prefix peol (case-fold-search t))
;    (setq inenv (or (YaTeX-inner-environment) "html")
;	  p (get 'YaTeX-inner-environment 'point))
;    (cond
;     ((string-match "^[uod]l" inenv)
;      (save-excursion
;	(if (re-search-backward "<\\(d[td]\\|li\\)>[ \t\n]*" p t)
;	    (progn
;	      (goto-char (match-end 0))
;	      (setq col (current-column)))
;	  (error "No <li>, <dt>, <dd>")))
;      (save-excursion
;	(end-of-line)
;	(setq peol (point))
;	(newline)
;	(indent-to-column col)
;	(setq fill-prefix (buffer-substring (point) (1+ peol)))
;	(delete-region (point) peol)
;	(fill-region-as-paragraph
;	 (progn (re-search-backward paragraph-start nil t) (point))
;	 (progn (re-search-forward paragraph-start nil t 2)
;		(match-beginning 0)))))
;     (t nil))))

;;; 
;;; ---------- Lint and Browsing ----------
;;; 
(defun yahtml-browse-menu ()
  "Browsing menu"
  (interactive)
  (message "J)weblint p)Browse R)eload...")
  (let ((c (char-to-string (read-char))))
    (cond
     ((string-match "j" c)
      (yahtml-lint-buffer (current-buffer)))
     ((string-match "[bp]" c)
      (yahtml-browse-current-file))
     ((string-match "r" c)
      (yahtml-browse-reload)))))

(defvar yahtml-lint-buffer "*weblint*")

(defun yahtml-lint-buffer (buf)
  "Call lint on buffer BUF."
  (interactive "bCall lint on buffer: ")
  (setq buf (get-buffer buf))
  (YaTeX-save-buffers)
  (YaTeX-typeset
   (concat yahtml-lint-program " "
	   (file-name-nondirectory (buffer-file-name buf)))
   yahtml-lint-buffer  "lint" "lint"))

(defun yahtml-file-to-url (file)
  "Convert local unix file name to URL.
If no matches found in yahtml-path-url-alist, return raw file name."
  (let ((list yahtml-path-url-alist) p url)
    (if (file-directory-p file)
	(setq file (expand-file-name yahtml-directory-index file))
      (setq file (expand-file-name file)))
    (if (string-match "^[A-Za-z]:/" file)
	(progn
	  ;; (aset file 1 ?|) ;これは要らないらしい…
	  (setq file (concat "///" file))))
    (while list
      (if (string-match (concat "^" (regexp-quote (car (car list)))) file)
	  (setq url (cdr (car list))
		file (substring file (match-end 0))
		url (concat url file)
		list nil))
      (setq list (cdr list)))
    (or url (concat "file:" file))))

(defun yahtml-url-to-path (file &optional basedir)
  "Convert local URL name to unix file name."
  (let ((list yahtml-path-url-alist) url realpath docroot
	(dirsufp (string-match "/$" file)))
    (setq basedir (or basedir
		      (file-name-directory
		       (expand-file-name default-directory))))
    (cond
     ((string-match "^/" file)
      (while list
	(if (file-directory-p (car (car list)))
	    (progn
	      (setq url (cdr (car list)))
	      (if (string-match "\\(http://[^/]*\\)/" url)
		  (setq docroot (substring url (match-end 1)))
		(setq docroot url))
	      (if (string-match (concat "^" (regexp-quote docroot)) file)
		  (setq realpath
			(expand-file-name
			 (substring
			  file
			  (if (= (aref file (1- (match-end 0))) ?/)
			      (match-end 0) ; "/foo"
			    (min (1+ (match-end 0)) (length file)))) ; "/~foo"
			 (car (car list)))))
	      (if realpath
		  (progn (setq list nil)
			 (if (and dirsufp (not (string-match "/$" realpath)))
			     (setq realpath (concat realpath "/")))))))
	(setq list (cdr list)))
      realpath)
     (t file))))
		
(defun yahtml-browse-current-file ()
  "Call WWW browser on current file."
  (interactive)
  (basic-save-buffer)
  (yahtml-browse-html (yahtml-file-to-url (buffer-file-name))))

(defun yahtml-browse-reload ()
  "Send `reload' event to netzscape."
  (let ((pb "* WWW Browser *") (cb (current-buffer)))
    (cond
     ((string-match "[Nn]etscape" yahtml-www-browser)
      (if (get-buffer pb)
	  (progn (set-buffer pb) (erase-buffer) (set-buffer cb)))
      ;;(or (get 'yahtml-netscape-sentinel 'url)
	;;  (error "Reload should be called after Browsing."))
      (put 'yahtml-netscape-sentinel 'url
	   (yahtml-file-to-url (buffer-file-name)))
      (basic-save-buffer)
      (set-process-sentinel
       (setq yahtml-browser-process
	     (start-process
	      "browser" pb shell-file-name yahtml-shell-command-option ;"-c"
	      (format "%s -remote 'reload'" yahtml-www-browser)))
       'yahtml-netscape-sentinel))
     (t
      (message "Sorry, RELOAD is supported only for Netscape.")))))

;;; ---------- Intelligent newline ----------
(defun yahtml-intelligent-newline (arg)
  "Intelligent newline for HTML"
  (interactive "P")
  (let (env func)
    (end-of-line)
    (setq env (downcase (or (YaTeX-inner-environment) "html")))
    (setq func (intern-soft (concat "yahtml-intelligent-newline-" env)))
    (newline)
    (if (and env func (fboundp func))
	(funcall func))))

(defun yahtml-intelligent-newline-ul ()
  (interactive)
  (insert (if yahtml-prefer-upcases "<LI> " "<li> "))
  (yahtml-indent-line))

(fset 'yahtml-intelligent-newline-ol 'yahtml-intelligent-newline-ul)

(defun yahtml-intelligent-newline-dl ()
  (interactive)
  (let ((case-fold-search t))
    (if (save-excursion
	  (re-search-backward "<\\(\\(dt\\)\\|\\(dd\\)\\)>"
			      (get 'YaTeX-inner-environment 'point) t))
	(cond
	 ((match-beginning 2)
	  (insert (if yahtml-prefer-upcases "<DD> " "<dd> "))
	  (setq yahtml-last-single-cmd "dt"))
	 ((match-beginning 3)
	  (insert (if yahtml-prefer-upcases "<DT> " "<dt> "))
	  (setq yahtml-last-single-cmd "dd")))
      (insert (if yahtml-prefer-upcases "<DT> " "<dt> ")))
    (yahtml-indent-line)))

(defun yahtml-intelligent-newline-select ()
  (interactive)
  (insert "<" (if yahtml-prefer-upcases "OPTION" "option") "> ")
  (yahtml-indent-line))

;;; ---------- Marking ----------
(defun yahtml-mark-begend ()
  "Mark current tag"
  (interactive)
  (YaTeX-beginning-of-environment)
  (let ((p (point)))
    (save-excursion
      (skip-chars-backward " \t" (point-beginning-of-line))
      (if (bolp) (setq p (point))))
    (push-mark p t))
  (yahtml-goto-corresponding-begend)
  (forward-list 1)
  (if (eolp) (forward-char 1)))

;;; ---------- complete marks ----------
(defun yahtml-complete-mark ()
  "Complete &gt, &lt, &ampersand, and &quote."
  (interactive)
  (message "1:< 2:> 3:& 4:\"")
  (let ((c (read-char)))
    (setq c (if (or (< c ?0) (> c ?5))
		(string-match (regexp-quote (char-to-string c)) "<>&\"")
	      (- c ?1)))
    (if (or (< c 0) (> c 4))
	nil
      (insert (format "&%s;" (nth c '("lt" "gt" "amp" "quot")))))))


;;; ---------- jump to error line ----------
(defvar yahtml-error-line-regexp
  "^\\(.*\\)(\\([0-9]+\\)):"
  "*Regexp of error position which is produced by lint program.")
(defun yahtml-prev-error ()
  "Jump to previous error seeing lint buffer."
  (interactive)
  (or (get-buffer yahtml-lint-buffer)
      (error "No lint program ran."))
  (YaTeX-showup-buffer yahtml-lint-buffer nil t)
  (yahtml-jump-to-error-line))

(defun yahtml-jump-to-error-line ()
  (interactive)
  (let ((p (point)) (e (point-end-of-line)))
    (end-of-line)
    (if (re-search-backward yahtml-error-line-regexp nil t)
	(let ((f (YaTeX-match-string 1))
	      (l (string-to-int (YaTeX-match-string 2))))
	  (forward-line -1)
	  (YaTeX-showup-buffer f nil t)
	  (goto-line l))
      (message "No line number usage"))))
    
;;; ---------- ----------

;;;
;;hilit19
;;;
(defvar yahtml-default-face-table
  '(
    (form	black/ivory	white/hex-442233	italic)
    ))
(defvar yahtml-hilit-patterns-alist
  '(
    ;; comments
    ("<!--\\s " "-->" comment)
    ;; include&exec
    ("<!--#\\(include\\|exec\\)" "-->" include)
    ;; string
    (hilit-string-find 39 string)
    (yahtml-hilit-region-tag "\\(em\\|strong\\)" bold)
    ("</?[uod]l>" 0 decl)
    ("<\\(di\\|dt\\|li\\|dd\\)>" 0 label)
    ("<a\\s +href" "</a>" crossref)
    ("</?\\sw+>" 0 decl)
    ("<form" "</form" form)
    ))

(defun yahtml-hilit-region-tag (tag)
  "Return list of start/end point of <TAG> form."
  (if (re-search-forward (concat "<" tag ">") nil t)
      (let ((m0 (match-beginning 0)))
	(skip-chars-forward " \t\n")
	(cons (point)
	      (progn (re-search-forward (concat "</" tag ">") nil t)
		     (match-beginning 0))))))

;(setq hilit-patterns-alist (delq (assq 'yahtml-mode hilit-patterns-alist) hilit-patterns-alist))
(cond
 ((and (featurep 'hilit19) (featurep 'yatex19))
  (or (assq 'yahtml-mode hilit-patterns-alist)
      (setq hilit-patterns-alist
	    (cons (cons 'yahtml-mode yahtml-hilit-patterns-alist)
		  hilit-patterns-alist)))))

(provide 'yahtml)

; Local variables:
; fill-prefix: ";;; "
; paragraph-start: "^$\\|\\|;;;$"
; paragraph-separate: "^$\\|\\|;;;$"
; End:

;;; -*- Emacs-Lisp -*-
;;; (c ) 1994 by HIROSE Yuuji [yuuji@ae.keio.ac.jp, pcs39334@ascii-net.or.jp]
;;; Last modified Mon Nov 20 11:26:38 1995 on inspire
;;; This is PURELY tentative.
;;; $Id$

;;;[Commentary]
;;;
;;; It is assumed you are already familiar with YaTeX.  The following
;;; completing featureas are available: ([prefix] means `C-c' by default)
;;;
;;;  * [prefix] b X	Complete environments such as `H1' which
;;;			normally requires closing tag `</H1>
;;;			<a href=foo> ... </a> is also classified into
;;;			this group
;;;  * [prefix] s	Complete declarative notations such as
;;;			`<img src="foo.gif">'
;;;			`<input name="var" ...>'
;;;  * [prefix] l	Complete typeface-changing commands such as
;;;			`<i> ... </i>' or `<samp> ... </samp>'
;;;  * [prefix] m	Complete single commands such as
;;;			`<br>' or `<hr>'
;;;  * menu-bar yahtml	Complete all by selecting a menu item (Though I
;;;			hate menu, this is most useful)
;;;
;;; NOTE!  This program is  truly  tentative.  If  you find some  bright
;;; future with this, please send me a mail to drive me to maintain this :)


(require 'yatex)
(defvar yahtml-prefix-map nil)
(defvar yahtml-mode-map nil "Keymap used in yahtml-mode.")

(defun yahtml-define-begend-key-normal (key env &optional map)
  "Define short cut yahtml-insert-begin-end key."
  (YaTeX-define-key
   key
   (list 'lambda '(arg) '(interactive "P")
	 (list 'yahtml-insert-begin-end env 'arg))
   map))

(defun yahtml-define-begend-region-key (key env &optional map)
  "Define short cut yahtml-insert-begin-end-region key."
  (YaTeX-define-key key (list 'lambda nil '(interactive)
			      (list 'yahtml-insert-begin-end env t)) map))

(defun yahtml-define-begend-key (key env &optional map)
  "Define short cut key for begin type completion both for
normal and region mode.  To customize yahtml, user should use this function."
  (yahtml-define-begend-key-normal key env map)
  (if YaTeX-inhibit-prefix-letter nil
    (yahtml-define-begend-region-key
     (concat (upcase (substring key 0 1)) (substring key 1)) env)))


(if yahtml-mode-map nil
  (setq yahtml-mode-map (make-sparse-keymap)
	yahtml-prefix-map (make-sparse-keymap))
  (define-key yahtml-mode-map YaTeX-prefix yahtml-prefix-map)
  (define-key yahtml-mode-map "\M-\C-@" 'YaTeX-mark-environment)
  (define-key yahtml-mode-map "\M-\C-a" 'YaTeX-beginning-of-environment)
  (define-key yahtml-mode-map "\M-\C-e" 'YaTeX-end-of-environment)
  (define-key yahtml-mode-map "\C-i" 'YaTeX-indent-line)
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
    (YaTeX-define-key "m" 'yahtml-insert-single map)
    (YaTeX-define-key "n" '(lambda () (interactive) (insert "<br>\n")) map)
    (if YaTeX-no-begend-shortcut
	(progn
	  (YaTeX-define-key "B" 'yahtml-insert-begend-region map)
	  (YaTeX-define-key "b" 'yahtml-insert-begend map))
      (yahtml-define-begend-key "bh" "HTML" map)
      (yahtml-define-begend-key "bH" "HEAD" map)
      (yahtml-define-begend-key "bt" "TITLE" map)
      (yahtml-define-begend-key "bb" "BODY" map)
      (yahtml-define-begend-key "bd" "DL" map)
      (yahtml-define-begend-key "b1" "H1" map)
      (yahtml-define-begend-key "b2" "H2" map)
      (yahtml-define-begend-key "b3" "H3" map)
      (yahtml-define-begend-key "ba" "a" map)
      (yahtml-define-begend-key "bf" "form" map)
      (yahtml-define-begend-key "bs" "select" map)
      (YaTeX-define-key "b " 'yahtml-insert-begend map)
      (YaTeX-define-key "B " 'yahtml-insert-begend map)
      )
    (YaTeX-define-key "e" 'yahtml-end-environment map)
    (YaTeX-define-key ">" 'yahtml-comment-region map)
    (YaTeX-define-key "<" 'yahtml-uncomment-region map)
    (YaTeX-define-key "g" 'yahtml-goto-corresponding-* map)
    )
)

(defvar yahtml-paragraph-separate
  (concat
   "^$\\|<br>\\|<p>\\|^[ \t]*</?\\(h[1-6]\\|p\\|dl\\|dd\\|dt\\|li\\|body\\|html\\|head\\|title\\|ul\\|ol\\|dl\\|pre\\)>")
  "*Regexp of html paragraph separater")
(defvar yahtml-syntax-table nil
  "*Syntax table for typesetting buffer")

(if yahtml-syntax-table nil
  (setq yahtml-syntax-table
	(make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?\< "(" yahtml-syntax-table)
  (modify-syntax-entry ?\> ")" yahtml-syntax-table)
)
(defvar yahtml-command-regexp "[A-Za-z0-9]+"
  "Regexp of constituent of html commands.")
(defvar yahtml-kanji-code 2
  "Kanji coding system of html file; 1=sjis, 2=jis, 3=euc")

;;; Completion tables for `form'
(defvar yahtml-form-table
  '(("img") ("input")))
(defvar yahtml-user-form-table nil)
(defvar yahtml-tmp-form-table nil)

(defvar yahtml-env-table
  '(("html") ("head") ("title") ("body") ("dl") ("a") ("form") ("select")
    ("h1") ("h2") ("h3") ("h4") ("h5") ("h6") ("ul")))

(defvar yahtml-user-env-table nil)
(defvar yahtml-tmp-env-table nil)

;;; Completion tables for typeface designator
(defvar yahtml-typeface-table
  '(("defn") ("em") ("cite") ("code") ("kbd") ("samp")
    ("strong") ("var") ("b") ("i") ("tt") ("u") ("address"))
  "Default completion table of typeface designator")
(defvar yahtml-user-typeface-table nil)
(defvar yahtml-tmp-typeface-table nil)

(defvar yahtml-single-cmd-table
  '(("hr") ("br") ("option") ("p"))
  "Default completion table of HTML single command.")
(defvar yahtml-user-single-cmd-table nil)
(defvar yahtml-tmp-single-cmd-table nil)
(defvar yahtml-last-single-cmd nil)

(defvar yahtml-prefer-upcases nil)
(cond
 (yahtml-prefer-upcases
  (setq yahtml-form-table
	(mapcar (function (lambda (list) (list (upcase (car list)))))
		yahtml-form-table))
  (setq yahtml-env-table
	(mapcar (function (lambda (list) (list (upcase (car list)))))
		yahtml-env-table))
  (setq yahtml-typeface-table
	(mapcar (function (lambda (list) (list (upcase (car list)))))
		yahtml-typeface-table))))

(defvar yahtml-struct-name-regexp
  "\\<\\(h[1-6]\\|[uod]l\\|body\\|title\\|head\\)")


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
  (make-local-variable 'YaTeX-ec) (setq YaTeX-ec "")
  (make-local-variable 'YaTeX-struct-begin) (setq YaTeX-struct-begin "<%1%2>")
  (make-local-variable 'YaTeX-struct-end) (setq YaTeX-struct-end "</%1>")
  (make-local-variable 'YaTeX-struct-name-regexp)
  (setq YaTeX-struct-name-regexp yahtml-struct-name-regexp)
  (make-local-variable 'YaTeX-prefix-map)
  (make-local-variable 'YaTeX-command-token-regexp)
  (setq YaTeX-command-token-regexp yahtml-command-regexp)
  (make-local-variable 'YaTeX-environment-indent)
  (setq YaTeX-environment-indent 0)
  (make-local-variable 'fill-prefix)
  (setq fill-prefix nil)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate yahtml-paragraph-separate
	paragraph-start  yahtml-paragraph-separate)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start "<!-- " comment-end " -->")
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
    '((1 "H1" . (lambda () (interactive) (yahtml-insert-begin-end "H1" nil)))
      (2 "H2" . (lambda () (interactive) (yahtml-insert-begin-end "H2" nil)))
      (3 "H3" . (lambda () (interactive) (yahtml-insert-begin-end "H3" nil)))
      (4 "H4" . (lambda () (interactive) (yahtml-insert-begin-end "H4" nil)))
      (5 "H5" . (lambda () (interactive) (yahtml-insert-begin-end "H5" nil)))
      (6 "H6" . (lambda () (interactive) (yahtml-insert-begin-end "H6" nil)))
      )))
  (setq yahtml-menu-map-logical (make-sparse-keymap "logical tags"))
  (yahtml-define-menu
   yahtml-menu-map-logical
   (nreverse
    '((em	"Embolden" .
	  (lambda () (interactive) (yahtml-insert-tag "EM")))
      (defn	"Define a word" .
	(lambda () (interactive) (yahtml-insert-tag "DEFN")))
      (cite	"Citation" .
	(lambda () (interactive) (yahtml-insert-tag "CITE")))
      (code	"Code" .
	(lambda () (interactive) (yahtml-insert-tag "CODE")))
      (kbd	"Keyboard" .
	(lambda () (interactive) (yahtml-insert-tag "KBD")))
      (samp	"Sample display" .
	(lambda () (interactive) (yahtml-insert-tag "SAMP")))
      (strong	"Strong" .
	(lambda () (interactive) (yahtml-insert-tag "STRONG")))
      (VAR	"Variable notation" .
	(lambda () (interactive) (yahtml-insert-tag "VAR")))
      )))
  (setq yahtml-menu-map-typeface (make-sparse-keymap "typeface tags"))
  (yahtml-define-menu
   yahtml-menu-map-typeface
   (nreverse
    '((b	"Bold" .
	  (lambda () (interactive) (yahtml-insert-tag "B")))
      (i	"Italic" .
	(lambda () (interactive) (yahtml-insert-tag "I")))
      (tt	"Typewriter" .
	(lambda () (interactive) (yahtml-insert-tag "TT")))
      (u	"Underlined" .
	(lambda () (interactive) (yahtml-insert-tag "U")))
      )))
  (setq yahtml-menu-map-listing (make-sparse-keymap "listing"))
  (yahtml-define-menu
   yahtml-menu-map-listing
   (nreverse
    '((ul	"Unnumbered" .
		(lambda () (interactive) (yahtml-insert-begin-end "UL" nil)))
      (ol	"Numbered" .
		(lambda () (interactive) (yahtml-insert-begin-end "OL" nil)))
      (dl	"Description" .
		(lambda () (interactive) (yahtml-insert-begin-end "DL" nil)))
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
(defun yahtml-insert-begend (&optional region)
  "Insert <cmd> ... </cmd>."
  (interactive "P")
  (let ((cmd (YaTeX-cplread-with-learning
	      (format "Environment(default %s): " yahtml-last-begend)
	      'yahtml-env-table 'yahtml-user-env-table 'yahtml-tmp-env-table))
	(bolp (bolp)))
    (if (string< "" cmd) (setq yahtml-last-begend cmd))
    (setq cmd yahtml-last-begend)
    (if region
	(let ((beg (region-beginning))
	      (end (region-end))
	      (addin (yahtml-addin cmd)))
	  (goto-char end)
	  (insert (format "</%s>%s" cmd (if bolp "\n" "")))
	  (goto-char beg)
	  (insert (format "<%s%s>%s" cmd addin (if bolp "\n" ""))))
      (insert (format "<%s%s" cmd (yahtml-addin cmd)))
      (if bolp (progn (insert (format ">\n</%s>\n" cmd cmd))
			(forward-line -1))
	(insert ">")
	(save-excursion (insert (format "</%s>" cmd)))))))

(defun yahtml-insert-begend-region ()
  "Call yahtml-insert-begend in the region mode."
  (interactive)
  (yahtml-insert-begend t))


(defun yahtml-insert-form (&optional form)
  "Insert <FORM option=\"argument\">."
   (interactive)
   (or form
       (setq form
	     (YaTeX-cplread-with-learning
	      "Form: "
	       'yahtml-form-table 'yahtml-user-form-table
	       'yahtml-tmp-form-table)))
   (let ((p (point)) q)
     (insert (format "<%s%s>" form (yahtml-addin (downcase form))))
     ;;(indent-relative-maybe)
     (if (cdr (assoc form yahtml-form-table))
	 (save-excursion (insert (format "</%s>" form))))
     (if (search-backward "\"\"" p t) (forward-char 1))))

(defun yahtml-addin (form)
  "Check add-in function's existence and call it if exists."
   (let ((addin (concat "yahtml:" form)))
     (if (and (intern-soft addin) (fboundp (intern-soft addin)))
	 (concat " " (funcall (intern addin)))
       "")))

(defun yahtml:a ()
  "Add-in function for <a>"
;  (or yahtml-urls (yahtml-collect-url-history))
;  (concat "href=\""
;	  (completing-read "href: " yahtml-urls)
;	  "\"")
  (concat "href=\"" (read-file-name "href: " "" nil nil "") "\"")
)

(defun yahtml:img ()
  "Add-in function for <img>"
  (or yahtml-urls (yahtml-collect-url-history))
  (let ((src (read-file-name "src: " "" nil t ""))
	(alg (completing-read "align: " '(("top") ("middle") ("bottom")))))
    (concat "src=\"" src "\""
	    (if (string< "" alg) (concat " align=\"" alg "\"")))))

(defun yahtml:form ()
  "Add-in function `form' input format"
  (concat
   " method=" (completing-read "Method: " '(("POST") ("GET")) nil t)
   " action=\"" (read-string "Action: ") "\""
   ))

(defun yahtml:select ()
  "Add-in function for `select' input format"
  (setq yahtml-last-single-cmd "option")
  (concat " name=\"" (read-string "name: ") "\""))

(defvar yahtml-input-types
  '(("text") ("password") ("checkbox") ("radio") ("submit")
    ("reset") ("image") ("hidden")))

(defun yahtml:input ()
  "Add-in function for `input' form"
  (let (name type value checked (size "") (maxlength ""))
    (setq name (read-string "name: ")
	  type (completing-read "type (default=text): "
				yahtml-input-types nil t)
	  value (read-string "value: "))
    (if (string-match "text\\|password\\|^$" type)
	(setq size (read-string "size: ")
	      maxlength (read-string "maxlength: ")))
    (concat
     "name=\"" name "\""
     (if (string< "" type) (concat " type=\"" type "\""))
     (if (string< "" value) (concat " value=\"" value "\""))
     (if (string< "" size) (concat " size=\"" size "\""))
     (if (string< "" maxlength) (concat " maxlength=\"" maxlength "\""))
    )))
	

(defun yahtml-insert-begin-end (env &optional region-mode)
  "Insert <ENV> \\n </ENV> by calling YaTeX-insert-begin-end."
  (interactive "sEnv: ")
  (setq env (funcall (if yahtml-prefer-upcases 'upcase 'downcase) env))
  (YaTeX-insert-begin-end env region-mode))

(defun yahtml-insert-tag (tag)
  "Insert <TAG> </TAG> and put cursor inside of them."
  (interactive
   (list
    (YaTeX-cplread-with-learning
     "Tag: "
     'yahtml-typeface-table 'yahtml-user-typeface-table
     'yahtml-tmp-typeface-table)))
  (setq tag (funcall (if yahtml-prefer-upcases 'upcase 'downcase) tag))
  (insert (format "<%s> " tag))
  (save-excursion (insert (format "</%s>" tag))))

(defun yahtml-insert-single (cmd)
  "Insert <CMD>."
  (interactive
   (list (YaTeX-cplread-with-learning
	  (format "Command%s: "
		  (if yahtml-last-single-cmd
		      (concat "(default " yahtml-last-single-cmd ")") ""))
	  'yahtml-single-cmd-table 'yahtml-user-single-cmd-table
	  'yahtml-tmp-single-cmd-table)))
  (setq cmd (funcall (if yahtml-prefer-upcases 'upcase 'downcase) cmd))
  (if (string< "" cmd) (setq yahtml-last-single-cmd cmd))
  (insert (format "<%s>" yahtml-last-single-cmd)))

;;; ---------- Jump ----------
(defun yahtml-on-href-p ()
  "Check if point is on href clause."
  (let ((p (point)) cmd)
    (save-excursion
      (or (bobp) (skip-chars-backward "^ \t"))
      (and (looking-at "href\\s *=\\s *\"?\\([^\"]+\\)\"?")
	   (< p (match-end 0))
	   (YaTeX-match-string 1)))))

(defun yahtml-goto-corresponding-href (&optional other)
  "Go to corresponding name."
  (let ((href (yahtml-on-href-p)) file name)
    (if href
	(cond
	 ((string-match "^http:" href)
	  (message "Sorry, jump across http is not supported."))
	 (t (setq file (substring href 0 (string-match "#" href)))
	    (if (string-match "#" href)
		(setq name (substring href (1+ (string-match "#" href)))))
	    (if (string< "" file)
		(progn
		  (if (string-match "/$" file)
		      (setq file (concat file "index.html")))
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
  (let ((p (point)) cmd)
    (save-excursion
      (if p (goto-char p))
      (if (= (char-after (point)) ?<) (forward-char 1))
      (if (and (re-search-backward "<" nil t)
	       (looking-at
		(concat "<\\(/?" yahtml-command-regexp "\\)\\b"))
	       (condition-case nil
		   (forward-list 1))
	       (< p (point)))
	  (YaTeX-match-string 1)))))

(defun yahtml-goto-corresponding-begend ()
  (let ((cmd (yahtml-on-begend-p)))
  (if cmd
      (progn
	(if (= (aref cmd 0) ?/)		;on </cmd> line
	    (re-search-backward (format "<%s" (substring cmd 1)))
	  (re-search-forward (format "</%s" cmd)))
	(if (match-beginning 0) (goto-char (match-beginning 0)))))))

(defun yahtml-goto-corresponding-* (&optional other)
  "Go to corresponding object."
  (interactive)
  (cond
   ((yahtml-goto-corresponding-href other))
   ((yahtml-goto-corresponding-begend other))
   ))

(defun yahtml-goto-corresponding-*-other-window ()
  "Go to corresponding object."
  (interactive)
  (yahtml-goto-corresponding-* t))

;;; ---------- commenting ----------
(defun yahtml-comment-region (beg end)
  (interactive "r")
  (comment-region beg end nil))

(defun yahtml-uncomment-region (beg end)
  (interactive "r")
  (comment-region beg end '(4)))



;;; ---------- ----------
;;; ---------- ----------
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
		     (1- (match-beginning 0)))))))

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

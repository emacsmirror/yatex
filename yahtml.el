;;; -*- Emacs-Lisp -*-
;;; (c ) 1994 by HIROSE Yuuji [yuuji@ae.keio.ac.jp, pcs39334@ascii-net.or.jp]
;;; Last modified Mon Apr 24 22:52:17 1995 on pajero
;;; This is PURELY tentative.
;;; $Id$

;;;[Commentary]
;;;
;;; It is assumed you are already familiar with YaTeX.  The following
;;; completing featureas are available: ([prefix] means `C-c' by default)
;;;
;;;  * [prefix] b X	Complete environments such as `H1' which
;;;			normally requires newline.
;;;  * [prefix] s	Complete declarative notations such as
;;;			`<a href="foo.html"> .... </a>'
;;;  * [prefix] l	Complete typeface-changing commands such as
;;;			`<i> ... </i>' or `<samp> ... </samp>'
;;;  * menu-bar yahtml	Complete all by selecting a menu item (Though I
;;;			hate menu, this is most useful)
;;;
;;; NOTE!  This program is  truly  tentative.  If  you find some  bright
;;; future with this, please send me a mail to drive me to maintain this :)


(require 'yatex)
(defvar yahtml-prefix-map (copy-keymap YaTeX-prefix-map))
(defvar yahtml-mode-map nil
  "Keymap used in yahtml-mode.")
(if yahtml-mode-map nil
  (setq yahtml-mode-map (make-sparse-keymap))
  (define-key yahtml-mode-map YaTeX-prefix yahtml-prefix-map)
  (define-key yahtml-mode-map "\M-\C-@" 'YaTeX-mark-environment)
  (define-key yahtml-mode-map "\M-\C-a" 'YaTeX-beginning-of-environment)
  (define-key yahtml-mode-map "\M-\C-e" 'YaTeX-end-of-environment))

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

;;; Completion tables for `form'
(defvar yahtml-form-table '(("a") ("form")))
(defvar yahtml-user-form-table nil)
(defvar yahtml-tmp-form-table nil)

(defvar yahtml-env-table
  '(("html") ("head") ("title") ("body") ("dl")
    ("h1") ("h2") ("h3") ("h4") ("h5") ("h6")))

;;; Completion tables for typeface designator
(defvar yahtml-typeface-table
  '(("defn") ("em") ("cite") ("code") ("kbd") ("samp")
    ("strong") ("var") ("b") ("i") ("tt") ("u"))
  "Default completion table of typeface designator")
(defvar yahtml-user-typeface-table nil)
(defvar yahtml-tmp-typeface-table nil)

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

(defun yahtml-mode ()
  (interactive)
  (yatex-mode)
  (setq major-mode 'yahtml-mode
	mode-name "yahtml")
  (make-local-variable 'YaTeX-ec) (setq YaTeX-ec "")
  (make-local-variable 'YaTeX-struct-begin) (setq YaTeX-struct-begin "<%1>")
  (make-local-variable 'YaTeX-struct-end) (setq YaTeX-struct-end "</%1>")
  (mapcar 'make-local-variable
	  '(env-table user-env-table tmp-env-table))
  (setq env-table yahtml-env-table)
  (mapcar 'make-local-variable
	  '(singlecmd-table user-singlecmd-table tmp-singlecmd-table))
  (make-local-variable 'YaTeX-struct-name-regexp)
  (setq YaTeX-struct-name-regexp "[^/]+")
  (make-local-variable 'YaTeX-prefix-map)
  (make-local-variable 'YaTeX-command-token-regexp)
  (setq YaTeX-command-token-regexp yahtml-command-regexp)
  (setq YaTeX-prefix-map yahtml-prefix-map)
  (set-syntax-table yahtml-syntax-table)
  (use-local-map yahtml-mode-map)
  (YaTeX-define-key "s" 'yahtml-insert-form)
  (YaTeX-define-key "l" 'yahtml-insert-tag)
  (if YaTeX-no-begend-shortcut nil
    (YaTeX-define-begend-key "bh" "HTML")
    (YaTeX-define-begend-key "bH" "HEAD")
    (YaTeX-define-begend-key "bt" "TITLE")
    (YaTeX-define-begend-key "bb" "BODY")
    (YaTeX-define-begend-key "bd" "DL")
    (YaTeX-define-begend-key "b1" "H1")
    (YaTeX-define-begend-key "b2" "H2")
    (YaTeX-define-begend-key "b3" "H3"))
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

(defun yahtml-insert-form (&optional form)
  "Insert <FORM option=\"argument\">  </FORM>."
   (interactive)
   (or form
       (setq form
	     (YaTeX-cplread-with-learning
	      "Form: "
	       'yahtml-form-table 'yahtml-user-form-table
	       'yahtml-tmp-form-table)))
   (let ((p (point)))
     (insert (format "<%s%s>\n" form (yahtml-addin (downcase form))))
     (indent-relative-maybe)
     (save-excursion (insert (format "</%s>" form)))
     (if (search-backward "\"\"" p t) (forward-char 1))))

(defun yahtml-addin (form)
  "Check add-in function's existence and call it if exists."
   (let ((addin (concat "yahtml::" form)))
     (if (and (intern-soft addin) (fboundp (intern-soft addin)))
	 (concat " " (funcall (intern addin)))
       "")))

(defun yahtml::a ()
  "Add-in function for <a>"
  (or yahtml-urls (yahtml-collect-url-history))
  (concat "href=\""
	  (completing-read "href: " yahtml-urls)
	  "\""))

(defun yahtml-insert-begin-end (env &optional region-mode)
  "Insert <ENV> \\n </ENV> by calling YaTeX-insert-begin-end."
  (interactive "sEnv: ")
  (setq env (funcall (if yahtml-prefer-upcases 'upcase 'downcase) env))
  (YaTeX-insert-begin-end env region-mode))

(defun yahtml-insert-tag (&optional tag)
  "Insert <TAG> </TAG> and put cursor inside of them."
  (interactive)
  (or tag
      (setq tag
	    (YaTeX-cplread-with-learning
	     "Tag: "
	     'yahtml-typeface-table 'yahtml-user-typeface-table
	     'yahtml-tmp-typeface-table)))
  (setq tag (funcall (if yahtml-prefer-upcases 'upcase 'downcase) tag))
  (insert (format "<%s> " tag))
  (save-excursion (insert (format "</%s>" tag))))

(provide 'yahtml)

; Local variables:
; fill-prefix: ";;; "
; paragraph-start: "^$\\|\\|;;;$"
; paragraph-separate: "^$\\|\\|;;;$"
; End:

;;; -*- Emacs-Lisp -*-
;;; YaTeX package manager
;;; yatexpkg.el
;;; (c )2003 by HIROSE, Yuuji [yuuji@yatex.org]
;;; Last modified Fri May  2 20:13:49 2003 on firestorm
;;; $Id$

(defvar YaTeX-package-alist-default
  '(("version"	(env "comment"))	;by tsuchiya@pine.kuee.kyoto-u.ac.jp

    ("plext"	(section "bou"))	;by yas.axis@ma.mni.ne.jp

    ("url"	(section "url"))	;by fujieda@jaist.ac.jp

    ("fancybox"	(section "shadowbox" "doublebox" "ovalbox" "Ovalbox"))
    ("pifont"	(section "ding"))
    ("longtable" (env "longtable"))
    ("ascmac"	(env "screen" "boxnote" "shadebox" "itembox")
		(maketitle "return" "Return" "yen")
     		(section "keytop"))
    ("bm"	(section "bm"))		;by aoyama@le.chiba-u.ac.jp

    ("graphicx"	(section "includegraphics"))
    ("alltt"	(env "alltt"))
    ("misc"	(section "verbfile" "listing"))
    ("eclbkbox"	(env "breakbox")))
  "Default package vs. macro list")

(defvar YaTeX-package-alist-private nil
  "*User defined package vs. macro list. See also YaTeX-package-alist-default")

(defun YaTeX-package-lookup (macro &optional type)
  "Look up a package which contains a definition of MACRO.
Optional second argument TYPE limits the macro type.
TYPE is a symbol, one of 'env, 'section, 'maketitle."
  (let ((list (append YaTeX-package-alist-private YaTeX-package-alist-default))
	element x pkg pkglist r)
    (while list
      (setq element (car list)
	    pkg (car element)
	    element (cdr element))
      (if (setq r (catch 'found
		    (while element
		      (setq x (car element))
		      (and (YaTeX-member macro (cdr x))
			   (or (null type)
			       (eq type (car x)))
			   (throw 'found (car x)))	;car x is type
		      (setq element (cdr element)))))
	  (setq pkglist (cons (cons pkg r) pkglist)))
      (setq list (cdr list)))
    pkglist))

(defvar YaTeX-package-resolved-list nil
  "List of macros whose package is confirmed to be loaded.")

(defun YaTeX-package-auto-usepackage (macro type)
  "(Semi)Automatically add the \\usepackage line to main-file.
Search the usepackage for MACRO of the TYPE."
  (let ((cb (current-buffer))
	(wc (current-window-configuration))
	(usepackage (concat YaTeX-ec "usepackage"))
	(pkglist (YaTeX-package-lookup macro))
	(usepkgrx (concat
		   YaTeX-ec-regexp
		   "\\(usepackage\\|include\\)\\b"))
	(register '(lambda () (set-buffer cb)
		     (set (make-local-variable 'YaTeX-package-resolved-list)
			  (cons macro YaTeX-package-resolved-list))))
	(begdoc (concat YaTeX-ec "begin{document}"))
	pb pkg mb0)
    (if (or (YaTeX-member macro YaTeX-package-resolved-list)
	    (null pkglist))
	nil				;nothing to do
      ;; Search `usepackage' into main-file
      (YaTeX-visit-main t)		;set buffer to parent file
      (setq pb (current-buffer))
      (save-excursion
	(save-restriction
	  (if (catch 'found
		(goto-char (point-min))
		(YaTeX-search-active-forward	;if search fails, goto eob
		 begdoc YaTeX-comment-prefix nil 1)
		(while (YaTeX-re-search-active-backward
			usepkgrx YaTeX-comment-prefix nil t)
		  (setq mb0 (match-beginning 0))
		  (skip-chars-forward "^{")
		  (let ((pl pkglist))
		    (while pl		;(car pl)'s car is package, cdr is type
		      (if (looking-at (regexp-quote (car (car pl))))
			  (throw 'found t))
		      (setq pl (cdr pl)))
		    (goto-char mb0))))
	      ;;corresponding \usepackage found
	      (funcall register)
	    ;; not found, insert it.
	    (if (y-or-n-p
		 (format "`%s' requires package. Put \\usepackage now?" macro))
		(progn
		  (setq pkg
			(completing-read
			 "Load which package?(TAB for list): "
			 pkglist))
		  (set-buffer pb)
		  (goto-char (point-min))
		  (if (YaTeX-re-search-active-forward
		       (concat YaTeX-ec-regexp
			       "document\\(style\\|class\\){")
		       YaTeX-comment-prefix nil t)
		      (forward-line 1))
		  (if (YaTeX-search-active-forward
		       begdoc YaTeX-comment-prefix nil t)
		      (goto-char (match-beginning 0)))
		  (insert
		   usepackage
		   (format "{%s}\t%% required for `\\%s' (yatex added)\n"
			   pkg macro))
		  (funcall register))
	      (message "Don't forget to put \\usepackage{%s} yourself later"
		       (car (car pkglist)))) ;doing car car is negligence...
    ))))))

;;; -*- Emacs-Lisp -*-
;;; YaTeX facilities for Emacs 19
;;; (c )1994 by HIROSE Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Mon Nov 21 21:52:16 1994 on figaro
;;; $Id$

;;; とりあえず hilit19 を使っている時に色が付くようにして
;;; メニューバーでごにょごにょできるようにしただけ。
;;; いったい誰がメニューバー使ってLaTeXソース書くんだろうか?
;;; まあいいや練習練習。後ろの方にちょっとコメントあり。

(require 'yatex)

(defun YaTeX-19-define-sub-menu (map vec &rest bindings)
  "Define sub-menu-item in MAP at vector VEC as BINDINGS.
BINDINGS is a form with optional length: (symbol title binding).
When you defined menu-bar keymap such like:
  (define-key foo-map [menu-bar foo] (make-sparse-keymap \"foo menu\"))
and you want to define sub menu for `foo menu' as followings.
  foo ->  menu1  (calling function `func1')
          menu2  (doing interactive call `(func2 ...)'
Call this function like this:
  (YaTeX-19-define-sub-menu foo-map [menu-bar foo]
   '(m1 \"Function 1\" func1)
   '(m2 \"Function 2\" (lambda () (interactive) (func2 ...))))
where
  `m1' and `m2' are the keymap symbol for sub-menu of `[menu-bar foo].
  `Funtion 1' and `Function 2' are the title strings for sub-menu.
"
  (let ((i 0) (vec2 (make-vector (1+ (length vec)) nil)))
    (while (< i (length vec))
      (aset vec2 i (aref vec i))
      (setq i (1+ i)))
    (setq bindings (reverse bindings))
    (mapcar
     (function
      (lambda (bind)
	(aset vec2 (1- (length vec2)) (car bind)) ;set menu-symbol
	(define-key map vec2
	  (cons (car (cdr bind))
		(car (cdr (cdr bind)))))))
     bindings)))

;; Menu for Typeset relating processes ----------------------------------------
(define-key YaTeX-mode-map [menu-bar yatex]
  (cons "YaTeX" (make-sparse-keymap "YaTeX")))
(define-key YaTeX-mode-map [menu-bar yatex process]
  (cons "Process" (make-sparse-keymap "Process")))
(YaTeX-19-define-sub-menu
 YaTeX-mode-map [menu-bar yatex process]
 '(buffer "LaTeX" (lambda () (interactive) (YaTeX-typeset-menu nil ?j)))
 '(kill "Kill LaTeX" (lambda () (interactive) (YaTeX-typeset-menu nil ?k)))
 '(bibtex "BibTeX" (lambda () (interactive) (YaTeX-typeset-menu nil ?b)))
 '(makeindex "makeindex" (lambda () (interactive) (YaTeX-typeset-menu nil ?i)))
 '(preview "Preview" (lambda () (interactive) (YaTeX-typeset-menu nil ?p)))
 '(lpr "lpr" (lambda () (interactive) (YaTeX-typeset-menu nil ?l)))
 '(lpq "lpq" (lambda () (interactive) (YaTeX-typeset-menu nil ?q)))
)

;; Help for LaTeX ------------------------------------------------------------
(YaTeX-19-define-sub-menu
 YaTeX-mode-map [menu-bar yatex]
 '(sephelp	"--")
 '(help		"Help on LaTeX commands" YaTeX-help)
 '(apropos	"Apropos on LaTeX commands" YaTeX-apropos))

;; Switch modes --------------------------------------------------------------
(define-key YaTeX-mode-map [menu-bar yatex switch]
  (cons "Switching YaTeX's modes" (make-sparse-keymap "modes")))
(or YaTeX-auto-math-mode
    (define-key YaTeX-mode-map [menu-bar yatex switch math]
      '("Toggle math mode" . (lambda () (interactive)
			       (YaTeX-switch-mode-menu nil ?t)))))
(define-key YaTeX-mode-map [menu-bar yatex switch mod]
  '("Toggle modify mode" . (lambda () (interactive)
			     (YaTeX-switch-mode-menu nil ?m))))

;; % menu --------------------------------------------------------------------
(define-key YaTeX-mode-map [menu-bar yatex percent]
  (cons "Edit %# notation" (make-sparse-keymap "Edit %# notation")))
(YaTeX-19-define-sub-menu
 YaTeX-mode-map [menu-bar yatex percent]
 '(!		"Change LaTeX typesetter(%#!)"
	(lambda () (interactive) (YaTeX-%-menu nil nil ?!)))
 '(begend	"Set %#BEGIN-%#END on region"
	(lambda () (interactive) (YaTeX-%-menu nil nil ?b)))
 '(lpr 		"Change LPR format"
	(lambda () (interactive) (YaTeX-%-menu nil nil ?l))))

;; What position -------------------------------------------------------------
(YaTeX-19-define-sub-menu
 YaTeX-mode-map [menu-bar yatex]
 '(what "What column in tabular" YaTeX-what-column))

;; Jump cursor ---------------------------------------------------------------
(define-key YaTeX-mode-map [menu-bar yatex jump]
  (cons "Jump cursor"
	 (make-sparse-keymap "Jump cursor")))
(YaTeX-19-define-sub-menu
 YaTeX-mode-map [menu-bar yatex jump]
 '(corres     "Goto corersponding position" YaTeX-goto-corresponding-*)
 '(main	      "Visit main source" (lambda () (interactive) (YaTeX-visit-main)))
 '(main-other "Visit main source other window" YaTeX-visit-main-other-window)
 )

;; ===========================================================================
(define-key YaTeX-mode-map [menu-bar yatex sepcom]
  '("---" . nil))

;; Comment/Uncomment ---------------------------------------------------------
(YaTeX-19-define-sub-menu
 YaTeX-mode-map [menu-bar yatex]
 '(comment	"Comment region or environment" YaTeX-comment-region)
 '(uncomment	"Unomment region or environment" YaTeX-uncomment-region)
 '(commentp	"Comment paragraph" YaTeX-comment-paragraph)
 '(uncommentp	"Uncomment paragraph" YaTeX-uncomment-paragraph)
 '(sepcom	"--"	nil)
)


;; ===========================================================================
;; Change/Kill/Fill
(YaTeX-19-define-sub-menu
 YaTeX-mode-map [menu-bar yatex]
 '(change	"Change macros"	YaTeX-change-*)
 '(kill 	"Kill macros"	YaTeX-kill-*)
 '(fillitem	"Fill \\item"	YaTeX-fill-item)
 '(newline	"Newline"	YaTeX-intelligent-newline)
 '(sepchg	"--" nil)
)

;; Menu for completions ------------------------------------------------------


;;;(YaTeX-19-define-sub-menu
;;; YaTeX-mode-map [menu-bar yatex]
;;; '(secr "Section-type command on region" YaTeX-make-section-region)
;;; '(sec  "Section-type command" YaTeX-make-section))

(define-key YaTeX-mode-map [menu-bar yatex sectionr]
  (cons "Section-type region(long name)"
	(make-sparse-keymap "Enclose region with section-type macro")))
(define-key YaTeX-mode-map [menu-bar yatex section]
  (cons "Section-type(long name)"
	(make-sparse-keymap "Section-type macro")))
(let ((sorted-section
       (sort
	(delq nil
	      (mapcar (function (lambda (s)
				  (if (> (length (car s)) 5)
				      (car s))))
		      (append section-table user-section-table)))
	'string<)))
  (apply 'YaTeX-19-define-sub-menu
	 YaTeX-mode-map [menu-bar yatex section]
	 (mapcar (function (lambda (secname)
			     (list (intern secname) secname
				   (list 'lambda ()
					 (list 'interactive)
					 (list 'YaTeX-make-section
					       nil nil nil secname)))))
		 sorted-section))
  (apply 'YaTeX-19-define-sub-menu
	 YaTeX-mode-map [menu-bar yatex sectionr]
	 (mapcar (function (lambda (secname)
			     (list (intern secname) secname
				   (list 'lambda ()
					 (list 'interactive)
					 (list 'YaTeX-make-section
					       nil
					       (list 'region-beginning)
					       (list 'region-end)
					       secname)))))
		 sorted-section)))

(define-key YaTeX-mode-map [menu-bar yatex envr]
  (cons "Environment region" (make-sparse-keymap "Environment region")))
(define-key YaTeX-mode-map [menu-bar yatex env]
  (cons "Environment" (make-sparse-keymap "Environment")))
(let (prev envname)
  (mapcar
   (function
    (lambda (envalist)
      (setq envname (car envalist))
      (define-key-after
	(lookup-key YaTeX-mode-map [menu-bar yatex env])
	(vector (intern envname))
	(cons envname
	      (list 'lambda () (list 'interactive)
		    (list 'YaTeX-insert-begin-end
			  envname nil)))
	prev)
      (define-key-after
	(lookup-key YaTeX-mode-map [menu-bar yatex envr])
	(vector (intern envname))
	(cons envname
	      (list 'lambda () (list 'interactive)
		    (list 'YaTeX-insert-begin-end
			  envname t)))
	prev)
      (setq prev (intern envname))))
   (sort (append env-table user-env-table)
	 '(lambda (x y) (string< (car x) (car y))))))

;; Highlightening
;; ローカルなマクロを読み込んだ後 redraw すると
;; ローカルマクロを keyword として光らせる(keywordじゃまずいかな…)。
(defvar YaTeX-hilit-pattern-adjustment
  (list
   ;;\def が define なんだから new* も define でしょう。
   '("\\\\\\(re\\)?new\\(environment\\|command\\){" "}" define)
   '("\\\\new\\(length\\|theorem\\|counter\\){" "}" define)
   ;;セクションコマンドが単なるキーワードってことはないでしょう。
   (list
    (concat "\\\\\\(" YaTeX-sectioning-regexp "\\){") "}"
    'sectioning)
   ;;eqnarray などの数式環境が入ってないみたい…
   '("\\\\begin{\\(eqnarray\\*?\\)\\|\\(equation\\*?\\)}"
     "\\\\end{\\(eqnarray\\*?\\)\\|\\(equation\\*?\\)}"
     formula))
  "Adjustment for hilit19's LaTeX hilit pattern.")
(defvar YaTeX-hilit-sectioning-face
  '(yellow/dodgerblue yellow/cornflowerblue))
(defvar YaTeX-hilit-singlecmd-face
  '(slateblue2 aquamarine))
(defun YaTeX-19-collect-macro ()
  (cond
   ((and (featurep 'hilit19) (fboundp 'hilit-translate))
    (let ((get-face
	   (function
	    (lambda (table)
	      (cond
	       ((eq hilit-background-mode 'light) (car table))
	       ((eq hilit-background-mode 'dark) (car (cdr table)))
	       (t nil))))))
      (hilit-translate
       sectioning (funcall get-face YaTeX-hilit-sectioning-face)
       macro (funcall get-face YaTeX-hilit-singlecmd-face)))
    (setq hilit-patterns-alist		;Remove at first.
	  (delq 'yatex-mode hilit-patterns-alist)
	  hilit-patterns-alist
	  (cons
	   (cons 'yatex-mode
		 (append
		  YaTeX-hilit-pattern-adjustment
		  (cdr (assq 'latex-mode hilit-patterns-alist))
		  (list
		   (list
		    (concat "\\\\\\("
			    (mapconcat
			     (function (lambda (s) (regexp-quote (car s))))
			     (append user-section-table tmp-section-table)
			     "\\|")
			    "\\){")
		    "}" 'keyword)
		   (list
		    (concat "\\\\\\("
			    (mapconcat
			     (function (lambda (s) (regexp-quote (car s))))
			     (append user-singlecmd-table tmp-singlecmd-table)
			     "\\|")
			    "\\)")
		    0 'macro))))
	   hilit-patterns-alist)))))
(YaTeX-19-collect-macro)
(defun YaTeX-hilit-recenter (arg)
  "Collect current local macro and hilit-recenter."
  (interactive "P")
  (YaTeX-19-collect-macro)
  (hilit-recenter arg))
(if (fboundp 'hilit-recenter)		;Replace hilit-recenter with
    (mapcar (function (lambda (key)	;YaTeX-hilit-recenter in yatex-mode
			(define-key YaTeX-mode-map key 'YaTeX-hilit-recenter)))
	    (where-is-internal 'hilit-recenter)))

;;; reverseVideo にして hilit-background-mode を 'dark
;;; にしている人は数式などが暗くなりすぎて見づらいかもしれない。
;;; 次のコードを hilit19 をロードしている場所の直後に置くとちょっ
;;; とはまし。
;;;  (if (eq hilit-background-mode 'dark)
;;;      (hilit-translate
;;;       string 'mediumspringgreen
;;;       formula 'khaki
;;;       label 'yellow-underlined))

(provide 'yatex19)

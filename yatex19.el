;;; -*- Emacs-Lisp -*-
;;; YaTeX facilities for Emacs 19
;;; (c )1994-1995 by HIROSE Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Wed Dec 20 22:32:21 1995 on supra
;;; $Id$

;;; とりあえず hilit19 を使っている時に色が付くようにして
;;; メニューバーでごにょごにょできるようにしただけ。
;;; いったい誰がメニューバー使ってLaTeXソース書くんだろうか?
;;; まあいいや練習練習。後ろの方にちょっとコメントあり。
;;; 真中辺にあるけど、hilit19.el 対応の方は結構本気。

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

;; Document hierarchy  ------------------------------------------------------
(YaTeX-19-define-sub-menu
 YaTeX-mode-map [menu-bar yatex]
 '(hier "Display document hierarchy" YaTeX-display-hierarchy-directly))

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

;; Other key bindings for window-system
;(YaTeX-define-key [?\C- ] 'YaTeX-do-completion)
(define-key YaTeX-mode-map [?\M-\C- ] 'YaTeX-mark-environment)

;; Highlightening
;; メニューに比べてこっちは結構本気でやってます。
;; だって文書構造がとっても分かり易いんだもん。
;; みんなも hilit19.el を使おう!
;;
;; さて、まずは対応する {} をピカピカ範囲とするような関数を作る。
;; これは hilit-LaTeX.el を参考にした。でも、ちゃんと section 型コマンドの
;; 引数を数えて正しい位置までピカピカさせるよ～ん!

(defun YaTeX-19-region-section-type (pattern)
  "Return list of starting and end point of section-type commands of PATTERN."
  (if (re-search-forward pattern nil t)
      (let ((m0 (match-beginning 0)) cmd (argc 1))
	(setq cmd (substring (YaTeX-match-string 0) 1 -1)
	      argc (or (car (cdr (YaTeX-lookup-table cmd 'section))) argc))
	(cons m0
	      (progn (skip-chars-backward "^{") (forward-char -2)
		     (while (> argc 0)
		       (skip-chars-forward "^{")
		       (forward-list 1)
		       (setq argc (1- argc)))
		     (point))))))

(defun YaTeX-19-region-large-type (pattern)
  "Return list of large-type contents.
Assumes PATTERN begins with `{'."
  (if (re-search-forward pattern nil t)
      (let ((m0 (match-beginning 0)))
	(goto-char m0)
	(skip-chars-forward "^ \t\n")
	(skip-chars-forward " \t\n")
	(cons (point)
	      (progn (goto-char m0) (forward-list 1)
		     (1- (point)))))))

;; 些細なことだが % の前の文字もピカリとさせてしまうようで… >hilit19
;; ↓この関数は下の hilit-set-mode-patterns の "[^\\]\\(%\\).*$" に
;; 依存している
(defun YaTeX-19-region-comment (pattern)
  "Return list of comment start and end point."
  (if (re-search-forward pattern nil t)
      (cons (match-beginning 2) (match-end 0))))

;;(make-face 'tt)
;;(set-face-font 'tt "-schumacher-clean-medium-r-normal--*-*-*-*-*-*-*-*")
;;(hilit-translate 'tt "white")

(defvar YaTeX-hilit-patterns-alist
  '(
    ;; comments
    (YaTeX-19-region-comment "\\([^\\]\\|^\\)\\(%\\).*$" comment)

    (YaTeX-19-region-section-type "\\\\footnote\\(mark\\|text\\)?{" keyword)
    ("\\\\[a-z]+box" 0 keyword)
    (YaTeX-19-region-section-type "\\\\\\(v\\|h\\)space\\(\*\\)?{" keyword)

    ;; (re-)define new commands/environments/counters
    (YaTeX-19-region-section-type
     "\\\\\\(re\\)?new\\(environment\\|command\\|theorem\\){" defun)
    (YaTeX-19-region-section-type
     "\\\\\\(re\\)?new\\(length\\|counter\\){" define)

    ;; various declarations/definitions
    (YaTeX-19-region-section-type
     "\\\\\\(set\\|setto\\|addto\\)\\(length\\|width\\|counter\\){"
     define)
    (YaTeX-19-region-section-type
     "\\\\\\(title\\|author\\|date\\|thanks\\){" define)

    ("\\\\documentstyle\\(\\[.*\\]\\)?{" "}" decl)
    ("\\\\\\(begin\\|end\\|nofiles\\|includeonly\\){" "}" decl)
    ("\\\\\\(raggedright\\|makeindex\\|makeglossary\\|maketitle\\)\\b" 0 decl)
    ("\\\\\\(pagestyle\\|thispagestyle\\|pagenumbering\\){" "}" decl)
    ("\\\\\\(normalsize\\|small\\|footnotesize\\|scriptsize\\|tiny\\|large\\|Large\\|LARGE\\|huge\\|Huge\\)\\b" 0 decl)
    ("\\\\\\(appendix\\|tableofcontents\\|listoffigures\\|listoftables\\)\\b"
     0 decl)
    ("\\\\\\(bf\\|em\\|it\\|rm\\|sf\\|sl\\|ss\\|tt\\)\\b" 0 decl)

    ;; label-like things
    ;;this should be customized by YaTeX-item-regexp
    ("\\\\\\(sub\\)*item\\b\\(\\[[^]]*\\]\\)?" 0 label)
    (YaTeX-19-region-section-type
     "\\\\caption\\(\\[[^]]*\\]\\)?{" label)

    ;; formulas
    ("[^\\]\\\\("  "\\\\)" formula)                   ; \( \)
    ("[^\\]\\\\\\[" "\\\\\\]" formula)                ; \[ \]
    ("\\\\begin{\\(eqn\\|equation\\|x?x?align\\|split\\|multiline\\|gather\\)"
     "\\\\end{\\(eqn\\|equation\\|x?x?align\\|split\\|multiline\\|gather\\).*}"
     formula)
    ("[^\\$]\\($\\($[^$]*\\$\\|[^$]*\\)\\$\\)" 1 formula); '$...$' or '$$...$$'

    ;; things that bring in external files
    ("\\\\\\(include\\|input\\|bibliography\\){" "}" include)

    ;; "wysiwyg" emphasis -- these don't work with nested expressions
    (YaTeX-19-region-large-type "{\\\\\\(em\\|it\\|sl\\)"  italic)
    (YaTeX-19-region-large-type "{\\\\bf" bold)
    ;;;(YaTeX-19-region-large-type "{\\\\tt" tt)
    ;;;("\\\\begin{verbatim" "\\\\end{verbatim" tt)

    ("``" "''" string)

    ;; things that do some sort of cross-reference
    (YaTeX-19-region-section-type
     "\\\\\\(\\(no\\)?cite\\|\\(page\\)?ref\\|label\\|index\\|glossary\\){"
     crossref))
"*Hiliting pattern alist for LaTeX text.")

;;(defvar YaTeX-hilit-pattern-adjustment-default nil)
;; ↑いらなくなった。
(defvar YaTeX-hilit-pattern-adjustment-private nil
  "*Adjustment hilit-pattern-alist for default yatex-mode's pattern.")
(defvar YaTeX-hilit-sectioning-face
  '(yellow/dodgerblue yellow/slateblue)
  "*Hilightening face for sectioning unit.  '(FaceForLight FaceForDark)")
(defvar YaTeX-hilit-sectioning-attenuation-rate
  '(15 40)
  "*Maximum attenuation rate of sectioning face. '(ForeRate BackRate)
Each rate specifies how much portion of RGB value should be attenuated
towards to lowest sectioning unit.  Numbers should be written in percentage.")
(defvar YaTeX-sectioning-patterns-alist nil
  "Hilightening patterns for sectioning units.")
(defvar YaTeX-hilit-singlecmd-face
  '(slateblue2 aquamarine)
  "*Hilightening face for maketitle type.  '(FaceForLight FaceForDark)")

;;; セクションコマンドを、構造レベルの高さに応じて色の濃度を変える
;;; 背景が黒でないと何が嬉しいのか分からないに違いない.
;;; もしかして白地の時は構造レベルに応じて色を明るくしたほうが良いのか?
(cond
 ((and (featurep 'hilit19) (fboundp 'x-color-values))
  (let*((sectface
	 (car (if (eq hilit-background-mode 'dark)
		  (cdr YaTeX-hilit-sectioning-face)
		YaTeX-hilit-sectioning-face)))
	(sectcol (symbol-name sectface))
	sect-pat-alist)
    (if (string-match "/" sectcol)
	(let ((fmin (nth 0 YaTeX-hilit-sectioning-attenuation-rate))
	      (bmin (nth 1 YaTeX-hilit-sectioning-attenuation-rate))
	      colorvalue fR fG fB bR bG bB pat fg bg level from face list lm)
	  (require 'yatexsec)
	  (setq fg (substring sectcol 0 (string-match "/" sectcol))
		bg (substring sectcol (1+ (string-match "/" sectcol)))
		colorvalue (x-color-values fg)
		fR (/ (nth 0 colorvalue) 256)
		fG (/ (nth 1 colorvalue) 256)
		fB (/ (nth 2 colorvalue) 256)
		colorvalue (x-color-values bg)
		bR (/ (nth 0 colorvalue) 256)
		bG (/ (nth 1 colorvalue) 256)
		bB (/ (nth 2 colorvalue) 256)
		lm YaTeX-sectioning-max-level
		list YaTeX-sectioning-level)
	  (while list
	    (setq pat (concat YaTeX-ec-regexp (car (car list)) "\\*?{")
		  level (cdr (car list))
		  fg (format "hex-%02x%02x%02x"
			     (- fR (/ (* level fR fmin) lm 100))
			     (- fG (/ (* level fG fmin) lm 100))
			     (- fB (/ (* level fB fmin) lm 100)))
		  bg (format "hex-%02x%02x%02x"
			     (- bR (/ (* level bR bmin) lm 100))
			     (- bG (/ (* level bG bmin) lm 100))
			     (- bB (/ (* level bB bmin) lm 100)))
		  from (intern (format "sectioning-%d" level))
		  face (intern (concat fg "/" bg)))
	    (hilit-translate from face)
	    (setq sect-pat-alist
		  (cons;;(list pat "}" face)
		   (list 'YaTeX-19-region-section-type pat face)
		   sect-pat-alist))
	    (setq list (cdr list)))
	  (setq YaTeX-sectioning-patterns-alist sect-pat-alist))))))

;; ローカルなマクロを読み込んだ後 redraw すると
;; ローカルマクロを keyword として光らせる(keywordじゃまずいかな…)。
(defun YaTeX-19-collect-macros ()
  (cond
   ((and (featurep 'hilit19) (fboundp 'hilit-translate))
    (let ((get-face
	   (function
	    (lambda (table)
	      (cond
	       ((eq hilit-background-mode 'light) (car table))
	       ((eq hilit-background-mode 'dark) (car (cdr table)))
	       (t nil)))))
	  sect single)
      (hilit-translate
       ;;sectioning (funcall get-face YaTeX-hilit-sectioning-face)
       macro (funcall get-face YaTeX-hilit-singlecmd-face))
      (if (setq sect (append user-section-table tmp-section-table))
	  (setq sect (concat "\\\\\\("
			     (mapconcat
			      (function
			       (lambda (s) (regexp-quote (car s))))
			      sect
			      "\\|")
			     "\\){")))
      (if (setq single (append user-singlecmd-table tmp-singlecmd-table))
	  (setq single (concat "\\\\\\("
			       (mapconcat
				(function (lambda (s) (regexp-quote (car s))))
				single
				"\\|")
			       "\\)\\b")))
      (setq hilit-patterns-alist		;Remove at first.
	    (delq (assq 'yatex-mode hilit-patterns-alist) hilit-patterns-alist)
	    hilit-patterns-alist
	    (cons
	     (cons 'yatex-mode
		   (append
		    (list nil)
		    YaTeX-sectioning-patterns-alist
		    YaTeX-hilit-pattern-adjustment-private
		    ;;YaTeX-hilit-pattern-adjustment-default
		    YaTeX-hilit-patterns-alist
		    (delq nil
			  (list
			   (if sect (list
				     'YaTeX-19-region-section-type
				     sect
				     'keyword))
			   (if single (list single 0 'macro))))))
	     hilit-patterns-alist))))))
;;(YaTeX-19-collect-macros)	;causes an error
(defun YaTeX-hilit-recenter (arg)
  "Collect current local macro and hilit-recenter."
  (interactive "P")
  (YaTeX-19-collect-macros)
  (hilit-recenter arg))
(if (fboundp 'hilit-recenter)		;Replace hilit-recenter with
    (mapcar (function (lambda (key)	;YaTeX-hilit-recenter in yatex-mode
			(define-key YaTeX-mode-map key 'YaTeX-hilit-recenter)))
	    (where-is-internal 'hilit-recenter)))

(defun YaTeX-switch-to-new-window ()
  (let ((c 0) (i 1) (free (make-string win:max-configs ? )))
    (while (< i win:max-configs)
      (or (aref win:configs i) (aset free i (+ i win:base-key)))
      (setq i (1+ i)))
    (while (not (string-match (char-to-string c) free))
      (message "Which window to create? [%s]: " free)
      (setq c (read-char)))
    (message "Creating window [%c]" c)
    (set-buffer (get-buffer-create "*scratch*"))
    (win:switch-window (- c win:base-key))))

(defun YaTeX-visit-main-other-frame ()
  "Visit main file in other frame.
WARNING, This code is not perfect."
  (interactive)
  (if (YaTeX-main-file-p) (message "I think this is main LaTeX source.")
    (let (parent)
      (save-excursion (YaTeX-visit-main t) (setq parent (current-buffer)))
      (cond
       ((get-buffer-window parent t)
	(goto-buffer-window parent))
       ((and (featurep 'windows) win:use-frame)
	(YaTeX-switch-to-new-window)
	(switch-to-buffer parent))
       (t (switch-to-buffer-other-frame (buffer-name parent)))))))

(defun YaTeX-goto-corresponding-*-other-frame (arg)
  "Go to corresponding object in other frame."
  (interactive "P")
  (let (b p)
    (save-window-excursion
      (save-excursion
	(YaTeX-goto-corresponding-* arg)
	(setq b (current-buffer) p (point))))
    (cond
     ((get-buffer-window b t)
      (goto-buffer-window b)
      (goto-char p))
     ((and (featurep 'windows) win:use-frame)
      (YaTeX-switch-to-new-window)
      (switch-to-buffer b)
      (goto-char p))
     (t (switch-to-buffer-other-frame (buffer-name b))
	(goto-char p))))
)

;;; reverseVideo にして hilit-background-mode を 'dark
;;; にしている人は数式などが暗くなりすぎて見づらいかもしれない。
;;; 次のコードを hilit19 をロードしている場所の直後に置くとちょっ
;;; とはまし。
;;;  (if (eq hilit-background-mode 'dark)
;;;      (hilit-translate
;;;       string 'mediumspringgreen
;;;       formula 'khaki
;;;       label 'yellow-underlined))
(and YaTeX-emacs-19
     (boundp 'byte-compile-current-file)
     (if (and (boundp 'window-system) window-system)
	 (require 'hilit19)
       (error "Byte compile this file on window system! Not `-nw'!")))

(provide 'yatex19)

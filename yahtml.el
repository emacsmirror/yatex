;;; -*- Emacs-Lisp -*-
;;; (c ) 1994-1999 by HIROSE Yuuji [yuuji@gentei.org]
;;; Last modified Wed Jul 14 18:01:18 1999 on firestorm
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
;;;[インストール方法]
;;;
;;; yahtml.el, yatexlib.el, yatexprc.el を load-path の通ったディレクト
;;; リにインストールしてください。その後、以下を参考に ~/.emacs に設定を
;;; 追加して下さい。
;;;
;;; 	(setq auto-mode-alist
;;; 		(cons (cons "\\.html$" 'yahtml-mode) auto-mode-alist))
;;; 	(autoload 'yahtml-mode "yahtml" "Yet Another HTML mode" t)
;;; 	(setq yahtml-www-browser "netscape")
;;;      ;お気に入りのブラウザを書いて下さい。netscapeが便利です。
;;; 	(setq yahtml-path-url-alist
;;; 	      '(("/home/yuuji/public_html" . "http://www.mynet/~yuuji")
;;; 		("/home/staff/yuuji/html" . "http://www.othernet/~yuuji")))
;;;      ;UNIXの絶対パスと対応するURLのリストを書いて下さい。
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
;;;  * [prefix] l	Complete typeface-changing commands such as
;;;			`<i> ... </i>' or `<samp> ... </samp>'
;;; 			This completion can be used to make in-line
;;; 			tags which is normally completed with [prefix] b.
;;;  * [prefix] s	Complete declarative notations such as
;;;			`<img src="foo.gif">'
;;;			`<input name="var" ...>'
;;;  * [prefix] m	Complete single commands such as
;;;			`<br>' or `<hr> or <li>...'
;;;  * [prefix] p	Insert <p></p> on the point
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
;;;  * [prefix] ;	Translate chars of `>', `<', `&', and `"' to 
;;;			`&gt;', `&lt;', `&amp;', `&quot;' respectively
;;;			in the region.
;;;  * [prefix] :	Do translation opposite to above, in the region.
;;;  * [prefix] #	Translate unsafe-chars and unreserved-chars to
;;;			URLencoded string in the region.
;;; 
;;;[キーの説明]
;;; 
;;; 以下の説明において、特にカスタマイズをしていない限り、[prefix] は
;;; C-c キーを意味します。
;;;
;;;  * [prefix] b X	`</H1>' といった終了タグが必要となる`H1'のよう
;;;			な環境を補完入力します。<a href=foo> ... </a>
;;;			もこのグループです。
;;;			`href=...' と入力した後、TABキーを押すことで、
;;;			ファイル名や (href="#foo") のようなラベルも補完
;;;			できます。
;;;  * [prefix] s	以下のような宣言の補完を行います。
;;;			`<img src="foo.gif">'
;;;			`<input name="var" ...>'
;;;  * [prefix] l	`<i> ... </i>' や `<samp> ... </samp>' のよう
;;;			なテキストスタイル指定のタグを補完します。
;;;			この補完機能は通常 [prefix] b で補完できるものを
;;;			一行内で書きたいときにも用いることが出来ます。
;;;  * [prefix] m	`<br>' や `<hr> '、`<li>' 等の単体タグの補完
;;;			を行います。
;;;  * [prefix] p	カーソル位置に<p></p>を挿入します。
;;;  * M-RET		おまかせ改行; もしul、ol、dl等のタグ(リスト)を
;;;			使っている場合に、環境に合わせて改行と <li>、
;;;			<dt>、<dd>を入力します。
;;;  * menu-bar yahtml	選択したアイテムをメニューより補完できます。
;;;			(私はメニューが嫌いなんですが、htmlに関してはメ
;;;			ニューは一番ありがたいかも)
;;;  * [prefix] g	対応するタグ、<dl> <-> </dl> や href="xxx" の
;;;			ような TAG にジャンプします。
;;;			<img src=...> の場合はイメージビューワを呼び出
;;;			します。href=hoge.html の場合はhoge.htmlに飛びま
;;;			す。
;;;  * [prefix] k	ポイント上の HTML タグを消去します。
;;;			もし universal-argument を付けた場合(C-uを先に押
;;;			す)HTMLタグで囲まれた内容も同時に消去します。
;;;  * [prefix] c	ポイント上のタグを変更します。
;;;			`href="xxx"'の上で [prefix] c を利用した場合は、
;;;			参照しているリンクを補完機能を使いながら変更で
;;;			きます。
;;;  * [prefix] t j	カレントファイルに対して jweblint を呼び出しま
;;;			す。
;;;  * [prefix] t p	WWW ブラウザでカレントファイルを表示します。
;;;			(lisp変数 yahtml-www-browser の設定をお忘れな
;;;			く。お推めは "netscape" で、ねすけの場合既にねす
;;;			けが起動されていた場合そのねすけに Reload 命令を
;;;			送るという芸当が出来ます)
;;;  * [prefix] a	YaTeX のアクセント記号補完と同じです。
;;;			&lt; &gt; 等が入力できます。
;;;  * [prefix] ;	指定したリジョン中の > < & " をそれぞれ
;;;			&gt; &lt; &amp; &quot; に変換します。
;;;  * [prefix] :	指定したリジョン中で上と逆の変換をします。
;;;  * [prefix] #	指定したリジョン中で%エンコードの必要な文字が
;;;			あればそれらをエンコードします。
;;; 
;;; [謝辞]
;;; 
;;; fj野鳥の会の皆さんには貴重な助言を頂きました。また、下に示す方々には
;;; 特に大きな協力を頂きました。あわせてここに感謝申し上げます。
;;; 
;;;	* 横田和也さん(マツダ)
;;;		マニュアルの和訳をして頂きました。
;;;	* 吉田尚志さん(NTT Data)
;;;		Mule for Win32 での動作のさせ方を教えて頂きました。
;;;		(というかほとんどやってもらった ^^;)
;;; 


;(require 'yatex)
(require 'yatexlib)
;;; --- customizable variable starts here ---
(defvar yahtml-prefix "\C-c"
  "*Prefix key stroke of yahtml functions.")
(defvar yahtml-image-viewer "xv" "*Image viewer program")
(defvar yahtml-www-browser "netscape"
  "*WWW Browser command")
(defvar yahtml-kanji-code 2
  "*Kanji coding system number of html file; 1=sjis, 2=jis, 3=euc")
;;(defvar yahtml-coding-system
;;  (cdr (assq yahtml-kanji-code YaTeX-kanji-code-alist))
;;  "Kanji coding system")
(and (featurep 'mule)
     (integerp yahtml-kanji-code)
     (setq yahtml-kanji-code
	   (cdr (assq yahtml-kanji-code YaTeX-kanji-code-alist))))

(defvar yahtml-fill-column 72 "*fill culumn used for yahtml-mode")
(defvar yahtml-fill-prefix nil "*fill prefix for yahtml-mode")

;;(defvar yahtml-www-server "www" "*Host name of your domain's WWW server")
(defvar yahtml-path-url-alist nil
  "*Alist of unix path name vs. URL name of WWW server.
Ex.
'((\"/usr/home/yuuji/http\" . \"http://www.comp.ae.keio.ac.jp/~yuuji\")
  (\"/usr/home/yuuji/darts/http\" . \"http://darts.comp.ae.keio.ac.jp/~darts\"))")
(defvar yahtml-directory-index "index.html"
  "*Directory index file name;
Consult your site's WWW administrator.")

(defvar yahtml-environment-indent 1
  "*Indentation depth of HTML's listing environment")

(defvar YaTeX-japan (or (boundp 'NEMACS) (boundp 'MULE) YaTeX-emacs-20)
  "Whether yatex mode is running on Japanese environment or not.")

(defvar yahtml-lint-program (if YaTeX-japan "jweblint" "weblint")
  "*Program name to lint HTML file")
(defvar yahtml-hate-too-deep-indentation nil
  "*Non-nil for this variable suppress deep indentation in listing environments.")

(defvar yahtml-always-/p nil
  "*Those who always use <p> with </p> set this to t.")

(defvar yahtml-p-prefered-env-regexp "^\\(body\\|dl\\)"
  "*Regexp of envs where paragraphed sentences are prefered.")

(defvar yahtml-template-file "~/http/template.html"
  "*Template HTML file.  It'll be inserted to empty file.")

(defvar yahtml-prefer-upcases nil
  "*Non-nil for preferring upcase TAGs")

(defvar yahtml-prefer-upcase-attributes nil
  "*Non-nil for preferring upcase attributes")

(defvar yahtml-server-type 'apache
  "*WWW server program type")

(defvar yahtml-apache-access-file ".htaccess"
  "*Server access file name for apache")

(defvar yahtml-use-css t "*Use stylesheet or not")

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
  (define-key yahtml-mode-map yahtml-prefix yahtml-prefix-map)
  (define-key yahtml-mode-map "\M-\C-@" 'yahtml-mark-begend)
  (if (and (boundp 'window-system) (eq window-system 'x) YaTeX-emacs-19)
      (define-key yahtml-mode-map [?\M-\C- ] 'yahtml-mark-begend))
  (define-key yahtml-mode-map "\M-\C-a" 'YaTeX-beginning-of-environment)
  (define-key yahtml-mode-map "\M-\C-e" 'YaTeX-end-of-environment)
  (define-key yahtml-mode-map "\M-\C-m" 'yahtml-intelligent-newline)
  (define-key yahtml-mode-map "\C-i" 'yahtml-indent-line)
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
    (YaTeX-define-key "p" 'yahtml-insert-p map)
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
    (YaTeX-define-key ";" 'yahtml-translate-region map)
    (YaTeX-define-key ":" 'yahtml-translate-reverse-region map)
    (YaTeX-define-key "#" 'yahtml-escape-chars-region map)
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
  "*Syntax table for yahtml-mode")

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
  '(("img") ("input") ("link") ("meta")))
(defvar yahtml-user-form-table nil)
(defvar yahtml-tmp-form-table nil)
(defvar yahtml-last-form "img")

(defvar yahtml-env-table
  '(("html") ("head") ("title") ("body") ("dl") ("ul") ("ol") ("pre")
    ("a") ("form") ("select") ("center") ("textarea") ("blockquote")
    ("OrderedList" . "ol")
    ("UnorderedList" . "ul")
    ("DefinitionList" . "dl")
    ("Preformatted" . "pre")
    ("table") ("thead") ("tbody") ("tfoot") ("caption") ("tr") ("th") ("td")
    ("address") 
    ("h1") ("h2") ("h3") ("h4") ("h5") ("h6")
    ;; ("p") ;This makes indentation screwed up!
    ("style") ("div")
    ))

;(defvar yahtml-itemizing-regexp
;  "\\(ul\\|ol\\|dl\\)"
;  "Regexp of itemizing forms")

(defvar yahtml-user-env-table nil)
(defvar yahtml-tmp-env-table nil)

;;; Completion tables for typeface designator
(and yahtml-always-/p
     (or (assoc "p" yahtml-env-table)
	 (setq yahtml-env-table (cons '("p") yahtml-env-table))))

(defvar yahtml-typeface-table
  (append
   '(("dfn") ("em") ("cite") ("code") ("kbd") ("samp") ("strike")
     ("strong") ("var") ("b") ("i") ("tt") ("u") ("big") ("small") ("font")
     ("sup") ("sub") ("span"))
   yahtml-env-table)
  "Default completion table of typeface designator")
(defvar yahtml-user-typeface-table nil)
(defvar yahtml-tmp-typeface-table nil)
(defvar yahtml-last-typeface-cmd "a")

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

;(defvar yahtml-struct-name-regexp
;  "\\<\\(h[1-6]\\|[uod]l\\|html\\|body\\|title\\|head\\|table\\|t[rhd]\\|pre\\|a\\|form\\|select\\|center\\|blockquote\\)\\b")
(defvar yahtml-struct-name-regexp
  (concat
   "\\<\\("
   (mapconcat (function (lambda (x) (car x))) yahtml-typeface-table "\\|")
   "\\)\\b")
  "Regexp of structure beginning.")
(or (assoc "p" yahtml-env-table)
    (setq yahtml-env-table (cons '("p") yahtml-env-table)))

(defun yahtml-dir-default-charset ()
  (cond
   ((and (eq yahtml-server-type 'apache) ;;check .htaccess
	 buffer-file-name)
    (let ((dir default-directory)
	  charset af ext (ldir "")
	  (case-fold-search t)
	  (uid (car (cdr (cdr (file-attributes "."))))))
      (setq ext (file-name-nondirectory buffer-file-name)
	    ext (substring ext (string-match "\\.[a-z0-9]+$" ext)))
      (if (string-match "^[A-Z]:" dir)
	  (setq dir (substring dir 2)))	;remove drive letter
      (while (and dir
		  (not (string= dir ldir))
		  (equal uid (car (cdr (cdr (file-attributes dir))))))
	(setq af (expand-file-name yahtml-apache-access-file dir))
	(if (file-exists-p af)
	    (save-excursion
	      (set-buffer (find-file-noselect af))
	      (save-excursion
		(goto-char (point-min))
		(if (re-search-forward  ;search the charset for same extension
		     (format "^\\s *AddType.*charset=\\(.*\\)\\%s$" ext)
		     nil t)
		    (progn
		      (setq charset
			    (buffer-substring
			     (match-beginning 1) (match-end 1)))
		      (cond
		       ((string-match "iso-2022-jp" charset)
			(setq charset 2))
		       ((string-match "euc-jp" charset)
			(setq charset 3))
		       ((string-match "shift_jis" charset)
			(setq charset 1))
		       (t (setq charset nil)))
		      (setq dir ""))))
	      (kill-buffer (current-buffer))))
	(setq ldir dir
	      dir (substring dir 0 (string-match "/$" dir))
	      dir (file-name-directory dir)))
      (if (featurep 'mule)
	  (setq charset (cdr (assq charset YaTeX-kanji-code-alist))))
      charset
      ))
   (t nil))
  )

(defun yahtml-mode ()
  (interactive)
  (let ((coding (or (yahtml-dir-default-charset) yahtml-kanji-code)))
    (cond
     ((and YaTeX-emacs-20 (boundp 'buffer-file-coding-system))
      (setq buffer-file-coding-system coding))
     ((featurep 'mule)
      (set-file-coding-system coding))
     ((boundp 'NEMACS)
      (make-local-variable 'kanji-fileio-code)
      (setq kanji-fileio-code coding))))
  (setq major-mode 'yahtml-mode
	mode-name "yahtml")
  (mapcar
   (function (lambda (x)
	       (make-local-variable (car x))
	       (set (car x) (if (and (symbolp (cdr x))
				     (boundp (cdr x)))
				(symbol-value (cdr x))
			      (cdr x)))))
   '((YaTeX-ec . "")
     (YaTeX-struct-begin . "<%1%2")
     (YaTeX-struct-end . "</%1>")
     (YaTeX-struct-name-regexp . yahtml-struct-name-regexp)
     (YaTeX-comment-prefix . "<!--")
     (YaTeX-coding-system . yahtml-kanji-code) ;necessary?
     (YaTeX-typesetting-mode-map . yahtml-lint-buffer-map)
     (fill-prefix . yahtml-fill-prefix) (fill-column . yahtml-fill-column)
     (paragraph-start . yahtml-paragraph-start)
     (paragraph-separate . yahtml-paragraph-separate)
     (comment-start . "<!-- ") (comment-end . " -->")
     (comment-start-skip . comment-start)
     (indent-line-function . yahtml-indent-line)))

  (set-syntax-table yahtml-syntax-table)
  (use-local-map yahtml-mode-map)
  (YaTeX-read-user-completion-table)
  (yahtml-css-scan-styles)
  (turn-on-auto-fill)			;Sorry, this is prerequisite
  (and (= 0 (buffer-size)) (file-exists-p yahtml-template-file)
       (y-or-n-p (format "Insert %s?" yahtml-template-file))
       (insert-file-contents (expand-file-name yahtml-template-file)))
  (run-hooks 'text-mode-hook 'yahtml-mode-hook))

(defun yahtml-define-menu (keymap bindlist)
  (cond
   ((featurep 'xemacs)
    (let ((name (keymap-name (symbol-value keymap))))
      (set keymap nil)
      (mapcar
       (function
	(lambda (bind)
	  (setq bind (cdr bind))
	   (if (eq (car (cdr bind)) 'lambda)
	       (setcar (cdr bind) 'progn))
	   (if (stringp (car (cdr bind)))
	       (set keymap (cons (cdr bind) (symbol-value keymap)))
	     (set keymap (cons (vector (car bind) (cdr bind) t)
			       (symbol-value keymap))))))
       bindlist)
      (set keymap (cons name (symbol-value keymap)))))
   (t
    (mapcar
     (function
      (lambda (bind)
	(define-key (symbol-value keymap) (vector (car bind)) (cdr bind))))
     bindlist))))

(defvar yahtml-menu-map nil "Menu map of yahtml")
(defvar yahtml-menu-map-sectioning nil "Menu map of yahtml(sectioning)")
(defvar yahtml-menu-map-listing nil "Menu map of yahtml(listing)")
(defvar yahtml-menu-map-logical nil "Menu map of yahtml(logical tags)")
(defvar yahtml-menu-map-typeface nil "Menu map of yahtml(typeface tags)")

;;; Variables for mosaic url history
(defvar yahtml-urls nil "Alist of global history")
(defvar yahtml-urls-private nil)
(defvar yahtml-urls-local nil)

(cond
 ((and YaTeX-emacs-19 (null yahtml-menu-map))
  (setq yahtml-menu-map (make-sparse-keymap "yahtml"))
  (setq yahtml-menu-map-sectioning (make-sparse-keymap "sectioning menu"))
  (YaTeX-define-menu
   'yahtml-menu-map-sectioning
   (nreverse
    '((1 "H1" . (lambda () (interactive) (yahtml-insert-begend nil "H1")))
      (2 "H2" . (lambda () (interactive) (yahtml-insert-begend nil "H2")))
      (3 "H3" . (lambda () (interactive) (yahtml-insert-begend nil "H3")))
      (4 "H4" . (lambda () (interactive) (yahtml-insert-begend nil "H4")))
      (5 "H5" . (lambda () (interactive) (yahtml-insert-begend nil "H5")))
      (6 "H6" . (lambda () (interactive) (yahtml-insert-begend nil "H6")))
      )))
  (setq yahtml-menu-map-logical (make-sparse-keymap "logical tags"))
  (YaTeX-define-menu
   'yahtml-menu-map-logical
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
  (YaTeX-define-menu
   'yahtml-menu-map-typeface
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
  (YaTeX-define-menu
   'yahtml-menu-map-listing
   (nreverse
    '((ul	"Unordered" .
		(lambda () (interactive) (yahtml-insert-begend nil "UL")))
      (ol	"Ordered" .
		(lambda () (interactive) (yahtml-insert-begend nil "OL")))
      (dl	"Definition" .
		(lambda () (interactive) (yahtml-insert-begend nil "DL")))
      )))
  (setq yahtml-menu-map-item (make-sparse-keymap "item"))
  (YaTeX-define-menu
   'yahtml-menu-map-item
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
  (YaTeX-define-menu
   'yahtml-menu-map
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
  (if (featurep 'xemacs)
      (add-hook 'yahtml-mode-hook
		'(lambda ()
		   (or (assoc "yahtml" current-menubar)
		       (progn
			 (set-buffer-menubar (copy-sequence current-menubar))
			 (add-submenu nil yahtml-menu-map))))))
  ))

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
	;; We want to keep region effective for new tagged environment
	;; to enable continuous regioning by another environment
	(let ((beg (region-beginning))
	      (end (region-end))
	      (addin (yahtml-addin cmd)))
	  (save-excursion
	    (goto-char end)
	    (insert-before-markers (format "</%s>%s" cmd (if bolp "\n" "")))
	    (goto-char beg)
	    (insert (format "<%s%s>%s" cmd addin (if bolp "\n" "")))))
      (insert (format "<%s%s>" cmd (yahtml-addin cmd)))
      (save-excursion
	(insert "\n")
	(indent-to-column cc)
	(insert (format "</%s>" cmd)))
      (if (string-match "^a\\|p$" cmd)	;aとp決め打ちってのが美しくない…
	  (newline)
	(yahtml-intelligent-newline nil))
      (yahtml-indent-line))))

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
		(format "Form(default %s): " yahtml-last-form)
		'yahtml-form-table 'yahtml-user-form-table
		'yahtml-tmp-form-table))))
   (let ((p (point)) q)
     (if (string= form "") (setq form yahtml-last-form))
     (setq yahtml-last-form form)
     (if yahtml-prefer-upcases (setq form (upcase form)))
     (insert (format "<%s%s>" form (yahtml-addin form)))
     ;;(indent-relative-maybe)
     (if (cdr (assoc form yahtml-form-table))
	 (save-excursion (insert (format "</%s>" form))))
     (if (search-backward "\"\"" p t) (forward-char 1))))

;;; ---------- Add-in ----------
(defun yahtml-addin (form)
  "Check add-in function's existence and call it if exists."
   (let ((addin (concat "yahtml:" (downcase form))) s a)
     (concat
      (if (setq a (assoc form yahtml-css-class-alist))
	  (yahtml-make-optional-argument ;should be made generic?
	   "class" (completing-read "class: " (cdr a))))
      (if (and (intern-soft addin) (fboundp (intern-soft addin))
	       (stringp (setq s (funcall (intern addin))))
	       (string< "" s))
	  (if (eq (aref s 0) ? ) s (concat " " s))
	""))))


(defvar yahtml-completing-buffer nil)
(defun yahtml-collect-labels (&optional file)
  "Collect current buffers label (<?? name=...>).
If optional argument FILE is specified collect labels in FILE."
  (let (list end)
    (save-excursion
      (set-buffer yahtml-completing-buffer)
      (if file (let (hilit-auto-highlight)
		 (set-buffer (find-file-noselect file))))
      (save-excursion
	(goto-char (point-min))
	(while ;(re-search-forward "<\\w+\\b" nil t)
	    (re-search-forward "\\(name\\|id\\)\\s *=" nil t)
	  ;(setq bound (match-end 0))
	  ;(search-forward ">" nil t)
	  (setq end (match-end 0))
	  (if (and ;(re-search-backward "\\(name\\|id\\)\\s *=" bound t)
	       (yahtml-on-assignment-p)
	       (progn
		 (goto-char end)
		 (skip-chars-forward " \t\n")
		 (looking-at "\"?#?\\([^\">]+\\)\"?\\b")))
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
  (let ((p (point)) initial i2 cmpl path dir file listfunc beg labels)
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
		     (or (yahtml-url-to-path initial) initial)
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

(defvar yahtml-escape-chars 'ask
  "*Escape reserved characters to URL-encoding or not.
Nil for never, t for everytime, and 'ask for inquiring
at each reserved chars.")

;
; Subject: [yatex:02849] Re: [yahtml] tilda in href tag
; From: Masayasu Ishikawa <mimasa@sfc.keio.ac.jp>
; To: yatex@arcadia.jaist.ac.jp
; Date: Mon, 31 May 1999 21:09:31 +0900
; RFC 2396 の "2.4.3. Excluded US-ASCII Characters" によると、以下の文字
; は必ずエスケープしないといけません。
;
;     control     = <US-ASCII coded characters 00-1F and 7F hexadecimal>
;     space       = <US-ASCII coded character 20 hexadecimal>
;     delims      = "<" | ">" | "#" | "%" | <">
;     unwise      = "{" | "}" | "|" | "\" | "^" | "[" | "]" | "`"
(defvar yahtml-unsafe-chars-regexp
  "[][\x0- \x7f <>%\"{}|\\^`]" ;#は除去する
  "Characters regexp which must be escaped in URI.")
;
; また、以下の文字は予約された用法以外に用いる場合にはエスケープしないと
; いけないことになっています。
;
;     reserved    = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" |
;                   "$" | ","
(defvar yahtml-unreserved-chars-regexp
  "[;/?:@&=+$,]"
  "Characters regexp which should be escaped in URI on certain conditions.
Not used yet.")

(defun yahtml-escape-chars-string (str)
  "Translate reserved chars to URL encoded string."
  (let ((p 0) (target "")
	(ask (eq yahtml-escape-chars 'ask)))
    (cond
     ((null yahtml-escape-chars) str)
     (t
      (while (and (string< "" str)
		  (setq p (string-match yahtml-unsafe-chars-regexp str)))
	(if (and ask (y-or-n-p (format "Escape char [%c] of `%s'"
				       (aref str p) (substring str 0 (1+ p)))))
	    (setq target (concat target
				 (substring str 0 p)
				 (format "%%%x" (aref str p))))
	  (setq target (concat target (substring str 0 (1+ p)))))
	(setq str (substring str (1+ p))))
      (concat target str)))))

(defun yahtml-escape-chars-region (beg end)
  "Translate reserved chars to encoded string in the region."
  (interactive "r")
  (save-excursion
    (let ((e (set-marker (make-marker) end)) c m yes)
      (goto-char beg)
      (while (and (< (point) e)
		  (re-search-forward
		   (concat yahtml-unsafe-chars-regexp "\\|"
			   yahtml-unreserved-chars-regexp) e t))
	(sit-for 0)
; 	(setq m (buffer-modified-p)
; 	      c (char-after (1- (point))))
; 	(save-excursion (backward-char 1) (insert " ==>"))
; 	(unwind-protect
; 	    (setq yes (y-or-n-p (format "Replace: [%c]" c)))
; 	  (save-excursion
; 	    (backward-char 1)
; 	    (delete-backward-char 4))
; 	  (set-buffer-modified-p m))
	(message "Replace: [%c] (y or n):" (setq c (char-after (1- (point)))))
	(if (memq (read-char) '(?y ?Y))
	    (progn
	      (delete-region (match-beginning 0) (match-end 0))
	      (insert (format "%%%x" c)))))
      (set-marker e nil))))
;; ab%defgls/.|

(defun yahtml:a ()
  "Add-in function for <a>"
  (let ((href ""))
    (setq yahtml-completing-buffer (current-buffer)
	  yahtml-urls (append yahtml-urls-private yahtml-urls-local)
	  href (yahtml-escape-chars-string
		(read-from-minibuffer "href: " "" yahtml-url-completion-map)))
    (prog1
	(concat (yahtml-make-optional-argument
		 "href" href)
		(yahtml-make-optional-argument
		 "name" (read-string "name: ")))
      (if (and (string-match "^http://" href)
	       (null (assoc href yahtml-urls-private))
	       (null (assoc href yahtml-urls-local)))
	  (YaTeX-update-table
	   (list href)
	   'yahtml-urls-private 'yahtml-urls-private 'yahtml-urls-local))
      )))

(defvar yahtml-parameters-completion-alist
  '(("align" ("top") ("middle") ("bottom") ("left") ("right") ("center"))
    ("src" . file)
    ("method" ("POST") ("GET"))
    ("rev" . yahtml-link-types-alist)
    ("rel" . yahtml-link-types-alist)
    ("type" . yahtml-content-types-alist)))

(defvar yahtml-link-types-alist 
  '(("alternate") ("stylesheet") ("start") ("next") ("prev")
    ("contents") ("index") ("glossary") ("chapter") ("section")
    ("subsection") ("appendix") ("help") ("bookmark")))

(defvar yahtml-content-types-alist
  '(("text/css") ("text/html") ("text/plain") ("text/richtext")
    ("text/sgml") ("text/xml")
    ("application/octet-stream") ("application/postscript") ("application/pdf")
    ("image/jpeg") ("image/gif") ("image/tiff") ("video/mpeg"))
  "Alist of content-types")

(defun yahtml-read-parameter (par &optional default alist)
  (let* ((alist
	  (cdr-safe (assoc (downcase par)
			   (or alist yahtml-parameters-completion-alist))))
	 (prompt (concat par ": "))
	 v)
    (cond
     ((eq alist 'file)
      (read-file-name prompt "" default nil ""))
     ((symbolp alist)
      (completing-read prompt (symbol-value alist) nil nil default))
     (alist
      (completing-read prompt alist nil nil default))
     (t 
      (read-string prompt default)))))
      
(defun yahtml-make-optional-argument (opt arg)
  "Make optional argument string."
  (if (string= "" arg)
      ""
    (concat " "
	    (if yahtml-prefer-upcase-attributes (upcase opt) (downcase opt))
	    "=\"" arg "\"")))

(defun yahtml:body ()
  "Add-in function for <body>"
  (let ((b (read-string "bgcolor="))
	(x (read-string "text color="))
	(l (read-string "link color="))
	(v (read-string "vlink color=")))
    (concat
     (yahtml-make-optional-argument "bgcolor" b)
     (yahtml-make-optional-argument "text" x)
     (yahtml-make-optional-argument "link" l)
     (yahtml-make-optional-argument "vlink" v))))


(defun yahtml:img ()
  "Add-in function for <img>"
  (let ((src (yahtml-read-parameter "src"))
	(alg (yahtml-read-parameter "align"))
	(alt (yahtml-read-parameter "alt"))
	(brd (read-string "border="))
	(l yahtml-prefer-upcase-attributes))
    (concat (if l "SRC" "src") "=\"" src "\""
	    (yahtml-make-optional-argument "align" alg)
	    (yahtml-make-optional-argument "alt" alt)
	    (yahtml-make-optional-argument "border" brd))))

(defun yahtml:form ()
  "Add-in function `form' input format"
  (concat
   " " (if yahtml-prefer-upcase-attributes "METHOD" "method=")
   (completing-read "Method: " '(("POST") ("GET")) nil t)
   " " (if yahtml-prefer-upcase-attributes "ACTION" "action") "=\""
   (read-string "Action: ") "\""
   ))

(defun yahtml:select ()
  "Add-in function for `select' input format"
  (setq yahtml-last-single-cmd "option")
  (concat " " (if yahtml-prefer-upcase-attributes "NAME" "name") "=\""
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
    (yahtml-make-optional-argument "align" alg)
))

(defvar yahtml-input-types
  '(("text") ("password") ("checkbox") ("radio") ("submit")
    ("reset") ("image") ("hidden") ("file")))

(defun yahtml:input ()
  "Add-in function for `input' form"
  (let ((size "") name type value checked (maxlength "")
	(l yahtml-prefer-upcase-attributes))
    (setq name (read-string "name: ")
	  type (completing-read "type (default=text): "
				yahtml-input-types nil t)
	  value (read-string "value: "))
    (if (string-match "text\\|password\\|^$" type)
	(setq size (read-string "size: ")
	      maxlength (read-string "maxlength: ")))
    (concat
     (if l "NAME" "name") "=\"" name "\""
     (yahtml-make-optional-argument "type" type)
     (yahtml-make-optional-argument "value" value)
     (yahtml-make-optional-argument "size" size)
     (yahtml-make-optional-argument "maxlength" maxlength)
    )))

(defun yahtml:textarea ()
  "Add-in function for `textarea'"
  (interactive)
  (let (name rows cols)
    (setq name (read-string "Name: ")
	  cols (read-string "Columns: ")
	  rows (read-string "Rows: "))
    (concat
     (concat (if yahtml-prefer-upcase-attributes "NAME=" "name=")
	     "\"" name "\"")
     (yahtml-make-optional-argument "cols" cols)
     (yahtml-make-optional-argument "rows" rows))))

(defun yahtml:table ()
  "Add-in function for `table'"
  (let ((b (read-string "border="))
	(a (yahtml-read-parameter "align")))
    (concat
     (yahtml-make-optional-argument "border" b)
     (yahtml-make-optional-argument "align" a))))
;(fset 'yahtml:caption 'yahtml:p)
(defun yahtml:caption ()
  "Add-in function for `caption' in table tag"
  (let ((yahtml-parameters-completion-alist '(("align" ("top") ("bottom")))))
    (yahtml-make-optional-argument "align" (yahtml-read-parameter "align"))))

(defun yahtml:font ()
  "Add-in function for `font'"
  (concat 
   (yahtml-make-optional-argument "color" (read-string "color="))
   (yahtml-make-optional-argument "size" (read-string "size="))))

(defun yahtml:style ()
  "Add-in function for `style'"
  (yahtml-make-optional-argument
   "type" (read-string "type=" "text/css")))

(defun yahtml:tr ()
  "Add-in function for `tr'"
  (setq ;yahtml-last-begend "td"		;; which do you prefer?
	yahtml-last-typeface-cmd "td")
  "")

(defun yahtml:link ()
  "Add-in function for `link' (まだちょっと良く分かってない)"
  (let (rel rev type href)
    (setq rel (yahtml-read-parameter "rel"))
    (cond
     ((equal rel "")
      (concat (yahtml-make-optional-argument
	       "rev" (yahtml-read-parameter "rev"))
	      (yahtml-make-optional-argument
	       "href" (yahtml-read-parameter "href")
	       ;;他に良く使うのって何?
	       )))
     ((string-match "stylesheet" rel)
      (concat
       (yahtml-make-optional-argument "rel" rel)
       (yahtml-make-optional-argument
	"type" (yahtml-read-parameter "type" "text/css"))
       (progn
	 (setq href
	       (read-from-minibuffer "href: " "" yahtml-url-completion-map))
	 (if (string< "" href)
	     (progn
	       (if (and (file-exists-p (yahtml-url-to-path href))
			(y-or-n-p "Load css symbols now? "))
		   (setq yahtml-css-class-alist
			 (yahtml-css-collect-classes-file
			  (yahtml-url-to-path href) yahtml-css-class-alist)))
	       (message "")
	       (yahtml-make-optional-argument "href" href))))))
     (t ;;??
      ))))

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

(defun yahtml-insert-p (&optional arg)
  "Convenient function to insert <p></p>"
  (interactive "P")
  (if arg (yahtml-insert-tag nil "p")
    (save-excursion			;insert "/p" first to memorize "p"
      (yahtml-insert-single "/p"))	;in the last-completion variable
    (yahtml-insert-single "p")))

;;; ---------- Jump ----------
(defun yahtml-on-href-p ()
  "Check if point is on href clause."
  (let ((p (point)) e cmd (case-fold-search t))
    (save-excursion
      (and ;;(string= (YaTeX-inner-environment t) "a") ;aでなくても許可にした
	   (save-excursion
	     ;;(search-forward "</a>" nil t) ;aでなくても許可にした
	     (search-forward "[\" \t\n]" nil t)
	     (setq e (point)))
	   ;(goto-char (get 'YaTeX-inner-environment 'point))
	   (re-search-backward "<\\(a\\|link\\)\\>" nil t)
	   (search-forward "href" e t)
	   (search-forward "=" e t)
	   (progn
	     (skip-chars-forward " \t\n")
	     (looking-at "\"?\\([^\"> \t\n]+\\)\"?"))
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
     ((string-match "^start\\>" yahtml-www-browser)
      (if (get-buffer pb)
	  (progn (set-buffer pb) (erase-buffer) (set-buffer cb)))
      (put 'yahtml-netscape-sentinel 'url href)
      (set-process-sentinel
       (setq yahtml-browser-process
	     (start-process
	      "browser" pb shell-file-name yahtml-shell-command-option
	      (format "%s \"%s\"" yahtml-www-browser href)))
       'yahtml-netscape-sentinel))
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
  (let ((href (yahtml-on-href-p)) file name (parent buffer-file-name))
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
		    (YaTeX-switch-to-buffer file))
		  (or YaTeX-parent-file (setq YaTeX-parent-file parent))))
	    (if name
		(progn (set-mark-command nil) (yahtml-jump-to-name name)))
	    t)))))

(defun yahtml-jump-to-name (name)
  "Jump to html's named tag."
  (setq name (format "\\(name\\|id\\)\\s *=\\s *\"?%s\\>\"?" name))
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

(defun yahtml-get-attrvalue (attr)
  "Extract current tag's attribute value from buffer."
  (let (e (case-fold-search t))
    (save-excursion
      (or (looking-at "<")
	  (progn (skip-chars-backward "^<") (backward-char 1)))
      (setq e (save-excursion (forward-list 1) (point)))
      (if (and
	   (re-search-forward (concat "\\b" attr "\\b") e t)
	   (progn (skip-chars-forward " \t\n=")
		  (looking-at "\"?\\([^\"> \t\n]+\\)\"?")))
	  (YaTeX-match-string 1)))))

(defun yahtml-goto-corresponding-source (&optional other)
  "Goto applet's source."
  (let ((env (yahtml-current-tag)) s (p (point)))
    (cond
     ((string-match "applet" env)
      (if (setq s (yahtml-get-attrvalue "code"))
	  (progn
	    (setq s (YaTeX-match-string 1)
		  s (concat
		     (substring s 0 (string-match "\\.[A-Za-z]+$" s))
		     ".java"))
	    (if other (YaTeX-switch-to-buffer-other-window s)
	      (YaTeX-switch-to-buffer s))
	    s)				;return source file name
	(message "No applet source specified")
	(sit-for 1)
	nil))
     ((string-match "!--#include" env)
      (cond
       ((setq s (yahtml-get-attrvalue "file")) ;<!--#include file="foo"-->
	(if other (YaTeX-switch-to-buffer-other-window s)
	  (YaTeX-switch-to-buffer s))
	s)
       ((setq s (yahtml-get-attrvalue "virtual"));<!--#include virtual="foo"-->
	(setq s (yahtml-url-to-path s))
	(if other (YaTeX-switch-to-buffer-other-window s)
	  (YaTeX-switch-to-buffer s))
	s)))
     ((and (string-match "!--#exec" env)
	   (setq s (yahtml-get-attrvalue "cmd")))
      (setq s (substring s 0 (string-match " \t\\?" s))) ;get argv0
      (let ((b " *yahtmltmp*"))		;peek a little
	(unwind-protect
	    (progn
	      (set-buffer (get-buffer-create b))
	      (insert-file-contents s nil 0 100)
	      (if (looking-at "#!")
		  (if other (YaTeX-switch-to-buffer-other-window s)
		    (YaTeX-switch-to-buffer s))))
	  (kill-buffer (get-buffer b)))
	(get-file-buffer s))))))

(defun yahtml-goto-corresponding-* (&optional other)
  "Go to corresponding object."
  (interactive)
  (cond
   ((yahtml-goto-corresponding-href other))
   ((yahtml-goto-corresponding-img))
   ((yahtml-goto-corresponding-begend))
   ((yahtml-goto-corresponding-source other))
   (t (message "I don't know where to go."))
   ))

(defun yahtml-goto-corresponding-*-other-window ()
  "Go to corresponding object."
  (interactive)
  (yahtml-goto-corresponding-* t))

(defun yahtml-visit-main ()
  "Go to parent file from where you visit current file."
  (interactive)
  (if YaTeX-parent-file (YaTeX-switch-to-buffer YaTeX-parent-file)))

;;; ---------- killing ----------
(defun yahtml-kill-begend (&optional whole)
  (let ((tag (yahtml-on-begend-p)) p q r bbolp)
    (if tag
	(save-excursion
	  (or (looking-at "<")
	      (progn (skip-chars-backward "^<") (forward-char -1)))
	  (setq p (point))
	  (yahtml-goto-corresponding-begend)
	  (or (looking-at "<")
	      (progn (skip-chars-backward "^<") (forward-char -1)))
	  (if (< (point) p)		;if on the opening tag
	      (progn (setq q p p (point))
		     (goto-char q))
	    (setq q (point)))		;now q has end-line's (point)
	  (if (not whole)
	      (kill-region
	       (progn (skip-chars-backward " \t")
		      (if (setq bbolp (bolp)) (point) q))
	       (progn (forward-list 1)
		      (setq r (point))
		      (skip-chars-forward " \t")
		      (if (and bbolp (eolp) (not (eobp))) (1+ (point)) r))))
	  (goto-char p)
	  (skip-chars-backward " \t")
	  (if (not whole)
	      (progn
		(kill-append
		 (buffer-substring
		  (setq p (if (setq bbolp (bolp)) (point) p))
		  (setq q (progn
			    (forward-list 1)
			    (setq r (point))
			    (skip-chars-forward " \t")
			    (if (and bbolp (eolp) (not (eobp)))
				(1+ (point))
			      r))))
		 t)
		(delete-region p q))
	    (kill-region
	     (if (bolp) (point) p)
	     (progn (goto-char q)
		    (forward-list 1)
		    (setq r (point))
		    (skip-chars-forward " \t")
		    (if (and (eolp) (not (eobp))) (1+ (point)) r))))
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
	href b1 e1 attr new css)
    (cond
     (tag
      (cond
       ((and (string-match "^a$" tag)
	     (save-excursion
	       (and
		(re-search-backward "<a\\b" nil t)
		(progn
		  (goto-char (match-end 0))
		  (skip-chars-forward " \t\n")
		  (setq b1 (point))
		  (search-forward ">" nil t))
		(setq e1 (match-beginning 0))
		(goto-char b1)
		(re-search-forward "href\\s *=" e1 t)
		(>= p (point))
		(progn
		  (goto-char (match-end 0))
		  (skip-chars-forward " \t\n")
		  (looking-at "\"?\\([^\"> \t\n]+\\)\"?"))
		(< p (match-end 0)))))
	(setq b1 (match-beginning 1) e1 (match-end 1)
	      yahtml-completing-buffer (current-buffer)
	      ;; yahtml-urls-local is buffer-local, so we must put
	      ;; that into yahtml-urls here
	      yahtml-urls (append yahtml-urls-private yahtml-urls-local)
	      href (read-from-minibuffer
		    "Change href to: " "" yahtml-url-completion-map))
	(if (string< "" href)
	    (progn
	      ;;(setq href  ;??
		;;    (if yahtml-prefer-upcases (upcase href) (downcase href)))
	      (delete-region b1 e1)
	      (goto-char b1)
	      (insert href))))
       ((setq attr (yahtml-on-assignment-p)) ;if on the assignment to attr
	(if (and (equal attr "class")	     ;treat "class" attribute specially
		 (setq css (assoc tag yahtml-css-class-alist)))
	    (setq new (yahtml-read-parameter ;should be made generic?
		       attr nil (list (cons "class" (cdr css)))))
	  ;;other than "class", read parameter normally
	  (setq new (yahtml-read-parameter attr)))
	(goto-char (car (get 'yahtml-on-assignment-p 'region)))
	(delete-region (point) (cdr (get 'yahtml-on-assignment-p 'region)))
	(insert new))
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
	  (set-marker q nil)
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
	  (set-marker beg nil)
	  (set-marker end nil)	
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
(defvar yahtml-translate-hyphens-when-comment-region t
  "*Non-nil for translate hyphens to &#45; when comment-region")

(defun yahtml-comment-region (&optional uncom)
  "Comment out region or environment."
  (interactive)
  (let ((e (make-marker)) beg p)
    (cond
     ((marker-position (set-marker e (yahtml-on-begend-p)))
      (save-excursion
	(setq p (point))
	(if (string-match "^/" e)
	    (setq beg (progn (forward-line 1) (point)))
	  (setq beg (progn (beginning-of-line) (point))))
	(goto-char p)
	(yahtml-goto-corresponding-begend)
	(if (string-match "^/" e)
	    (beginning-of-line)
	  (forward-line 1))
	(set-marker e (point))
	;(comment-region beg (point) (if uncom (list 4)))
	))
     (t ;(comment-region (region-beginning) (region-end) (if uncom (list 4)))
      (setq beg (region-beginning))
      (set-marker e (region-end))))
    (if yahtml-translate-hyphens-when-comment-region
	(let ((yahtml-entity-reference-chars-alist-default nil)
	      (yahtml-entity-reference-chars-alist '((?- . "#45")))
	      yahtml-entity-reference-chars-regexp
	      yahtml-entity-reference-chars-reverse-regexp)
	  (yahtml-entity-reference-chars-setup)
	  (funcall
	   (if uncom 'yahtml-translate-reverse-region
	     'yahtml-translate-region)
	   beg e)))
    (comment-region beg e (if uncom (list 4)))
    (set-marker e nil)))

(defun yahtml-uncomment-region ()
  (interactive)
  (yahtml-comment-region t))

;;; ---------- translate to entity references ----------
(defvar yahtml-entity-reference-chars-alist-default
  ;'((?> . "gt") (?< . "lt") (?& . "amp") (?\" . "quot") (?' . "apos"))
  '((?> . "gt") (?< . "lt") (?& . "amp") (?\" . "quot"))
  "Default translation table from character to entity reference")
(defvar yahtml-entity-reference-chars-alist nil
  "*Translation table from character to entity reference")
(defvar yahtml-entity-reference-chars-regexp nil)
(defvar yahtml-entity-reference-chars-reverse-regexp nil)

(defun yahtml-entity-reference-chars-setup ()
  (let ((list (append yahtml-entity-reference-chars-alist-default
		      yahtml-entity-reference-chars-alist)))
    (setq yahtml-entity-reference-chars-regexp "["
	  yahtml-entity-reference-chars-reverse-regexp "&\\(")
    (while list
      (setq yahtml-entity-reference-chars-regexp
	    (concat yahtml-entity-reference-chars-regexp
		    (char-to-string (car (car list))))
	    yahtml-entity-reference-chars-reverse-regexp
	    (concat yahtml-entity-reference-chars-reverse-regexp
		    (cdr (car list))
		    (if (cdr list) "\\|")))
      (setq list (cdr list)))
    (setq yahtml-entity-reference-chars-regexp
	  (concat yahtml-entity-reference-chars-regexp "]")
	  yahtml-entity-reference-chars-reverse-regexp
	  (concat yahtml-entity-reference-chars-reverse-regexp "\\);"))))

(yahtml-entity-reference-chars-setup)

(defun yahtml-translate-region (beg end)
  "Translate inhibited literals."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((ct (append yahtml-entity-reference-chars-alist
		       yahtml-entity-reference-chars-alist-default)))
	(goto-char beg)
	(while (re-search-forward yahtml-entity-reference-chars-regexp nil t)
	  ;(setq c (preceding-char))
	  (replace-match
	   (concat "&" (cdr (assoc (preceding-char) ct)) ";")))))))

(defun yahtml-translate-reverse-region (beg end)
  "Translate entity references to literals."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((ct (append yahtml-entity-reference-chars-alist
		       yahtml-entity-reference-chars-alist-default))
	    ec)
	(goto-char beg)
	(while (re-search-forward
		yahtml-entity-reference-chars-reverse-regexp nil t)
	  ;(setq c (preceding-char))
	  (setq ec (YaTeX-match-string 1))
	  (delete-region (match-end 0) (match-beginning 0))
	  (insert (car (YaTeX-rassoc ec ct))))))))

(defun yahtml-inner-environment-but (exclude &optional quick)
  "Return the inner environment but matches with EXCLUDE tag."
  (let (e (case-fold-search t))
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
	       (or
		(and (fboundp 'char-category)
		     (string-match "[chj]" (char-category (preceding-char))))
		(and (fboundp 'char-charset)
		     (not (eq (char-charset (preceding-char)) 'ascii)))))
	  (setq ccol (1+ ccol)))))
  (if (and force (> col ccol))
      (progn
	(insert-char ?\  (- col ccol))
	col)
    ccol)))

(defun yahtml-fill-paragraph (arg)
  (interactive "P")
  (let*((case-fold-search t) (p (point)) fill-prefix
	(e (or (yahtml-inner-environment-but "^\\(a\\|p\\)\\b" t) "html"))
	indent
	(startp (get 'YaTeX-inner-environment 'point))
	(prep (string-match "^pre$" e))
	(ps1 (if prep (default-value 'paragraph-start)
	       paragraph-start))
	(ps2 (if prep (concat (default-value 'paragraph-start)
			      "$\\|^\\s *</?pre>")
	       paragraph-start)))
    (save-excursion
      (unwind-protect
	  (progn
	    (if prep
		(fset 'move-to-column 'yahtml-move-to-column))
	    (save-excursion
	      (beginning-of-line)
	      (indent-to-column (yahtml-this-indent))
	      (setq fill-prefix
		    (buffer-substring (point) (point-beginning-of-line)))
	      (delete-region (point) (point-beginning-of-line)))
	    (fill-region-as-paragraph
	     (progn (re-search-backward paragraph-start nil t)
		    (or (save-excursion
			  (goto-char (match-beginning 0))
			  (if (looking-at "<")
			      (forward-list)
			    (goto-char (match-end 0))
			    (skip-chars-forward " \t>"))
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
  "Indent a line (faster wrapper)"
  (interactive)
  (let (indent)
    (if (and (save-excursion
	       (beginning-of-line) (skip-chars-forward "\t ")
	       (not (looking-at "<")))
	     (save-excursion
	       (forward-line -1)
	       (while (and (not (bobp)) (looking-at "^\\s *$"))
		 (forward-line -1))
	       (skip-chars-forward "\t ")
	       (setq indent (current-column))
	       (not (looking-at "<"))))
	(progn
	  (save-excursion
	    (beginning-of-line)
	    (skip-chars-forward " \t")
	    (or (= (current-column) indent)
		(YaTeX-reindent indent)))
	  (and (bolp) (skip-chars-forward " \t")))
      (yahtml-indent-line-real))))

(defun yahtml-this-indent ()
  (let ((envs "[uod]l\\|table\\|[ht][rhd0-6]\\|select\\|blockquote\\|center\\|menu\\|dir\\|font")
	(itemizing-envs "^\\([uod]l\\|menu\\|dir\\)$")
	(itms "<\\(dt\\|dd\\|li\\|t[rdh]\\|option\\)\\b")
	inenv p col peol (case-fold-search t))
    (save-excursion
      (beginning-of-line)
      (setq inenv (or (yahtml-inner-environment-but "^\\(a\\|p\\)\\b" t)
		      "html")
	    col (get 'YaTeX-inner-environment 'indent)
	    p (get 'YaTeX-inner-environment 'point)
	    op nil))
    (save-excursion
      (cond
       ((string-match envs inenv)
	(save-excursion
	  (beginning-of-line)
	  (skip-chars-forward " \t")
	  (cond				;lookup current line's tag
	   ((looking-at (concat "</\\(" envs "\\)>"))
	    col)
	   ((looking-at itms)
	    (+ col yahtml-environment-indent))
	   ((and yahtml-hate-too-deep-indentation
		 (looking-at (concat "<\\(" envs "\\)")))
	    (+ col (* 2 yahtml-environment-indent)))
	   ((and (< p (point))
		 (string-match itemizing-envs inenv)
		 (save-excursion
		   (and
		    (setq op (point))
		    (goto-char p)
		    (re-search-forward itms op t)
		    (progn
		      (skip-chars-forward "^>")
		      (skip-chars-forward ">")
		      (skip-chars-forward " \t")
		      (setq col (if (looking-at "$")
				    (+ col yahtml-environment-indent)
				  (current-column)))))))
	    col)
	   (t
	    (+ col yahtml-environment-indent)))))
       (t col)))))

(defun yahtml-indent-line-real ()
  "Indent current line."
  (interactive)
  (YaTeX-reindent (yahtml-this-indent))
  (if (bolp) (skip-chars-forward " \t"))
  (let (peol col)
    (if (and (setq inenv (yahtml-on-begend-p))
	     (string-match
	      (concat "^\\<\\(" yahtml-struct-name-regexp "\\)") inenv))
	(save-excursion
	  (setq peol (point-end-of-line))
	  (or (= (char-after (point)) ?<)
	      (progn (skip-chars-backward "^<") (forward-char -1)))
	  (setq col (current-column))
	  (if (and (yahtml-goto-corresponding-begend t)
		   (> (point) peol))	;if on the different line
	      (YaTeX-reindent col))))))

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
  (require 'yatexprc)
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
	      (cond
	       ((string-match (concat "^" (regexp-quote docroot)) file)
		(setq realpath
		      (expand-file-name
		       (substring
			file
			(if (= (aref file (1- (match-end 0))) ?/)
			    (match-end 0) ; "/foo"
			  (min (1+ (match-end 0)) (length file)))) ; "/~foo"
		       (car (car list))))))
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
    (setq env (downcase (or (yahtml-inner-environment-but "^\\(a\\|p\\)\\b" t)
			    "html")))
    (setq func (intern-soft (concat "yahtml-intelligent-newline-" env)))
    (newline)
    (if (and env func (fboundp func))
	;; if intelligent line function is defined, call that
	(funcall func)
      ;; else do the default action
      (if (string-match yahtml-p-prefered-env-regexp env)
	  (yahtml-insert-p)))))

(defvar yahtml-faithful-to-htmllint nil)
(defun yahtml-intelligent-newline-ul ()
  (interactive)
  (yahtml-insert-single "li")
  (or yahtml-faithful-to-htmllint (insert " "))
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
	  (yahtml-insert-single "dd")
	  (or yahtml-faithful-to-htmllint (insert " "))
	  (setq yahtml-last-single-cmd "dt"))
	 ((match-beginning 3)
	  (yahtml-insert-single "dt")
	  (or yahtml-faithful-to-htmllint (insert " "))
	  (setq yahtml-last-single-cmd "dd")))
      (insert (if yahtml-prefer-upcases "<DT> " "<dt> "))
      (setq yahtml-last-single-cmd "dd"))
    (yahtml-indent-line)
    (and (string-match yahtml-p-prefered-env-regexp "dl")
	 (string-equal yahtml-last-single-cmd "dt")
	 (yahtml-insert-p nil))))

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
  (message "1:< 2:> 3:& 4:\" 5:' 6:nbsp")
  (let ((c (read-char)))
    (setq c (if (or (< c ?0) (> c ?7))
		(string-match (regexp-quote (char-to-string c)) "<>&\"")
	      (- c ?1)))
    (if (or (< c 0) (> c 6))
	nil
      (insert (format "&%s;"
		      (nth c '("lt" "gt" "amp" "quot" "apos" "nbsp")))))))


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
  (yahtml-jump-to-error-line t))

(defun yahtml-jump-to-error-line (&optional sit)
  (interactive "P")
  (let ((p (point)) (e (point-end-of-line)))
    (end-of-line)
    (if (re-search-backward yahtml-error-line-regexp nil t)
	(let ((f (YaTeX-match-string 1))
	      (l (string-to-int (YaTeX-match-string 2))))
	  (if sit (sit-for 1))
	  (forward-line -1)
	  (YaTeX-showup-buffer (YaTeX-switch-to-buffer f t) nil t)
	  (goto-line l))
      (message "No line number usage"))))

;;; ---------- Style Sheet Support ----------
(defvar yahtml-css-class-alist nil
  "Alist of elements vs. their classes")

(defun yahtml-css-collect-classes-region (beg end &optional initial)
  (save-restriction
    (save-excursion
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((alist initial) b e element class a
	    (s1 (aref (syntax-table) ?\{ ))
	    (s2 (aref (syntax-table) ?\} )))
	;(modify-syntax-entry ?{ "(}")
        ;(modify-syntax-entry ?} "){")
	(setq b (point))
	(unwind-protect
	    (while (search-forward "{" nil t)
	      (setq e (point))
	      (goto-char b)
	      (while (re-search-forward		;ちょといい加減なREGEXP
		      "\\([a-z][a-z0-9]*\\)\\.\\([a-z][a-z0-9]*\\)\\>" e t)
		(setq element (YaTeX-match-string 1)
		      class (YaTeX-match-string 2))
		(if (setq a (assoc element alist))
		    (or (assoc class (cdr a))
			(setcdr a (cons (list class) (cdr a))))
		  (setq alist (cons (list element (list class)) alist))))
	      (goto-char (1- e))
	      ;(forward-list 1)
	      (search-forward "}" nil t)
	      (setq b (point)))
	  (aset (syntax-table) ?\{ s1)
	  (aset (syntax-table) ?} s2))
      alist))))
	    
(defun yahtml-css-collect-classes-buffer (&optional initial)
  (interactive)
  (yahtml-css-collect-classes-region (point-min) (point-max) initial))

(defun yahtml-css-collect-classes-file (file &optional initial)
  (let ((hilit-auto-highlight nil) (cb (current-buffer)))
    (set-buffer (find-file-noselect file))
    (prog1
	(yahtml-css-collect-classes-buffer initial)
      (set-buffer cb))))

(defun yahtml-css-scan-styles ()
  (save-excursion
    (goto-char (point-min))
    (set (make-local-variable 'yahtml-css-class-alist) nil)
    (while (re-search-forward "<\\(style\\|link\\)" nil t)
      (let ((b (match-beginning 0))(tag (YaTeX-match-string 1)) e href alist)
	(cond
	 ((string-match "style" tag)
	  (goto-char b)
	  (save-excursion (forward-list 1) (setq e (point)))
	  (cond
	   ((search-forward "text/css" e 1) ;css definition starts
	    (setq alist
		  (yahtml-css-collect-classes-region
		   (point) (progn (search-forward "</style>") (point))
		   alist)))))
	 ((and (string-match "link" tag)
	       (setq href (yahtml-get-attrvalue "href"))
	       (file-exists-p (yahtml-url-to-path href)))
	    (setq alist
		  (yahtml-css-collect-classes-file
		   (yahtml-url-to-path href) alist))))
	(setq yahtml-css-class-alist alist)))))

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
    ("<!--#\\(include\\|exec\\|config\\|fsize\\|flastmod\\)" "-->" include)
    ;; string
    (hilit-string-find ?\\ string)
    (yahtml-hilit-region-tag "\\(em\\|strong\\)" bold)
    ("</?[uod]l>" 0 decl)
    ("<\\(di\\|dt\\|li\\|dd\\)>" 0 label)
    ("<a\\s +href" "</a>" crossref)
    (yahtml-hilit-region-tag-itself "</?\\sw+\\>" decl)
    ))

(defun yahtml-hilit-region-tag (tag)
  "Return list of start/end point of <TAG> form."
  (if (re-search-forward (concat "<" tag ">") nil t)
      (let ((m0 (match-beginning 0)))
	(skip-chars-forward " \t\n")
	(cons (point)
	      (progn (re-search-forward (concat "</" tag ">") nil t)
		     (match-beginning 0))))))

(defun yahtml-hilit-region-tag-itself (ptn)
  "Return list of start/end point of <tag options...> itself."
  (if (re-search-forward ptn nil t)
      (let ((m0 (match-beginning 0)))
	(skip-chars-forward "^>")
	(cons m0 (1+ (point) )))))

;(setq hilit-patterns-alist (delq (assq 'yahtml-mode hilit-patterns-alist) hilit-patterns-alist))
(and (featurep 'hilit19)
     (or (assq 'yahtml-mode hilit-patterns-alist)
	 (setq hilit-patterns-alist
	       (cons (cons 'yahtml-mode yahtml-hilit-patterns-alist)
		     hilit-patterns-alist))))

(run-hooks 'yahtml-load-hook)
(provide 'yahtml)

; Local variables:
; fill-prefix: ";;; "
; paragraph-start: "^$\\|\\|;;;$"
; paragraph-separate: "^$\\|\\|;;;$"
; End:

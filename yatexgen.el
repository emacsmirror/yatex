;;; -*- Emacs-Lisp -*-
;;; Generate add-in functions for YaTeX.
;;; yatexgen.el rev.1(beta2)
;;; (c)1991-1993 by HIROSE Yuuji.[yuuji@ae.keio.ac.jp]
;;; Last modified Mon Sep 20 17:55:13 1993 on gloria
;;; $Id$

(require 'yatex)
(provide 'yatexgen)

(defmacro YaTeX-setq (var japanese english)
  (list 'setq var
	(if YaTeX-japan japanese english))
)

(put 'YaTeX-setq 'lisp-indent-hook 1)

(YaTeX-setq YaTeX-generate-initial-message
  "             �����������[�h�ւ悤����!!

���߂Ă��l�͂��̃o�b�t�@�̗�ɂ��������Ďw���ʂ�ɂ���ė��K���ĂˁB
�{�Ԃ̎������̃o�b�t�@�ɏo�郁�b�Z�[�W�� *�悭�ǂ��* ���삵�Ȃ��Ƃ�
�܂��֐������Ȃ���!!

  �ł̓��^�[���L�[�������ĉ������B"
  "             Welcome to auto-generation mode!!

If this is your first trial, exercise this according to example and
following my messages.  Then, at making actual function, operate
reading my messages *carefully*, or you'll fail to generate appropriate
function.

  Hit return key!"
)

(YaTeX-setq YaTeX-generate-start-message
  "�����͂��߂��.\n1.�o�^�������⊮������Ă݂�.
���Ƃ��� section �^�⊮�� \\documentstyle �������� \\documentstyle{}
����������Ă݂Ă�. �����Ɓw�`�^�⊮�x���g��Ȃ��ƃ_����!�B
�ŁA���������肽�[��!!"
  "Let's begin completion for which you want to make add-in function.
If you want to make add-in function for \\documentstyle input only
`\\documentstyle{}' *with* completion of yatex-mode.
If you finish this, please press RET."
)

(YaTeX-setq YaTeX-generate-abort-message
  "��߂��A��߂��`���߂�ǂ����`"
  "Aborted."
)

(YaTeX-setq YaTeX-generate-same-message
  "���ꂶ��A�Ȃɂ��ς���Ă˂�����[��! ��߂��B"
  "I found no difference between them.  So I'm quitting."
)

(YaTeX-setq YaTeX-generate-invalid-message
  "����́A���Ɩ����Ƃ������̂���."
  "It's impossible."
)

(YaTeX-setq YaTeX-generate-idontknow-message
  "���`��A����Ă悭�킩��Ȃ��Ȃ��B�΂��ł��߂�˃F�`"
  "Sorry I can't tell your adding method."
)

(YaTeX-setq YaTeX-generate-confirm-message
  "�Ƃ������Ƃ́A�t���������������͂���ł����̂�"
  "Is it additional string of add-in function?"
)

(YaTeX-setq YaTeX-generate-output-message
  "2.����A����ɂ������������̂� *�J�[�\���̈ʒu��* �����Ă݂�. 
�������� \\documentstyle{} �̗Ⴞ�� \\documentstyle[12pt]{} �Ƃ��ɂ���́B
�������悤�����ǁA���̈ʒu����J�[�\������������_����!!
�ŁA�܂����������肽�[��!!"
  "2.Then input additional string *at CURSOR POSITION*
According to last example \\documentstyle{},
modify it \\documentstyle[12pt]{}.  RET to finish."
)

(YaTeX-setq YaTeX-generate-put-arg-message
  "3.���̂����A�L�[�{�[�h����ǂݍ���ŗ~��������������ɓ���āB
�������� \\documentstyle[12pt]{} ��������A�t�����镶����[12pt]������
��œ��ꂽ���̂� 12pt �̕��������ł��ˁB
�ŁA�S������I������A�肽�[�񂾂������Ă�!!"
  "3.In this string, extract string which you want to input from key
board with quiry afterwards.  For example, though additional string is
\\documentstyle[12pt]{}, but you want enter only `12pt' by hand.
RET to finish!"
)

(YaTeX-setq YaTeX-generate-read-prompt-message
  "4.�ł́A���Ƃł����̕������ǂݍ��ގ��ɁA�ǂ������v�����v�g��
�o�������ł���? ���ɓ���ĉ������B�ʓ|�Ȃ�P�Ƀ��^�[����ł��ĂˁB
�������� 12pt �̕�����������A�w�T�C�Y�́x�Ƃ����������߁B"
  "4.When you use this add-in function afterwards, what message
do you like to be prompted with to enter these values.  In last
example `12pt', typical prompt string may be `Size?: '."
)

(YaTeX-setq YaTeX-generate-done-message
  "�悵! ���ꂪ�A�N�̍�肽�������֐����B~/.emacs �ɂł�����Ă�������
�y���Ă���B���̃o�b�t�@(*���ē�*)�� yatex-mode �ɂ��Ă�������
�ł����֐����{���ɂ��]�݂̓�������邩�m���߂Ă݂�Ƃ��������ˁB
  �Ƃ���ŁA���̊֐�����ȂɊȒP���낤? ���낻�뎩���ŏ�������ǂ�?
"
  "OK! This is the definition of function you want to make!  Add
this description to your ~/.emacs or so.  Use this buffer(*Guide*)
for testing of this function please.
  But you can see this function quite easy, can't you? You had better
write your most favorite add-in function yourself!
"
)

(YaTeX-setq YaTeX-generate-nomatch-message
  "���炱��A����ȕ�����ǂ��ɂ��ˁ[��!!"
  "No such string in additional string."
)
(YaTeX-setq YaTeX-generate-buffer
  "*�t���֐������o�b�t�@*"
  "*Generate-add-in-function*"
)

(YaTeX-setq YaTeX-generate-message-buffer
  "*���ē�*"
  "*Guide*"
)

(YaTeX-setq YaTeX-generate-bug-message
  "���߁`��!! ������ƁA���̃A�h�C���֐�����̎��s����������݂���!!
��҂܂ŘA�����Ă������`�`�`��!"
  "Sorry I failed to make add-in function for you...
Send bug report to me."
)

(YaTeX-setq YaTeX-generate-narrow-message
  "��ʂ����܂�����悤�ȋC�����܂��B"
  "Too narrow screen height."
)

(defvar YaTeX-generate-message-height
  10 "Window height of YaTeX-generate-message-buffer")

;;;
;Generate mode.
;;;
(defun YaTeX-generate ()
  "Genarate YaTeX add-in function with enquiry."
  (interactive)
  (if (< (screen-height) (+ YaTeX-generate-message-height 10))
      (error YaTeX-generate-narrow-message))
  (put 'YaTeX-generate 'disabled t)
  (save-window-excursion
    (unwind-protect
	(let (input output (i 0) (beg 0) end add-in map map1 si str slist
		    (from (make-marker)) (to (make-marker)))
	  (delete-other-windows)
	  (switch-to-buffer YaTeX-generate-message-buffer)
	  (yatex-mode)
	  (erase-buffer)
	  (insert YaTeX-generate-initial-message)
	  (read-string
	   (if YaTeX-japan "���^�[���L�[�������ĉ�����." "Press RETURN."))
	  (erase-buffer)
	  (insert YaTeX-generate-start-message)
	  (pop-to-buffer (get-buffer-create YaTeX-generate-buffer))
	  (enlarge-window (- (window-height) YaTeX-generate-message-height 1))
	  (erase-buffer)
	  (yatex-mode)
	  (use-local-map (setq map (copy-keymap YaTeX-mode-map)))
	  (define-key (current-local-map) "\n" 'exit-recursive-edit)
	  (define-key (current-local-map) "\r" 'exit-recursive-edit)
	  (define-key (current-local-map) "\C-g" 'abort-recursive-edit)
	  (setq map1 (copy-keymap map))
	  (YaTeX-suppress-sparse-keymap map)
	  ;;First get input form.
	  (recursive-edit)
	  (setq input (buffer-string)
		end (1- (length input)))
	  (if (string= "" input) (error YaTeX-generate-abort-message))
	  (YaTeX-generate-move-to-add-in-position)
	  (set-marker from (1- (point)))  ;;Can't write before `from'
	  (set-marker to (1+ (point)))    ;;Can't write after `to'
	  ;;Second get output form.
	  (setq beg (1- (point)));;Cheat begin point!
	  (YaTeX-generate-display-message YaTeX-generate-output-message)
	  (use-local-map map1)
	  (fset 'si (symbol-function 'self-insert-command))
	  (defun self-insert-command (arg)
	    (interactive "p")
	    (if (or (not (equal (buffer-name) YaTeX-generate-buffer))
		    (and (> (point) (marker-position from))
			 (< (point) (marker-position to))))
		(insert (this-command-keys)) (ding)))
	  (unwind-protect
	      (recursive-edit)
	    (fset 'self-insert-command (symbol-function 'si)))
	  (setq output (buffer-string))
	  (cond ((string= "" output)	(error YaTeX-generate-abort-message))
		((string= input output)	(error YaTeX-generate-same-message))
		((< (length output) (length input))
		 (error YaTeX-generate-invalid-message)))
	  ;;(while (and (< beg end) (= (aref input beg) (aref output i)))
	  ;;  (setq beg (1+ beg) i (1+ i))) ;;for universal use.
	  (setq i (1- (length output)))
	  (while (and (>= end beg) (= (aref output i) (aref input end)))
	    (setq end (1- end) i (1- i)))
	  (setq add-in (substring output beg
				  (if (= i (1- (length output))) nil (1+ i))))
	  (erase-buffer)
	  (insert add-in)
	  (if (not (y-or-n-p YaTeX-generate-confirm-message))
	      (error YaTeX-generate-idontknow-message))
	  ;;Extract arguments.
	  (YaTeX-generate-display-message YaTeX-generate-put-arg-message)
	  (setq i 1)
	  (while (not (string=
		       "" (setq str (read-string (format "Arg %d: " i)))))
	    (if (not (string-match (regexp-quote str) add-in))
		(progn
		  (ding)
		  (YaTeX-generate-display-message
		   YaTeX-generate-nomatch-message -1))
	      (setq slist (append slist (list (list str))) i (1+ i)))
	    );input all of arguments.
	  ;;Compare with output string.
	  (set-buffer YaTeX-generate-buffer) ;;for safety
	  (emacs-lisp-mode)
	  (if (> i 1)
	      (YaTeX-generate-parse-add-in slist add-in)
	    (erase-buffer)
	    (insert "(defun " (YaTeX-generate-function-name) " ()\n")
	    (insert "\"" (YaTeX-generate-lisp-quote add-in) "\")\n")
	    (indent-region (point-min) (point-max) nil)
	    (message (if YaTeX-japan
			 "���̂��炢�̊֐���ŏ���!!"
		       "You don't need me to make such easy function.")))
	  );let
      (put 'YaTeX-generate 'disabled nil)
      (put 'YaTeX-addin 'disabled nil)
    ))
  (YaTeX-generate-display-message YaTeX-generate-done-message)
  (switch-to-buffer YaTeX-generate-buffer)
  (condition-case error
      (eval-current-buffer)
    (error (insert YaTeX-generate-bug-message)))
  (pop-to-buffer YaTeX-generate-message-buffer)
)

(defun YaTeX-generate-parse-add-in (args add-in)
  "Parse add-in string and extract argument for it.
Variable add-in is referred in parent function."
  (let ((i 1) j (case-fold-search nil) ;i holds argument number
	(prompt (make-vector (length args) ""))
	(used (make-vector (length add-in) nil))
	func-name (string ""))
    ;;Phase 1. extract argument from add-in string.
    (mapcar
     '(lambda (arg)
	(let ((index 0) (match 0) beg end (carg (car arg)))
	  (YaTeX-generate-display-message
	   YaTeX-generate-read-prompt-message)
	  (aset prompt (1- i)
		(read-string
		 (format
		  (if YaTeX-japan "%d�Ԗ�(%s)��ǂގ�?: "
		    "When reading argument #%d(%s)?: ") i (car arg))))
	  (while (string-match (regexp-quote carg) (substring add-in index))
	    (setq beg (+ index (match-beginning 0))
		  end (+ index (match-end 0)))
	    (if (aref used beg) nil
	      (setq match (1+ match))
	      (cond
	       ((= match 1)
		;;(setq arg (append arg (list (list beg end))))
		(YaTeX-generate-register-match))
	       ((YaTeX-generate-ask-match-position)
		(YaTeX-generate-register-match))))
	    (setq index end))
	  (setq i (1+ i))))
     args)
    ;;Phase 2. Generate function!!
    (setq i 0)
    (setq func-name (YaTeX-generate-function-name))
    (while (< i (length add-in))
      (setq beg i j (aref used i))
      (while (and (< i (length add-in)) (equal j (aref used i)))
	(setq i (1+ i)))
      (if j		;If it is argument.
	  (setq string (concat string (format " arg%d" j)))
	(setq string
	      (concat string " \""
		      (YaTeX-generate-quote-quote (substring add-in beg i))
		      "\""))
	))
    (erase-buffer)
    (setq i 1)
    (insert
     "(defun " func-name " ()\n"
     "  (let (")
    (mapcar
     '(lambda (arg)
	(insert (format "(arg%d (read-string \"%s: \"))\n"
			i (aref prompt (1- i))))
	(setq i (1+ i)))
     args)
    (delete-region (point) (progn (forward-line -1) (end-of-line) (point)))
    (insert ")\n(concat " (YaTeX-generate-lisp-quote string)
	    ")))\n")
    (indent-region (point-min) (point) nil)
    used)
)

(defun YaTeX-generate-ask-match-position ()
  "Ask user whether match-position is in his expectation,
Referencing variables in parent function YaTeX-generate-parse-add-in."
  (pop-to-buffer YaTeX-generate-message-buffer)
  (goto-char (point-max))
  (insert "\n\n"
	  (format (if YaTeX-japan "%d �Ԗڂ̈��� %s ����"
		    "Is argument #%d's value `%s' also corresponding to")
		  i carg) "\n" add-in "\n")
  (indent-to-column beg)
  (let ((c beg))
    (while (< c end) (insert "^") (setq c (1+ c))))
  (insert "\n" (if YaTeX-japan "�����ɂ��Ή����Ă��?"
		 "this underlined part too?"))
  (other-window -1)
  (y-or-n-p (if YaTeX-japan "�������͂����Ă܂���" "Is underline right"))
)

(defun YaTeX-generate-register-match ()
  (nconc arg (list (list beg end)))
  (let ((x beg))
    (while (< x end) (aset used x i)(setq x (1+ x))))
)

(defun YaTeX-generate-display-message (mes &optional bottom)
  "Display message to generation buffer."
  (pop-to-buffer YaTeX-generate-message-buffer)
  (goto-char (point-max))
  (insert "\n\n")
  (if bottom (recenter (1- bottom)) (recenter 0))
  (insert mes)
  (other-window -1)
)

(defun YaTeX-generate-move-to-add-in-position ()
  "Move cursor where add-in function should insert string."
  (cond
   ((eq YaTeX-current-completion-type 'begin)
    (goto-char (point-min))
    (skip-chars-forward "^{")
    (setq env-name
	  (buffer-substring (1+ (point))
			    (progn (skip-chars-forward "^}") (point))))
    (forward-char 1))
   ((eq YaTeX-current-completion-type 'section)
    (goto-char (point-min))
    (skip-chars-forward "^{"))
   ((eq YaTeX-current-completion-type 'maketitle)
    (goto-char (point-max))
    (if (= (preceding-char) ? )
	(forward-char -1)))
   )
)

(defun YaTeX-generate-function-name ()
  (concat
   "YaTeX:"
   (cond
    ((eq YaTeX-current-completion-type 'begin) env-name)
    ((eq YaTeX-current-completion-type 'section) section-name)
    ((eq YaTeX-current-completion-type 'maketitle) single-command)))
)

(defun YaTeX-generate-lisp-quote (str)
  (let ((len (length str))(i 0) (quote ""))
    (while (< i len)
      (if (= (aref str i) ?\\)
	  (setq quote (concat quote "\\")))
      (if (= (aref str i) 127)
	  (setq quote (concat quote "\""))
	(setq quote (concat quote (substring str i (1+ i)))))
      (setq i (1+ i)))
    quote)
)

(defun YaTeX-generate-quote-quote (str)
  (let ((len (length str))(i 0) (quote ""))
    (while (< i len)
      (if (= (aref str i) ?\")
	  (setq quote (concat quote (char-to-string 127))))
      (setq quote (concat quote (substring str i (1+ i))))
      (setq i (1+ i)))
    quote)
)

(defun YaTeX-suppress-sparse-keymap (map)
  (let ((i ? ))
    (while (< i 127)
      (define-key map (char-to-string i) 'undefined)
      (setq i (1+ i))))
)

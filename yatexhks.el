;;; -*- Emacs-Lisp -*-
;;; Hooks for YaTeX

;;; $BLnD;$K4XO"$9$k5-=R(B($B$?$H$($P%"%I%$%s4X?t(B)$B$O(B yatexhks.el $B$H$$$&L>A0$N(B
;;; $B%U%!%$%k$KF~$l$F$/$@$5$$!#5/F0;~$K<+F0E*$K%m!<%I$7$^$9!#(B

;;; All the private definitions for YaTeX can be stuffed into the file
;;; named `yatexhks.el'.  The file `yatexhks.el' will be automatically
;;; loaded at the end of loading `yatex.el'.

;Private definitions begin from here.

(define-key YaTeX-user-extensional-map "0"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "part")))
(define-key YaTeX-user-extensional-map "1"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "chapter")))
(define-key YaTeX-user-extensional-map "2"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "section")))
(define-key YaTeX-user-extensional-map "3"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "subsection")))
(define-key YaTeX-user-extensional-map "4"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "subsubsection")))
(define-key YaTeX-user-extensional-map "5"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "paragraph")))
(define-key YaTeX-user-extensional-map "6"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "subparagraph")))
(define-key YaTeX-user-extensional-map "r"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "ref")))
(define-key YaTeX-user-extensional-map "i"
  '(lambda () (interactive) (YaTeX-make-singlecmd "item")))
(define-key YaTeX-user-extensional-map "\C-b"
  '(lambda () (interactive) (YaTeX-make-singlecmd "leftarrow")))
(define-key YaTeX-user-extensional-map "l"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "label")))
(define-key YaTeX-user-extensional-map "S"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "setlength")))
(define-key YaTeX-user-extensional-map "b"
  '(lambda () (interactive) (YaTeX-make-fontsize nil "bf")))


;
;;; End of yatexhks.el
(provide 'yatexhks)

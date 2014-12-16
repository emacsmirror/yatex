;;; yatex23.el --- YaTeX facilities for Emacs 23 or later -*- coding: sjis -*-
;;; (c)2014 by HIROSE Yuuji.[yuuji@yatex.org]
;;; Last modified Tue Dec 16 11:08:10 2014 on firestorm
;;; $Id:$

;;; Code:
(defun YaTeX-dnd-handler (uri action)
  "DnD handler for yatex-mode
Convert local image URI to \\includegraphcis{} and
.tex file names to \\include{}."
  (save-excursion
    (YaTeX-visit-main t)
    (let*((file (dnd-get-local-file-name uri))
	  (path (file-relative-name file))
	  (insert-file-directory nil)
	  (case-fold-search t))
      (cond
       ((memq action '(copy link move private))
	(cond
	 ((string-match "\\.\\(jpe?g\\|png\\|gif\\|bmp\\|tiff?\\|e?ps\\|pdf\\)$" path)
	  (insert "\\includegraphics"
		  "{" (YaTeX::includegraphics 1 path t) "}")
	  (YaTeX-package-auto-usepackage "includegraphics" 'section))
	 ((string-match "\\.tex$" path)
	  (insert "\\include{" path "}"))
	 ))
       (t (message "%s" action))))))

(provide 'yatex23)

; Local variables:
; fill-prefix: ";;; "
; paragraph-start: "^$\\|\\|;;;$"
; paragraph-separate: "^$\\|\\|;;;$"
; coding: sjis
; End:

;;; yatexflt.el --- YaTeX filter command utilizer -*- coding: sjis -*-
;;; 
;;; (c)1993-2018 by HIROSE Yuuji.[yuuji@yatex.org]
;;; Last modified Tue Jan  9 13:28:56 2018 on firestorm
;;; $Id$

;;; Commentary:
;;;
;;;	This lisp enables passing inline text to some external filter
;;;	command to generate files such as graphic files.
;;; 
;;;	Typical situation is using blockdiag/dot(graphviz) command to
;;;	generate png/pdf file.
;;;
;;; Example:
;;;
;;;	[[LaTeX Source]]
;;;	%#BEGIN FILTER{foo.pdf}{dot -T %t -o o}
;;;	\if0
;;;	===
;;;	digraph {
;;;	  A -> B;
;;;	  B -> C;
;;;	}
;;;	===
;;;	\fi
;;;	%#END
;;;	\includegraphics{foo.pdf}
;;;
;;;	In this  case above, when  you type  `[prefix] t e'  between two
;;;	`===' lines, the  content in a region are fed  to dot command as
;;;	follows:
;;;	
;;;	    echo TEXT | dot -T pdf -o foo.pdf 
;;;
;;;	Then foo.pdf file will be generated and the image (as PNG) will
;;;	be displayed in the next window.


;;; Code:
(require 'yatexlib)
(defvar YaTeX-filter-special-env-alist-default
  '((".blockdiag"
     "blockdiag -T %t -o %o -"
     "blockdiag {
  default_fontsize = 32;
  A -> B;
}")
    (".seqdiag" "seqdiag -T %t -o %o -"
     "seqdiag {
  client -> server [label = \"SYN\"];
  client <- server [label = \"SYN/ACK\"];
  client -> server [label = \"ACK\"];}")
    (".actdiag" "actdiag -T %t -o %o -"
     "actdiag {
  sayHo -> ho -> hohoho
  lane dj {
    label = \"DJ\"
    sayHo [label = \"Say Ho\"]; hohoho [label = \"Ho Ho Ho!\"]; }
  lane mc { label = \"MC\"; ho [label = \"Hooooh!\"]}}")
    (".nwdiag" "nwdiag -T %t -o %o -"
     "nwdiag {
  network ext {
    address = \"10.1.2.0/24\"
    router [address = \"10.1.2.1\"]
  }
  network int {
    address = \"192.168.22.0/24\"
    router [address = \"192.168.22.1\"]
    websrv [address = \"192.168.22.80\"]
    cli-1; cli-2
  }
}")
    (".rackdiag" "rackdiag -T %t -o %o -"
     "rackdiag {
  16U;
  1: UPS [4U]; 5: Storage [3U]; 8: PC [2U]; 8: PC [2U];
}")
    (".dot"
     "dot -T %t -o %o"
     "digraph {
  graph [charset=\"utf-8\"]
}
bigraph {
  graph [charset=\"utf-8\"]}"
     )))

(defvar YaTeX-filter-special-env-alist-private nil)
(defvar YaTeX-filter-special-env-alist
  (append YaTeX-filter-special-env-alist-private
	  YaTeX-filter-special-env-alist-default))

(defun YaTeX-filter-filter-set-conversion-flag ()
  (let ((ovl (get 'YaTeX-filter-filter-sentinel 'overlay)))
    (if ovl				;; When successful conversion met,
	(progn				;; (1)Set conversion complete flag
	  (add-hook			;; (2)Add hook of seim-automatic
	   'write-file-hooks		;;    update of convert to write-
	   'YaTeX-filter-update-all)	;;    file hook.
	  (overlay-put ovl 'converted t)))))

(defun YaTeX-filter-filter-unset-conversion-flag
    (ovl after beg end &optional length) 
  (if after (overlay-put ovl 'converted nil)))


(defun YaTeX-filter-pngify-sentinel (proc msg)
  (save-excursion
    (let ((b (process-buffer proc)) (selw (selected-window))
	  img)
      (set-buffer b)
      (cond
       ((eq (process-status proc) 'run)
	(put-text-property (point-min) (point-max) 'invisible t))
       ((eq (process-status proc) 'exit)
	(set-buffer b)
	(YaTeX-popup-image
	 (YaTeX-buffer-substring
	  (get 'YaTeX-filter-pngify-sentinel 'start) (point-max))
	 b)
	(YaTeX-filter-filter-set-conversion-flag))
       (t
	(set-buffer b)
	(remove-text-properties (point-min) (point-max) '(invisible t))
	(insert "\nProcess aborted %s\n" msg))))))

(defvar YaTeX-filter-pdf2png-stdout
  (cond
   ((YaTeX-executable-find "convert")	"convert -trim %s PNG:-")
   (t
    "gs -dNOPAUSE -sDEVICE=png256 -sOutputFile=- -dBATCH -q -r75 %s"))
  "Command line syntax to convert PDF file to PNG stream")

(defun YaTeX-filter-modified-BEGEND-regions ()
  "Return the list of overlays which contains un-converted text."
  (save-excursion
    (save-restriction
      (widen)
      (let (r pl (list (overlays-in (point-min) (point-max))))
	(while list
	  (if (and (overlay-get (car list) 'filter-input)
		   (setq pl (plist-member (overlay-properties (car list))
					  'converted))
		   (not (plist-get pl 'converted)))
	      (setq r (cons (car list) r)))
	  (setq list (cdr list)))
	(nconc r)
	r))))

(defun YaTeX-filter-update-all ()
  "Update all destination files from built-in source text."
  (interactive)
  (let ((timeout 4)
	ans ovl (update-list (YaTeX-filter-modified-BEGEND-regions)))
    (if update-list
	(save-excursion
	  (save-window-excursion
	    (catch 'abort
	      (while update-list
		(goto-char (overlay-start (setq ovl (car update-list))))
		(or (pos-visible-in-window-p)
		    (set-window-start nil (point)))
		(unwind-protect
		    (progn
		      (overlay-put ovl 'face 'YaTeX-on-the-fly-activated-face)
		      (message "Non-update source found: Update here: %s "
			       "Y)es N)o A)bort")
		      (setq ans (read-char))
		      (cond
		       ((memq ans '(?Y ?y))
			(YaTeX-filter-BEGEND)
			(while (and (> (setq timeout (1- timeout)))
				    (eq (process-status "Filter") 'run))
			  (message "Waiting for conversion process to finish")
			  (sit-for 1)))
		       ((memq ans '(?A ?a)) (throw 'abort t))
		       (t nil)))
		  (overlay-put ovl 'face nil))
		(setq update-list (cdr update-list)))))))
    ;; Write file hook should return nil
    nil))

(defun YaTeX-filter-filter-sentinel (proc msg)
  (put 'YaTeX-filter-pngify-sentinel 'start nil)
  (let ((b (process-buffer proc))
	(imagefile (get 'YaTeX-filter-filter-sentinel 'outfile))
	ovl
	(selw (selected-window)))
    (save-excursion
      (cond
       ((eq (process-status proc) 'run))
       ((eq (process-status proc) 'exit)
	(set-buffer b)
	(remove-images (point-min) (point-max))
	(if (and (file-regular-p imagefile)
		 (file-readable-p imagefile))
	    (save-excursion
	      (setq buffer-read-only nil)
	      (cond
	       ((string-match "\\.\\(jpg\\|png\\)" imagefile)
		(erase-buffer)
		(YaTeX-popup-image imagefile b)
		(YaTeX-filter-filter-set-conversion-flag))
	       (t 			;Convert again to PNG file
		(goto-char (point-max))
		(insert "\nConvert Again to PNG file...\n")
		(put 'YaTeX-filter-pngify-sentinel 'start (point))
		(set-process-sentinel
		 (start-process
		  "Filter" b		;Safe to reuse
		  shell-file-name YaTeX-shell-command-option
		  (format YaTeX-filter-pdf2png-stdout imagefile))
		 'YaTeX-filter-pngify-sentinel)
		(set-buffer-multibyte nil)
		))
	      (select-window selw)))
	(YaTeX-preview-image-mode)
	)
       (t					;Other status might be an error
	(set-buffer b)
	(goto-char (point-max))
	(insert (format "%s\n" (process-status proc))))))))

(defun YaTeX-filter-pass-to-filter (begend-info)
  "Pass current BEGIN FILTER environment to external command."
  (put 'YaTeX-filter-filter-sentinel 'outfile nil)
  ;; begend-info is from YaTeX-in-BEGEND-p: (BEG END ARGS)
  (let ((b (car begend-info)) (e (nth 1 begend-info))
	(args (nth 2 begend-info))
	(p (point)) openb closeb outfile cmdline point-if0 point-fi)
    (save-excursion
      (if (and begend-info
	       (string-match "FILTER" args) ;easy test
	       (goto-char (car begend-info))
	       (re-search-forward
		"FILTER\\s *{\\([^}]+\\)}" e t)
	       (setq outfile (YaTeX-match-string 1))
	       (goto-char (match-end 0))
	       (prog2			;Step into the second brace
		   (skip-chars-forward "\t ")
		   (looking-at "{")	;Check if 2nd brace surely exists
		 (skip-chars-forward "{")
		 (skip-chars-forward "\t"))
	       (setq openb (point))
	       (condition-case nil
		   (progn (up-list 1) t)
		 (error nil))
	       (setq closeb (1- (point))
		     cmdline (YaTeX-buffer-substring openb closeb))
	       (re-search-forward "^\\\\if0\\>" p t)
	       (setq point-if0 (point))
	       (re-search-forward "^\\\\fi\\>" e t)
	       (setq point-fi (match-beginning 0)))
	  (let*((case-fold-search t)
		(type (cond
		       ((string-match "\\.png$" outfile) "png")
		       ((string-match "\\.svg$" outfile) "svg")
		       (t				 "pdf")))
		(newcmdline (YaTeX-replace-formats
			     cmdline
			     (list (cons "t" type)
				   (cons "o" outfile))))
		(delim (progn (goto-char point-if0)
			      (forward-line 1)
			      (and (looking-at "\\(.\\)\\1\\1") ;Triple chars
				   (prog1
				       (YaTeX-match-string 0)
				     (forward-line 1)))))
		(text-start (point))
		(text-end (if (and delim
				   (re-search-forward
				    (concat "^" (regexp-quote delim))
				    point-fi t))
			      (match-beginning 0)
			    point-fi))
		(text (YaTeX-buffer-substring text-start text-end))
		;;
		;; Now it's time to start filter process
		;;
		(procbuf (YaTeX-system newcmdline "Filter" 'force))
		(proc (get-buffer-process procbuf))
		;(procbuf (get-buffer-create " *Filter*"))
		(ovl (progn
		       (remove-overlays text-start text-end)
		       (make-overlay text-start text-end)))
		(ovlmodhook	;hook function to reset conv-success flag
		 'YaTeX-filter-filter-unset-conversion-flag))
	    (if proc
		(progn
		  (overlay-put ovl 'filter-input outfile)
		  (overlay-put ovl 'converted nil)
		  (overlay-put ovl 'modification-hooks (list ovlmodhook))
		  (set-process-coding-system proc 'undecided 'utf-8)
		  (set-process-sentinel proc 'YaTeX-filter-filter-sentinel)
		  (YaTeX-showup-buffer procbuf)
		  (set-buffer procbuf)
		  (setq buffer-read-only nil)
		  (erase-buffer)
		  (insert (format "Starting process `%s'...\n" newcmdline))
		  (set-marker (process-mark proc) (point-max))
		  (process-send-string proc text)
		  (process-send-string proc "\n")
		  (process-send-eof proc) ;Notify stream chunk end
		  (process-send-eof proc) ;Notify real EOF
		  (put 'YaTeX-filter-filter-sentinel 'outfile outfile)
		  (put 'YaTeX-filter-filter-sentinel 'overlay ovl))))))))

(defun YaTeX-insert-filter-special (filter list &optional region-p)
  (let ((f (YaTeX-read-string-or-skip "Output file: ")))
    (if region-p
	(if (< (point) (mark)) (exchange-point-and-mark)))
    (save-excursion (insert "===\n\\fi\n%#END\n"))
    (and region-p (exchange-point-and-mark))
    (insert (format "%%#BEGIN FILTER{%s}{%s}\n\\if0\n===\n"
		    f (or (car list) "")))
    (save-excursion (insert (or (car (cdr list)) "\n") "\n"))))

(provide 'yatexflt)

; Local variables:
; fill-prefix: ";;;	"
; paragraph-start: "^$\\|\\|;;;$"
; paragraph-separate: "^$\\|\\|;;;$"
; End:

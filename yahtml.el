;;; -*- Emacs-Lisp -*-
;;; (c ) 1994 by HIROSE Yuuji [yuuji@ae.keio.ac.jp, pcs39334@ascii-net]
;;; Last modified Sat Jan 29 16:58:00 1994 on gloria
;;; This is sample hack definition for HTML.
;;;
;;; [Purely tentative version]
;;; $Id$

(require 'yatex)
(defvar yahtml-prefix-map (copy-keymap YaTeX-prefix-map))
(defvar yahtml-mode-map nil
  "Keymap used in yahtml-mode.")
(if yahtml-mode-map nil
  (setq yahtml-mode-map (make-sparse-keymap))
  (define-key yahtml-mode-map YaTeX-prefix yahtml-prefix-map))

(defun yahtml-mode ()
  (interactive)
  (yatex-mode)
  (setq major-mode 'yahtml-mode
	mode-name "yahtml")
  (make-local-variable 'YaTeX-ec)
  (setq YaTeX-ec "")
  (make-local-variable 'YaTeX-struct-begin)
  (setq YaTeX-struct-begin "<%1>")
  (make-local-variable 'YaTeX-struct-end)
  (setq YaTeX-struct-end "</%1>")
  (make-local-variable 'env-table)
  (setq env-table
	'(("HTML") ("HEAD") ("TITLE") ("BODY") ("H1") ("H2") ("H3") ("DL")))
  (make-local-variable 'YaTeX-prefix-map)
  (setq YaTeX-prefix-map yahtml-prefix-map)
  (use-local-map yahtml-mode-map)
  (YaTeX-define-begend-key "bh" "HTML")
  (YaTeX-define-begend-key "bH" "HEAD")
  (YaTeX-define-begend-key "bt" "TITLE")
  (YaTeX-define-begend-key "bb" "BODY")
  (YaTeX-define-begend-key "bd" "DL")
  (YaTeX-define-begend-key "b1" "H1")
  (YaTeX-define-begend-key "b2" "H2")
  (YaTeX-define-begend-key "b3" "H3")
)

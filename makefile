#
# Makefile for YaTeX/yahtml
#

# Edit these variables to be suitable for your site
EMACS	= emacs
#EMACS	= mule
PREFIX	= `${EMACS} -batch --eval '(princ (expand-file-name "../../../.." data-directory))'`
# PREFIX	= /usr/local

#EMACSDIR= ${PREFIX}/lib/${EMACS}
## emacs20 or later
EMACSDIR= ${PREFIX}/share/${EMACS}
## XEmacs
#EMACS	= xemacs
#EMACSDIR= ${PREFIX}/lib/${EMACS}
## Meadow (Sample)
#EMACS	= meadow
#EMACSDIR = c:/usr/local/meadow
## Cocoa(or Carbon)Emacs on Darwin (Sample)
#EMACS	= /Applications/Emacs.app/Contents/MacOS/Emacs
#PREFIX	= /Applications/Emacs.app/Contents/Resources
#EMACSDIR = ${PREFIX}

LISPDIR	= ${EMACSDIR}/site-lisp/yatex
# LISPDIR	= ${EMACSDIR}/site-packages/lisp/yatex
DOCDIR	= ${LISPDIR}/docs
HELPDIR	= ${LISPDIR}/help
INFODIR	= ${PREFIX}/share/info

TAR	= tar
INSTALL	= install -c -m 444
MKDIR	= mkdir -p
INSTINFO= install-info


# Comment out below if you are using Emacs Windows(meadow, etc)
GEO	= -geometry 80x20+0+0

###################
# Do not edit below
###################
# make install		to install YaTeX into public space
# make ajimi		to feel taste
# make ajimi-nw		same as above, but -nw mode
# make package		to create package for relase
# make yahtmlpack	to create package for relase
# make clean		to delete all producted files
# make tag		to add release tags
LISP	= ${LISP18} ${LISP19} ${LISP23}
YAHTML	= yahtml.el
COMMON	= yatexlib.el yatexprc.el yatexhlp.el
LISP18	= yatex.el yatexadd.el yatexgen.el yatexenv.el \
	  ${COMMON} \
	  yatexmth.el yatexhks.el yatexhlp.el yatexflt.el \
	  yatexm-o.el yatexsec.el  yatexhie.el yatexpkg.el ${YAHTML}
LISP19	= yatex19.el
LISP23	= yatex23.el
DOCS	= ${DOCSRC} ${DOCOBJ} ${NEWS}
NEWS	= yatex.new
DOCHTML	= docs/htmlqa docs/htmlqa.eng docs/yahtmlj.tex docs/yahtmle.tex
DOCSRC	= docs/yatexj.tex docs/yatexe.tex \
	  docs/yatex.ref docs/yatexref.eng \
	  docs/yatexadd.doc docs/yatexgen.doc \
	  docs/qanda docs/qanda.eng ${DOCHTML}
DOCOBJ	= docs/yatexj docs/yatexe docs/yahtmlj docs/yahtmle
HELP	= help/YATEXHLP.jp help/YATEXHLP.eng
MANIFEST= manifest
EXTRA	= dir install 00readme makefile readme.meadow.j newpage.rb
DISTRIB = ${EXTRA} ${LISP} ${DOCS} ${MANIFEST} ${HELP}
RCSFILE	= ${LISP} ${NEWS} ${DOCSRC} ${HELP}
YAHTMLLISP = ${YAHTML} ${COMMON}
YAHTMLDIST = ${YAHTMLLISP} install 00readme makefile newpage.rb
PACK	= `ls ${DISTRIB}`
TMPDIR	= /tmp
VERSION = `head -20 yatex.el|awk -F'"' '/revision/{print $$2}'`
PACKDIR	= ${TMPDIR}/yatex${VERSION}

all:
	@echo "Edit this makefile first."
	@echo 'Type "${MAKE} install" to install YaTeX.'
	@echo 'Type "${MAKE} install-yahtml" to install yahtml.'
	@echo 'If you love elc files, type "${MAKE} elc" before ${MAKE} install'
#	@echo "If you don't use X-clinet of Emacs,"
#	@echo 'type "make install-nw" instead.'

install: install-real install-message
#install-yahtml: bytecompile-yahtml
install-yahtml:
	[ -d ${LISPDIR} ] || mkdir ${LISPDIR}
	for f in *.el; do \
	 rm -f ${LISPDIR}/$${f}c; \
	done

	${INSTALL} *.el* ${LISPDIR}

install-real:
	[ -d ${LISPDIR} ] || ${MKDIR} ${LISPDIR}
	[ -d ${HELPDIR} ] || ${MKDIR} ${HELPDIR}
	[ -d ${DOCDIR} ] || ${MKDIR} ${DOCDIR}
	[ -d ${INFODIR} ] || ${MKDIR} ${INFODIR}
	for f in *.el; do \
	 rm -f ${LISPDIR}/$${f}c; \
	done
	${INSTALL} *.el* ${NEWS} ${LISPDIR}
	${INSTALL} ${DOCSRC} ${DOCDIR}
	${INSTALL} ${DOCOBJ} ${INFODIR}
	${INSTALL} ${HELP} ${HELPDIR}

install-message:
	@echo "--------------------------------"
	@echo "If you have install-info command, type '${MAKE} install-info'."
	@echo "If not, add next lines into your site's info dir manually."
	@cat dir
	@echo "--------------------------------"
	@echo "=== INSTALLATION DONE ==="
	@echo "  You might need to add these expression below to your ~/.emacs"
	@echo "  完了. ~/.emacs 等に以下を追加する必要があるかもしれません."
	@echo
	@echo ";;; ------ Startup definitions for YaTeX ------ ;;;"
	@${MAKE} show-init
	@echo ";;; ------------------------------------------- ;;;"
	@echo
	@echo " To get elisp above again, call ${MAKE} command as below."
	@echo " 上記elispを再度得るには以下のように${MAKE}を起動してください."
	@echo " % ${MAKE} $${PREFIX:+PREFIX=$$PREFIX }show-init"

install-info:
	for f in ${DOCOBJ}; do \
	  b=`basename $$f | sed 's,/.*,,'`; \
	  ${INSTINFO} --entry="`grep $$b dir`" --section=TeX \
		--section=Emacs $${f} ${INFODIR}/dir; \
	done

show-init:
	@printf '%s\n' \
	  '(setq auto-mode-alist' \
	  "  (cons (cons \"\\.tex$$\" 'yatex-mode) auto-mode-alist))" \
	  "(autoload 'yatex-mode \"yatex\" \"Yet Another LaTeX mode\" t)" \
	  "(add-to-list 'load-path \"${LISPDIR}\")" \
	  "(setq YaTeX-help-file \"${LISPDIR}/help/YATEXHLP.eng\")"
	@printf '(setq tex-command "%s")\n' \
	 `CMDS='platex pdflatex ptex2pdf lualatex' DFLT=latex \
	  ${MAKE} -s search-cmd`
	@printf '(setq dvi2-command "%s")\n' \
	 `CMDS='pxdvi xdvik kxdvi dviout texworks' DFLT=xdvi \
	  ${MAKE} -s search-cmd`
	@printf '(setq tex-pdfview-command "%s")\n' \
	 `CMDS='evince mupdf xpdf kpdf texworks sumatrapdf' \
	  DFLT=acroread \
	  ${MAKE} -s search-cmd`

show-init2:
	@${MAKE} LISPDIR=$$PWD show-init

search-cmd:
	@for f in $$CMDS; do \
	  type $$f >/dev/null 2>&1 && echo $$f && exit 0; done; echo $$DFLT

install-nw: bytecompile-nw install-real

elc:	bytecompile

bytecompile: lp
	if [ "$$DISPLAY"x = ""x ]; then \
		echo "Set DISPLAY environment variable!!"; exit 1; fi
	${EMACS} -q ${GEO} -l ./yatexlib.el -e bcf-and-exit ${LISP}

bytecompile-nw: lp1
	${EMACS} -batch -l ./yatexlib.el -e batch-byte-compile ${LISP18}

bytecompile-yahtml:
	if [ "$$DISPLAY"x = ""x ]; then \
		echo "Set DISPLAY environment variable!!"; exit 1; fi
	${EMACS} -q -g 80x20+0+0 -l ./yatexlib.el -e bcf-and-exit ${YAHTMLLISP}

lp:
	echo '(setq load-path (cons "." load-path))'	> lp.el
	echo '(load-file "./yatexlib.el")'		>>lp.el

lp1:	lp
	echo '(load-file "./yatex.el")'			>>lp.el

lp2:
	echo '(setq load-path (cons "'`pwd`'" load-path))'		>>lp.el
	echo '(setq auto-mode-alist'					>>lp.el
	echo '(cons (cons "\\.tex" '"'yatex-mode) auto-mode-alist))"	>>lp.el
	echo '(load-library "yatex")'					>>lp.el

ajimi: lp lp2
	${EMACS} -l ./lp.el -e yatex-mode

ajimi-nw: lp lp2
	${EMACS} -nw -l ./lp.el -e yatex-mode

clean:
	rm -f *.elc *~ lp.el

info: docs/yatexj docs/yatexe docs/yahtmlj docs/yahtmle

docs/yatexj: docs/yatexj.tex
	(cd docs; ${EMACS} -batch -l ../yatexlib.el -e tfb-and-exit yatexj.tex)

docs/yatexe: docs/yatexe.tex
	(cd docs; ${EMACS} -batch -l ../yatexlib.el -e tfb-and-exit yatexe.tex)

docs/yahtmlj: docs/yahtmlj.tex
	(cd docs;${EMACS} -batch -l ../yatexlib.el -e tfb-and-exit yahtmlj.tex)

docs/yahtmle: docs/yahtmle.tex
	(cd docs;${EMACS} -batch -l ../yatexlib.el -e tfb-and-exit yahtmle.tex)

package: info
	@-mkdir ${PACKDIR}
	@tar cf - ${PACK} | (cd ${PACKDIR}; tar xf -)
	( version=${VERSION}; cd ${TMPDIR}; \
	     ${TAR} vzcf ${TMPDIR}/yatex$$version.tar.gz yatex$$version)

yahtmlpack:
	@-mkdir ${PACKDIR}
	@tar cf - ${YAHTMLDIST} | (cd ${PACKDIR}; tar xf -)
	( version=${VERSION}; cd ${TMPDIR}; \
	     ${TAR} vzcf ${TMPDIR}/yahtml$$version.tar.gz yatex$$version)

tag:
	hg tag yatex-${VERSION}
# ci:
# 	ci -r${VERSION} -sRel -f ${RCSFILE}
# 	ci -u${VERSION} makefile 00readme

# co:
# 	co ${RCSFILE}


RSYNCDIR	= ${HOME}/http/yatex/rsync/yatex
sync:	
	-hg push
	-hg push git
	-hg push cvs
	(cd ${RSYNCDIR} && hg up -Cv dev && cvs ci -m '')

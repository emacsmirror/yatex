#
# Makefile for YaTeX.
#

LISP	= comment.el yatex.el yatexadd.el yatexgen.el yatexenv.el yatexlib.el \
	  yatexmth.el yatexhks.el yatexhlp.el yatexprc.el \
	  yatexm-o.el yatexsec.el
#	\  yahatml.el 
DOCS	= yatex.new docs/yatexj docs/yatexj.tex docs/yatexe docs/yatexe.tex \
	  docs/yatex.ref docs/yatexadd.doc docs/yatexgen.doc docs/qanda
HELP	= help/YATEXHLP.jp
MANIFEST= manifest
EXTRA	= dir #00readme
DISTRIB = $(EXTRA) $(LISP) $(DOCS) $(MANIFEST) $(HELP)
RCSFILE	= $(LISP) $(DOCS) $(HELP)
PACK	= `echo $(DISTRIB)|xargs ls`
TAR	= gtar
TMPDIR	= /tmp
VERSION = `head yatex.el|awk '/rev\./{print $$4}'`
PACKDIR	= $(TMPDIR)/yatex$(VERSION)
EMACS	= mule

all:

install:
	$(EMACS) -batch -e batch-byte-compile $(LISP)
	#install -c * $(MYELISPLIB)

package:
	@-mkdir $(PACKDIR)
	@tar cf - $(PACK) | (cd $(PACKDIR); tar xf -)
	find $(PACKDIR) -type f -exec chmod -x '{}' \;
	( version=$(VERSION); cd $(TMPDIR); \
	     $(TAR) vzcf $(TMPDIR)/yatex$$version.tar.gz yatex$$version)

ci:
	ci -r$(VERSION) -sRel -f $(RCSFILE)
	ci -r$(VERSION) -u makefile
co:
	co $(RCSFILE)
co-l:
	co -l $(RCSFILE)

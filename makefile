#
# Makefile for YaTeX.
#

MVER	= 1.50
LISP	= comment.el yatex.el yatexadd.el yatexgen.el yatexenv.el yatexlib.el \
	  yatexmth.el yatexhks.el yatexhlp.el yatexprc.el \
	  yatexm-o.el yatexsec.el
#	\  yahatml.el 
DOCS	= $(DOCSRC) $(DOCOBJ)
DOCSRC	= yatex.new docs/yatexj.tex docs/yatexe.tex \
	  docs/yatex.ref docs/yatexadd.doc docs/yatexgen.doc docs/qanda
DOCOBJ	= docs/yatexj docs/yatexe
HELP	= help/YATEXHLP.jp
MANIFEST= manifest
EXTRA	= dir install 00readme
DISTRIB = $(EXTRA) $(LISP) $(DOCS) $(MANIFEST) $(HELP)
RCSFILE	= $(LISP) $(DOCSRC) $(HELP)
PACK	= `echo $(DISTRIB)|xargs ls`
TAR	= gtar
TMPDIR	= /tmp
VERSION = `head yatex.el|awk '/rev\./{print $$4}'`
PACKDIR	= $(TMPDIR)/yatex$(VERSION)
EMACS	= mule
INSTALL	= install -c

all:

install:
	$(EMACS) -batch -e batch-byte-compile $(LISP)
	#$(INSTALL) * $(MYELISPLIB)

package:
	@-mkdir $(PACKDIR)
	@tar cf - $(PACK) | (cd $(PACKDIR); tar xf -)
	find $(PACKDIR) -type f -exec chmod -x '{}' \;
	( version=$(VERSION); cd $(TMPDIR); \
	     $(TAR) vzcf $(TMPDIR)/yatex$$version.tar.gz yatex$$version)

ci:
	ci -r$(VERSION) -sRel -f $(RCSFILE)
	ci -u$(VERSION) makefile

co:
	co $(RCSFILE)

co-l:
	co -l $(RCSFILE)

tci:
	ci -l$(VERSION).0 -Ncurrent $(RCSFILE) makefile

dostci:
	ci -l$(MVER).0 -Ncurrent @rcsfile

gohome:
	zip -u -r /com/okoma/yuuji/tmp/dosconv/yatex.zip . \
	-x '*RCS/*' -x 'texinfo/*'

AM_YFLAGS=-d
BUILT_SOURCES=rpal.h
EXTRA_DIST=rpal.scm rpal.1 rpal.1.xml
MAINTAINERCLEANFILES=rpal.1
SUBDIRS = m4 doc tests lib
bin_PROGRAMS = rpal
dist_lisp_DATA=rpal-mode.el
man1_MANS = rpal.1
rpal_SOURCES = rpal.y lex.l
rpal_CFLAGS = @GUILE_CFLAGS@
rpal_LDFLAGS = @GUILE_LDFLAGS@ -L./lib
rpal_LDADD = @LEXLIB@ -lcompat

if HAVE_XSLTPROC
rpal.1: rpal.1.xml
	$(XSLTPROC) http://docbook.sf.net/release/xsl/1.69.1/manpages/docbook.xsl rpal.1.xml
else 
if HAVE_SAXON
rpal.1: rpal.1.xml
	$(SAXON) rpal.1.xml http://docbook.sf.net/release/xsl/1.69.1/manpages/docbook.xsl
endif
endif

install-data-hook:
	$(INSTALL) -D rpal.scm @GUILE_SITE@/rpal.scm

uninstall-hook:
	rm -f @GUILE_SITE@/rpal.scm

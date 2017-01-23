
####
#### Copyright (C) 2016
#### Free Software Foundation, Inc.

#### This file is part of GNU Guile-CV

#### GNU Guile-CV is free software; you can redistribute it and/or
#### modify it under the terms of the GNU Lesser General Public
#### License as published by the Free Software Foundation; either
#### version 3 of the License, or (at your option) any later version.

#### GNU Guile-CV is distributed in the hope that it will be
#### useful, but WITHOUT ANY WARRANTY; without even the implied
#### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#### See the GNU Lesser General Public License for more details.

#### You should have received a copy of the GNU Lesser General Public
#### License along with GNU Guile-CV.  If not, see
#### <https://www.gnu.org/licenses/lgpl.html>.
####


include $(top_srcdir)/am/guile.mk

moddir=$(GUILE_GLOBAL_SITE)
godir=$(GUILE_SITE_CCACHE)

pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA=meta/guile-cv-1.0.pc

SOURCES = \
	cv/support/modules.scm			\
	cv/support/goops.scm			\
	cv/support/g-export.scm			\
	cv/support/utils.scm			\
	cv/support/float.scm			\
	cv/support/push.scm			\
	cv/support/keyword.scm			\
	cv/support/f32vector.scm		\
	cv/support.scm				\
	cv/init.scm				\
	cv/idata.scm				\
	cv/imgproc.scm				\
	cv/impex.scm				\
	cv/filters.scm				\
	cv/morphology.scm			\
	cv/segmentation.scm			\
	cv/properties.scm				\
	cv/adds.scm				\
	cv/utils.scm				\
	cv.scm

# NOCOMP_SOURCES= \
#	$(wildcard cv/images/*)
# 	$(wildcard cv/latex/*)	\
# 	$(wildcard cv/gtkrc/*)	\
# 	$(shell find $(top_srcdir)/cv/pof -type f)

EXTRA_DIST = \
	AUTHORS				\
	ChangeLog			\
	COPYING				\
	INSTALL				\
	LICENSE				\
	NEWS				\
	README				\
	pre-inst-env.in			\
	meta/guile-cv-1.0.pc.in	\
	$(wildcard am/*)		\
	$(wildcard m4/*)		\
	$(SOURCES)
#	$(NOCOMP_SOURCES)

# bin_SCRIPTS = cv/cv

CLEANFILES = \
	meta/guile-cv-1.0.pc		\
	$(GOBJECTS)
# 	$(bin_SCRIPTS)

ETAGS_ARGS = \
	$(SOURCES)
# guile-cv

SUBDIRS = \
	test-suite			\
	doc

distclean-local:
	rm -f Makefile
	rm -f Makefile.in
	rm -f aclocal.m4
	rm -rf autom4te.cache
	rm -rf build-aux
	rm -f configure
	rm -f *~
	rm -f guile-cv-*.tar.gz
	rm -f guile-cv-*.tar.gz.sig
	rm -rf doc/guile-cv
	rm -f doc/*.info
	rm -f doc/*.pdf
	rm -f doc/*~
	rm -f doc/*.aux doc/*.cp doc/*.cps doc/*.fn doc/*.fns doc/*.ky
	rm -f doc/*.log doc/*.pg doc/*.toc doc/*.tp doc/*.vr doc/*.vrs
	rm -f doc/Makefile
	rm -f doc/Makefile.in
	rm -f doc/stamp-vti
	rm -f cv/*~
	rm -f test-suite/Makefile
	rm -f test-suite/Makefile.in


# * .PHONY: gen-pof:

# Makefile.am ends here


####
#### Copyright (C) 2016 - 2017
#### Free Software Foundation, Inc.

#### This file is part of GNU Guile-CV.

#### GNU Guile-CV is free software; you can redistribute it and/or
#### modify it under the terms of the GNU General Public License as
#### published by the Free Software Foundation; either version 3 of the
#### License, or (at your option) any later version.

#### GNU Guile-CV is distributed in the hope that it will be useful, but
#### WITHOUT ANY WARRANTY; without even the implied warranty of
#### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#### General Public License for more details.

#### You should have received a copy of the GNU General Public License
#### along with GNU Guile-CV.  If not, see
#### <https://www.gnu.org/licenses/gpl.html>.
####


#include $(top_srcdir)/am/guile.mk


lib_LTLIBRARIES = \
	libguile-cv.la


nodist_libguile_cv_SOURCES = \
	libguile-cv.c

libguile_cv_SOURCES = \
	libguile-cv.c		\
	libguile-cv.h

libguile_cv_CFLAGS = \
	$(AM_CFLAGS) $(GUILE_CFLAGS)

libguile_cv_LIBADD = $(GUILE_LIBS)

libguile_cv_LDFLAGS = -module

# Makefile.am ends here

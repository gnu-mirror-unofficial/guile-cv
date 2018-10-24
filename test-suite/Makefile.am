
####
#### Copyright (C) 2016 - 2018
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


TESTS = \
	tests/support.scm	\
	tests/cv.scm		\
	tests/imgproc.scm	\
	tests/segmentation.scm	\
	tests/texture.scm

TEST_EXTENSIONS = .scm

TESTS_ENVIRONMENT = \
	$(abs_top_builddir)/pre-inst-env		\
	$(GUILE) -L $(abs_top_builddir)/test-suite	\
		 --no-auto-compile

EXTRA_DIST = \
	$(TESTS)

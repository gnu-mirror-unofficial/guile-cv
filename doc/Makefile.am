
####
#### Copyright (C) 2016 - 2017
#### David Pirotte <david at altosw dot be>

#### This file is part of Guile-CV

#### Guile-CV is free software; you can redistribute it and/or modify
#### it under the terms of the GNU Lesser General Public License as
#### published by the Free Software Foundation; either version 3 of
#### the License, or (at your option) any later version.

#### Guile-CV is distributed in the hope that it will be useful, but
#### WITHOUT ANY WARRANTY; without even the implied warranty of
#### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#### Lesser General Public License for more details.

#### You should have received a copy of the GNU Lesser General Public
#### License along with Guile-CV.  If not, see
#### <https://www.gnu.org/licenses/lgpl.html>.
####


# AUTOMAKE_OPTIONS = gnu

info_TEXINFOS = guile-cv.texi

DOC_IMAGES = \
	$(wildcard images/*)
#	images/pp-17-bf.png		\
#	images/pp-17-bin.png		\
#	images/pp-17-label.png		\
#	images/pp-17-label-all.png

DOC_CSS = \
	$(wildcard css/*)

guile_cv_TEXINFOS = \
  version.texi			\
  variables.texi		\
  preface.texi     		\
  introduction.texi		\
  using-guile-cv.texi		\
  api-abreviations.texi		\
  api-overview.texi		\
  api-cv-impex.texi		\
  api-cv-idata.texi		\
  api-cv-imgproc.texi		\
  api-cv-filters.texi		\
  api-cv-morphology.texi	\
  api-cv-segmentation.texi	\
  api-cv-properties.texi	\
  api-cv-utils.texi		\
  api-support-modules.texi	\
  api-support.texi		\
  api.texi			\
  indices.texi			\
  fdl.texi			\
  $(DOC_IMAGES)			\
  $(DOC_CSS)

nobase_doc_DATA = \
	$(DOC_IMAGES)		\
	$(DOC_CSS)

EXTRA_DIST = \
	$(DOC_IMAGES)		\
	$(DOC_CSS)

AM_MAKEINFOHTMLFLAGS = --css-ref=../css/manual.css

printenv:
	printf '$(DOC_IMAGES)\n'

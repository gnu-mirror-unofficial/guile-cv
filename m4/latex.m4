##
## Autoconf macros for working with latex.
##
## Copyright (C) 2015 - 2021 Free Software Foundation, Inc.
##
## This library is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 3 of the
## License, or (at your option) any later version.
##
## This library is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this library; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA

# serial 1


###
### Index
###

## LATEX_CHECK
## LATEX_DOCUMENT_CLASS_CHECK
## LATEX_DOCUMENT_CLASS_AVAILABLE
## LATEX_DOCUMENT_CLASS_REQUIRED
## LATEX_PACKAGE_CHECK
## LATEX_PACKAGE_AVAILABLE
## LATEX_PACKAGE_REQUIRED


###
### Code
###

## NOTE: latex.m4 uses Guile and requires the guile.m4 macros, which
## is part of Guile and installed in /your/prefix/share/aclocal, check
## the value of your ACLOCAL_FLAGS environment variable.  If you
## prefer, you may also copy all the m4 macro files you need for your
## project in in /your/project/m4 subdirectory, this way the autotool
## chain never fails to find them, but updating them as new versions
## come out is under your entire responsability.  In this case don't
## forget to call AC_CONFIG_MACRO_DIRS([m4]) in your configure.ac
## project file.


###
### LATEX_CHECK
###   -- using Guile, imports (build-aux latex), evaluate the
###      guile scheme code and capture the return value
###

# Usage: LATEX_CHECK(var, check)
#
# @var{var} is a shell variable name to be set to the return value.
# @var{check} is a Guile Scheme expression, evaluated with "$GUILE -L
# "$srcdir" -c", importing the (build-aux latex) module, and returning
# either 0 or non-#f to indicate the check passed.  Non-0 number or #f
# indicates failure.  Avoid using the character "#" since that
# confuses autoconf.
#
AC_DEFUN([LATEX_CHECK],
 [AC_REQUIRE([GUILE_PROGS])
  $GUILE -L "$srcdir" -c "(use-modules (build-aux latex)) $2" > /dev/null 2>&1
  $1=$?
 ])


###
### LATEX_DOCUMENT_CLASS_CHECK
###   -- check if a latex document class is available
###

# Usage: LATEX_DOCUMENT_CLASS_CHECK(var, class)
#
# @var{var} is a shell variable name to be set to "yes" or "no".
# @var{class} is a symbol, like: numprint.
#
AC_DEFUN([LATEX_DOCUMENT_CLASS_CHECK],
 [AC_MSG_CHECKING([for \documentclass{$2}])
  LATEX_CHECK($1, (exit ((lambda () (latex-check-for-document-class \"$2\")))))
  if test "$$1" = "0" ; then $1=yes ; else $1=no ; fi
  AC_MSG_RESULT($$1)
 ])


###
### LATEX_DOCUMENT_CLASS_AVAILABLE
###   -- check availability of a latex document_class
###

# Usage: LATEX_DOCUMENT_CLASS_AVAILABLE(var, document_class)
#
# @var{var} is a shell variable name to be set to "yes" or "no".
# @var{document_class} is a symbol, like: article.
#
# @var{var} is marked for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([LATEX_DOCUMENT_CLASS_AVAILABLE],
 [LATEX_DOCUMENT_CLASS_CHECK($1,$2,0)
  AC_SUBST($1)
 ])


###
### LATEX_DOCUMENT_CLASS_REQUIRED
###   -- fail if a latex document class is unavailable
###

# Usage: LATEX_DOCUMENT_CLASS_REQUIRED(class)
#
# @var{class} is a symbol like: article.
#
AC_DEFUN([LATEX_DOCUMENT_CLASS_REQUIRED],
 [LATEX_DOCUMENT_CLASS_AVAILABLE(ac_latex_document_class_required, $1)
  if test "$ac_latex_document_class_required" = "no" ; then
    AC_MSG_ERROR([required latex document class not found: ($1)])
  fi
 ])


###
### LATEX_PAKAGE_CHECK
###   -- check if a latex package is available
###

# Usage: LATEX_PACKAGE_CHECK(var, package)
#
# @var{var} is a shell variable name to be set to "yes" or "no".
# @var{package} is a symbol, like: numprint.
#
AC_DEFUN([LATEX_PACKAGE_CHECK],
 [AC_MSG_CHECKING([for \usepackage{$2}])
  LATEX_CHECK($1, (exit ((lambda () (latex-check-for-package \"$2\")))))
  if test "$$1" = "0" ; then $1=yes ; else $1=no ; fi
  AC_MSG_RESULT($$1)
 ])


###
### LATEX_PACKAGE_AVAILABLE
###   -- check availability of a latex package
###

# Usage: LATEX_PACKAGE_AVAILABLE(var, package)
#
# @var{var} is a shell variable name to be set to "yes" or "no".
# @var{package} is a symbol, like: numprint.
#
# @var{var} is marked for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([LATEX_PACKAGE_AVAILABLE],
 [LATEX_PACKAGE_CHECK($1,$2,0)
  AC_SUBST($1)
 ])


###
### LATEX_PACKAGE_REQUIRED
###   -- fail if a latex package is unavailable
###

# Usage: LATEX_PACKAGE_REQUIRED(package)
#
# @var{package} is a symbol like: numprint.
#
AC_DEFUN([LATEX_PACKAGE_REQUIRED],
 [LATEX_PACKAGE_AVAILABLE(ac_latex_package_required, $1)
  if test "$ac_latex_package_required" = "no" ; then
    AC_MSG_ERROR([required latex package not found: ($1)])
  fi
 ])

## latex.m4 ends here

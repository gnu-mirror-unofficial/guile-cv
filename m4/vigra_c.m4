## Autoconf macros for working with Vigra_C.
##
##   Copyright (C) 2017 Free Software Foundation, Inc.
##
## This library is free software; you can redistribute it and/or
## modify it under the terms of the GNU Lesser General Public License
## as published by the Free Software Foundation; either version 3 of
## the License, or (at your option) any later version.
##
## This library is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public
## License along with this library; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA

# serial 10

## Index
## -----
##
## VIGRA_C_LIB -- vigra_c library checks

## Code
## ----

###
### VIGRA_C_CHECK
###   -- using Guile, evaluate the guile scheme code and capture the
###      return value
###

# Usage: VIGRA_C_CHECK(var, check)
#
# @var{var} is a shell variable name to be set to the return value.
# @var{check} is a Guile Scheme expression, evaluated with "$GUILE -c"
# and returning either 0 to indicate the check passed, -1 indicates
# failure.  Avoid using the character "#" since that confuses
# autoconf.
#
AC_DEFUN([VIGRA_C_CHECK],
 [$GUILE -c "(exit (catch #t
          (lambda ()
            $2
            0)
          (lambda (key . args)
            -1)))" > /dev/null 2>&1
  $1=$?
 ])

###
### VIGRA_C_FUNCTION_CHECK
###   -- using Guile, checks that a function name is in libvigra_c
###

# Usage: VIGRA_C_FUNCTION_CHECK(var, name)
#
# @var{var} is a shell variable name to be set to the return value.
# @var{name} is the name of a vigra_c function. Using Guile, this
# macro checks that @var{name} is effectively in libvigra_c.
#
AC_DEFUN([VIGRA_C_FUNCTION_CHECK],
 [$GUILE -c "(exit (catch #t
        (lambda ()
          (dynamic-func \"$2\" (dynamic-link \"libvigra_c\"))
          0)
        (lambda (key . args)
          -1)))" > /dev/null 2>&1
  $1=$?
 ])

AC_DEFUN([VIGRA_C_LIB],
 [AC_PATH_PROG(GUILE, guile)
  if test "$GUILE" = "" ; then
      AC_MSG_ERROR([libvigra_c checks need guile, not found])
  fi
  AC_SUBST(GUILE)
  AC_MSG_CHECKING([for libvigra_c])
  VIGRA_C_CHECK(VIGRA_C, (dynamic-link \"libvigra_c\"))
#  AC_MSG_NOTICE([ vigra_c is $VIGRA_C])
  if test $VIGRA_C != "0" ; then
      AC_MSG_ERROR([libvigra_c not found])
  fi
  AC_MSG_RESULT(yes)
 ])

AC_DEFUN([VIGRA_C_FUNCTION],
 [AC_REQUIRE([VIGRA_C_LIB])
  if test "$GUILE" = "" ; then
      AC_MSG_ERROR([libvigra_c checks need guile, not found])
  fi
  AC_SUBST(GUILE)
  AC_MSG_CHECKING([for $1])
  VIGRA_C_FUNCTION_CHECK(C_FUNCTION, $1)
#  AC_MSG_NOTICE([ vigra_c is $C_FUNCTION])
  if test $C_FUNCTION != "0" ; then
      AC_MSG_ERROR([$1 not libvigra_c])
  fi
  AC_MSG_RESULT(yes)
 ])

## vigra_c.m4 ends here


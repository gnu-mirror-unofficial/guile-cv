## Autoconf macros for working with Vigra.
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
## VIGRA_LIB -- checks for vigra-config
##           -- uses it to check the installed vigra library version

## Code
## ----

AC_DEFUN([VIGRA_LIB],
 [AC_PATH_PROG(VIGRA_CONFIG, vigra-config)
  _vigra_required_version="m4_default([$1], [1.11])"
  if test "$VIGRA_CONFIG" = "" ; then
      AC_MSG_ERROR([vigra-config required but not found])
  fi
  AC_SUBST(VIGRA_CONFIG)

  _vigra_effective_version=`$VIGRA_CONFIG --version`

  AC_MSG_CHECKING([for Vigra version >= $_vigra_required_version])

  _required_major_version=`echo $_vigra_required_version | cut -d . -f 1`
  _required_minor_version=`echo $_vigra_required_version | cut -d . -f 2`
  _required_micro_version=`echo $_vigra_required_version | cut -d . -f 3`

  _effective_major_version=`echo $_vigra_effective_version | cut -d . -f 1`
  _effective_minor_version=`echo $_vigra_effective_version | cut -d . -f 2`
  _effective_micro_version=`echo $_vigra_effective_version | cut -d . -f 3`

  if test "$_effective_major_version" -gt "$_required_major_version"; then
      true
  elif test "$_effective_major_version" -eq "$_required_major_version"; then
      if test "$_effective_minor_version" -gt "$_required_minor_version"; then
          true
      elif test "$_effective_minor_version" -eq "$_required_minor_version"; then
          if test -n "$_required_micro_version"; then
              if test "$_effective_micro_version" -lt "$_required_micro_version"; then
                  AC_MSG_ERROR([Vigra $_vigra_required_version required, but $_vigra_effective_version found])
              fi
          fi
      else
          AC_MSG_ERROR([Vigra $_vigra_required_version required, but $_vigra_effective_version found])
      fi
  else
      AC_MSG_ERROR([Vigra $_vigra_required_version required, but $_vigra_effective_version found])
  fi
  AC_MSG_RESULT([$_effective_prog_version])
])

## vigra.m4 ends here




####
#### Copyright (C) 2019 - 2021
#### Free Software Foundation, Inc.

#### Autoconf macro to check the presence and version of the makeinfo
#### program.

#### This library is free software; you can redistribute it and/or
#### modify it under the terms of the GNU Lesser General Public
#### License as published by the Free Software Foundation; either
#### version 3 of the License, or (at your option) any later version.

#### This library is distributed in the hope that it will be useful,
#### but WITHOUT ANY WARRANTY; without even the implied warranty of
#### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#### Lesser General Public License for more details.

#### You should have received a copy of the GNU Lesser General Public
#### License along with this library.  If not, see
#### <https://www.gnu.org/licenses/lgpl.html>.
####


## Index
## -----

## MAKEINFO_PROG -- check for makeinfo presence and version


## Code
## ----

## MAKEINFO_PROG -- check for makeinfo presence and version

# Usage: MAKEINFO_PROG([VERSION])

# This macro looks for the program @code{makeinfo}

# By default, this macro will search for the a version of makeinfo >=
# 6.5. An x.y can be specified. If an older version is found, the macro
# will signal an error.

AC_DEFUN([MAKEINFO_PROG],
 [_makeinfo_required_version="m4_default([$1], [])"
  if test -z "$_makeinfo_required_version"; then
    _makeinfo_required_version=6.5
  fi
  _major_version=`echo $_makeinfo_required_version | cut -d . -f 1`
  _minor_version=`echo $_makeinfo_required_version | cut -d . -f 2`

  AC_PATH_PROGS(MAKEINFO, makeinfo)
  if test -z "$MAKEINFO"; then
      AC_MSG_ERROR([makeinfo required but not found])
  fi

  AC_MSG_CHECKING([for Makeinfo version >= $_makeinfo_required_version])
  _texinfo_version=`"$MAKEINFO" --version | head -n 1`
  _makeinfo_version=`echo $_texinfo_version | cut -d " " -f 4`
  _makeinfo_major_version=`echo $_makeinfo_version | cut -d . -f 1`
  _makeinfo_minor_version=`echo $_makeinfo_version | cut -d . -f 2`

  if test "$_makeinfo_major_version" -gt "$_major_version"; then
    true
  elif test "$_makeinfo_major_version" -eq "$_major_version"; then
    if test "$_makeinfo_minor_version" -lt "$_minor_version"; then
      AC_MSG_ERROR([Makeinfo $_makeinfo_required_version required, but $_makeinfo_version found])
    fi
  else
    AC_MSG_ERROR([Makeinfo $_makeinfo_required_version required, but $_makeinfo_version found])
  fi
  AC_MSG_RESULT([$_makeinfo_version])
 ])

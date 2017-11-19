/*  -*- mode: C; coding: utf-8 -*-

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

*/

int float_to_int_c (float f)
{
  int i;

  i = (int)f;
  return (i);
}

int f32vector_min_c (float *v, int n_cell, float *r)
{
  int i;

  r[0] = v[0];
  r[1] = 0.0;
  for (i = 1; i < n_cell; i++) {
    if (v[i] < r[0]) {
        r[0] = v[i];
        r[1] = (float)i;
    }
  }
  return 1;
}

int f32vector_max_c (float *v, int n_cell, float *r)
{
  int i;

  r[0] = v[0];
  r[1] = 0.0;
  for (i = 1; i < n_cell; i++) {
    if (v[i] > r[0]) {
      r[0] = v[i];
      r[1] = (float)i;
    }
  }
  return 1;
}

int f32vector_range_c (float *v, int n_cell, float *r)
{
  int i;

  r[0] = r[2] = v[0];
  r[1] = r[3] = 0.0;
  for (i = 1; i < n_cell; i++) {
    if (v[i] < r[0]) {
      r[0] = v[i];
      r[1] = (float)i;
    }
    if (v[i] > r[2]) {
      r[2] = v[i];
      r[3] = (float)i;
    }
  }
  return 1;
}

int f32vector_scrap_c (float *chan,
                       float *l_chan,
                       int n_cell,
                       int *scrap_cache,
                       float *to)
{
  int i, val;

  for (i = 0; i < n_cell; i++) {
    val = (int)l_chan[i];
    if ((val == 0) | (scrap_cache[val] == 1)) {
      to[i] = 0;
    }
    else {
      to[i] = chan[i];
    }
  }
  return 1;
}

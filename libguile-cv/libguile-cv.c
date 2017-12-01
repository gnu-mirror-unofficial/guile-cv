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

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <limits.h>
/* #include <libguile.h> */


/*
 * misc.
 *
*/

size_t pointer_address_size_c ()
{
  size_t n = sizeof(float *) * CHAR_BIT;

  return n;
}


/*
 * floats
 *
*/

int float_to_int_c (float f)
{
  int i;

  i = (int)f;
  return (i);
}

int float_equal_c (float f1, float f2, float prec)
{
  if ((abs (f1 - f2)) <= prec) {
    return 1;
  }
  else {
    return 0;
  }
}


/*
 * f32vectors
 *
*/

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
  return 0;
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
  return 0;
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
  return 0;
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
  return 0;
}

int f32vector_add_value_c (float *v,
                           int n_cell,
                           float val,
                           float *to)
{
  int i;

  for (i = 0; i < n_cell; i++) {
     to[i] = v[i] + val;
  }
  return 0;
}

int f32vector_add_vectors_c (float *to,
                             int n_cell,
                             float *v_ptr[],
                             int n_vectors)
{
  int i, j, sum;

  for (i = 0; i < n_cell; i++) {
    sum = 0;
    for (j = 0; j < n_vectors; j++) {
      sum += v_ptr[j][i];
    }
    to[i] = sum;
  }
  return 0;
}

int f32vector_subtract_value_c (float *v,
                                int n_cell,
                                float val,
                                float *to)
{
  int i;

  for (i = 0; i < n_cell; i++) {
     to[i] = v[i] - val;
  }
  return 0;
}

int f32vector_subtract_vectors_c (float *to,
                                  int n_cell,
                                  float *v_ptr[],
                                  int n_vectors)
{
  int i, j, result;

  for (i = 0; i < n_cell; i++) {
    result = v_ptr[0][i];
    for (j = 1; j < n_vectors; j++) {
      result -= v_ptr[j][i];
    }
    to[i] = result;
  }
  return 0;
}

int f32vector_multiply_value_c (float *v,
                                int n_cell,
                                float val,
                                float *to)
{
  int i;

  for (i = 0; i < n_cell; i++) {
     to[i] = v[i] * val;
  }
  return 0;
}

int f32vector_divide_value_c (float *v,
                              int n_cell,
                              float val,
                              float *to)
{
  int i;

  if (val == 0) {
    printf ("ERROR: Attempt to divide by 0");
    return -1;
  }
  else {
    for (i = 0; i < n_cell; i++) {
      to[i] =  v[i] / val;
    }
    return 0;
  }
}

int f32vector_and_vectors_c (float *to,
                             int n_cell,
                             float *v_ptr[],
                             int n_vectors)
{
  int i, j, bool;

  for (i = 0; i < n_cell; i++) {
    bool = 1;
    for (j = 0; j < n_vectors; j++) {
      if (v_ptr[j][i] == 0) {
        bool = 0;
      }
    }
    if (bool == 1) {
      to[i] = v_ptr[0][i];
    }
    else {
      to[i] = 0;
    }
  }
  return 0;
}

int f32vector_or_vectors_c (float *to,
                            int n_cell,
                            float *v_ptr[],
                            int n_vectors)
{
  int i, j, val;

  for (i = 0; i < n_cell; i++) {
    val = 0;
    for (j = 0; j < n_vectors; j++) {
      if ((val == 0) && (v_ptr[j][i] > 0)) {
        val = v_ptr[j][i];
        }
    }
    to[i] = val;
  }
  return 0;
}

int f32vector_equal_vectors_c (int n_cell,
                               float *v_ptr[],
                               int n_vectors,
                               float prec)
{
  int i, j, val;

  for (i = 0; i < n_cell; i++) {
    val =  v_ptr[0][i];
    for (j = 1; j < n_vectors; j++) {
      if (prec == 0) {
        if (v_ptr[j][i] != val) {
          return -1;
        }
      }
      else {
        if (float_equal_c (v_ptr[j][i], val, prec) == 0) {
          return -1;
        }
      }
    }
  }
  return 0;
}

int f32vector_binary_vectors_c (int n_cell,
                                float *v_ptr[],
                                int n_vectors)
{
  int i, j;

  for (i = 0; i < n_cell; i++) {
    for (j = 0; j < n_vectors; j++) {
      if ((v_ptr[j][i] == 0) || (v_ptr[j][i] == 255)) {
      }
      else {
        return -1;
      }
    }
  }
  return 0;
}

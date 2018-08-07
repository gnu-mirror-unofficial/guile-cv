/*  -*- mode: C; coding: utf-8 -*-

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

*/


/*
 * misc.
 *
*/

size_t pointer_address_size_c ();


/*
 * floats
 *
*/

int float_to_int_c (float f);
int float_equal_c (float f1, float f2, float prec);


/*
 * bounding box
 *
*/

int bb_intersect_c (int l_one,
                    int t_one,
                    int r_one,
                    int b_one,
                    int l_two,
                    int t_two,
                    int r_two,
                    int b_two);


/*
 * f32vectors
 *
*/

int f32vector_min_c (float *v, int len, float *r);
int f32vector_max_c (float *v, int len, float *r);
int f32vector_range_c (float *v, int len, float *r);

int f32vector_scrap_c (float *chan,
                       float *l_chan,
                       int n_cell,
                       int *scrap_cache,
                       float *to);

int f32vector_scrap_in_place_c (float *chan,
                                float *l_chan,
                                int n_cell,
                                int *scrap_cache);

int f32vector_threshold_c (float *to,
                           int n_cell,
                           float *v_ptr[],
                           int n_vectors,
                           float threshold
                           int bg);

int f32vector_fill_holes_c (float *labels,
                            int n_cell,
                            float bg_label);

int f32vector_rgb_to_gray_c (float *to,
                             int n_cell,
                             float *r,
                             float *g,
                             float *b);

int f32vector_add_value_c (float *v,
                           int n_cell,
                           float val,
                           float *to);

int f32vector_add_vectors_c (float *to,
                             int n_cell,
                             float *v_ptr[],
                             int n_vectors);

int f32vector_subtract_value_c (float *v,
                                int n_cell,
                                float val,
                                float *to);

int f32vector_subtract_vectors_c (float *to,
                                  int n_cell,
                                  float *v_ptr[],
                                  int n_vectors);

int f32vector_multiply_value_c (float *v,
                                int n_cell,
                                float val,
                                float *to);

int f32vector_times_vectors_c (float *to,
                               int n_cell,
                               float *v_ptr[],
                               int n_vectors);

int f32vector_divide_value_c (float *v,
                              int n_cell,
                              float val,
                              float *to);

int f32vector_and_vectors_c (float *to,
                             int n_cell,
                             float *v_ptr[],
                             int n_vectors);

int f32vector_or_vectors_c (float *to,
                            int n_cell,
                            float *v_ptr[],
                            int n_vectors);

int f32vector_xor_vectors_c (float *to,
                             int n_cell,
                             float *v_ptr[],
                             int n_vectors);

int f32vector_equal_vectors_c (int n_cell,
                               float *v_ptr[],
                               int n_vectors,
                               float prec);

int f32vector_binary_vectors_c (int n_cell,
                                float *v_ptr[],
                                int n_vectors);

int f32vector_is_a_seed_c (float *i_chan,
                           int n_cell,
                           float *s_chan);

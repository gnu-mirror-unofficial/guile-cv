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

int im_fast_channel_offset (int i,
                            int j,
                            int width);


/*
 * bounding box
 *
*/

int point_inside_c (int left,
                    int top,
                    int right,
                    int bottom,
                    int pt_x,
                    int pt_y);

int bb_intersect_c (int l_one,
                    int t_one,
                    int r_one,
                    int b_one,
                    int l_two,
                    int t_two,
                    int r_two,
                    int b_two);


/*
 * floats
 *
*/

int float_to_int_c (float f);
int float_equal_c (float f1, float f2, float prec);


/*
 * s32vectors
 *
*/

int s32_ref (int *chan,
             int i,
             int j,
             int width);

int s32_set (int *chan,
             int i,
             int j,
             int width,
             int val);

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

int f32vector_times_value_c (float *v,
                             int n_cell,
                             float val,
                             float *to);

int f32vector_times_vectors_c (float *to,
                               int n_cell,
                               float *v_ptr[],
                               int n_vectors);

int f32vector_mtimes_c (float *v1,
                        int w1,
                        int h1,
                        float *v2,
                        int w2,
                        float *to);

int f32vector_divide_value_c (float *v,
                              int n_cell,
                              float val,
                              float *to);

int f32vector_divide_vectors_c (float *to,
                                int n_cell,
                                float *v_ptr[],
                                int n_vectors);

int f32vector_invert_c (float *v,
                        int n_cell,
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

int f32vector_scale_c (float *v,
                       int n_cell,
                       float p_max,
                       float n_max,
                       float *to);

int f32vector_to_s32vector_c (float *v,
                              int n_cell,
                              int *to);

int f32vector_delineate_c (float *v,
                           float *v_min,
                           float *v_max,
                           int n_cell,
                           int threshold,
                           float *to);


/*
 * glcm
 *
*/

int glcm_c (int *chan,
            int width,
            int height,
            int *g0,
            int *g45,
            int *g90,
            int *g135,
            int n_gl,
            int dist);

;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2018
;;;; Free Software Foundation, Inc.

;;;; This file is part of GNU Guile-CV.

;;;; GNU Guile-CV is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation; either version 3 of the
;;;; License, or (at your option) any later version.

;;;; GNU Guile-CV is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU Guile-CV.  If not, see
;;;; <https://www.gnu.org/licenses/gpl.html>.
;;;;

;;; Commentary:

;;; Code:


(define-module (cv support libguile-cv)
  #:use-module (system foreign)
  #:use-module (cv init)

  #:export (;; misc.
            pointer-address-size-c

            ;; floats
            float-to-int-c
            float-equal-c

            ;; bounding box
            bb-intersect-c

            ;; f32vectors
            f32vector-min-c
            f32vector-max-c
            f32vector-range-c
            f32vector-scrap-c
            f32vector-scrap-in-place-c
            f32vector-threshold-c
            f32vector-fill-holes-c
            f32vector-rgb-to-gray-c
            f32vector-add-value-c
            f32vector-add-vectors-c
            f32vector-subtract-value-c
            f32vector-subtract-vectors-c
            f32vector-times-value-c
            f32vector-times-vectors-c
            f32vector-divide-value-c
            f32vector-divide-vectors-c
            f32vector-invert-c
            f32vector-and-vectors-c
            f32vector-or-vectors-c
            f32vector-xor-vectors-c
            f32vector-equal-vectors-c
            f32vector-binary-vectors-c
            f32vector-is-a-seed-c
            f32vector-scale-c
            f32vector-to-s32vector-c
            ;; glcm
            glcm-c))


;;;
;;; misc.
;;;

(define pointer-address-size-c
  (pointer->procedure size_t
                      (dynamic-func "pointer_address_size_c"
                                    %libguile-cv)
                      (list)))


;;;
;;; floats
;;;

(define float-to-int-c
  (pointer->procedure int
                      (dynamic-func "float_to_int_c"
                                    %libguile-cv)
                      (list float)))

(define float-equal-c
  (pointer->procedure int
                      (dynamic-func "float_equal_c"
                                    %libguile-cv)
                      (list float	;; f1
                            float	;; f2
                            float)))	;; precision


;;;
;;; bouding box
;;;

(define bb-intersect-c
  (pointer->procedure int
                      (dynamic-func "bb_intersect_c"
                                    %libguile-cv)
                      (list int int int int int int int int)))


;;;
;;; f32vectors
;;;

(define f32vector-min-c
  (pointer->procedure int
                      (dynamic-func "f32vector_min_c"
                                    %libguile-cv)
                      (list '*		;; channel
                            int		;; n-cell
                            '*)))	;; result vector

(define f32vector-max-c
  (pointer->procedure int
                      (dynamic-func "f32vector_max_c"
                                    %libguile-cv)
                      (list '*		;; channel
                            int		;; n-cell
                            '*)))	;; result vector

(define f32vector-range-c
  (pointer->procedure int
                      (dynamic-func "f32vector_range_c"
                                    %libguile-cv)
                      (list '*		;; channel
                            int		;; n-cell
                            '*)))	;; result vector

(define f32vector-scrap-c
  (pointer->procedure int
                      (dynamic-func "f32vector_scrap_c"
                                    %libguile-cv)
                      (list '*		;; chan
                            '*		;; l-chan
                            int		;; n-cell
                            '*		;; scrap-cache
                            '*)))	;; to

(define f32vector-scrap-in-place-c
  (pointer->procedure int
                      (dynamic-func "f32vector_scrap_in_place_c"
                                    %libguile-cv)
                      (list '*		;; chan
                            '*		;; l-chan
                            int		;; n-cell
                            '*)))	;; scrap-cache

(define f32vector-threshold-c
  (pointer->procedure int
                      (dynamic-func "f32vector_threshold_c"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n-cell
                            '*		;; v-ptr[]
                            int		;; n-vectors
                            float	;; threshold
                            int)))	;; bg [0 or 255]

(define f32vector-fill-holes-c
  (pointer->procedure int
                      (dynamic-func "f32vector_fill_holes_c"
                                    %libguile-cv)
                      (list '*       ;; labeled chan
                            int      ;; n-cell
                            float))) ;; bg label value

(define f32vector-rgb-to-gray-c
  (pointer->procedure int
                      (dynamic-func "f32vector_rgb_to_gray_c"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n-cell
                            '*		;; r
                            '*		;; g
                            '*)))	;; b

(define f32vector-add-value-c
  (pointer->procedure int
                      (dynamic-func "f32vector_add_value_c"
                                    %libguile-cv)
                      (list '*		;; chan
                            int		;; n-cell
                            float	;; value
                            '*)))	;; to

(define f32vector-add-vectors-c
  (pointer->procedure int
                      (dynamic-func "f32vector_add_vectors_c"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n-cell
                            '*		;; v-ptr[]
                            int)))	;; n-vectors

(define f32vector-subtract-value-c
  (pointer->procedure int
                      (dynamic-func "f32vector_subtract_value_c"
                                    %libguile-cv)
                      (list '*		;; chan
                            int		;; n-cell
                            float	;; value
                            '*)))	;; to

(define f32vector-subtract-vectors-c
  (pointer->procedure int
                      (dynamic-func "f32vector_subtract_vectors_c"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n-cell
                            '*		;; v-ptr[]
                            int)))	;; n-vectors

(define f32vector-times-value-c
  (pointer->procedure int
                      (dynamic-func "f32vector_times_value_c"
                                    %libguile-cv)
                      (list '*		;; chan
                            int		;; n-cell
                            float	;; value
                            '*)))	;; to

(define f32vector-times-vectors-c
  (pointer->procedure int
                      (dynamic-func "f32vector_times_vectors_c"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n-cell
                            '*		;; v-ptr[]
                            int)))	;; n-vectors

(define f32vector-divide-value-c
  (pointer->procedure int
                      (dynamic-func "f32vector_divide_value_c"
                                    %libguile-cv)
                      (list '*		;; chan
                            int		;; n-cell
                            float	;; value
                            '*)))	;; to

(define f32vector-divide-vectors-c
  (pointer->procedure int
                      (dynamic-func "f32vector_divide_vectors_c"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n-cell
                            '*		;; v-ptr[]
                            int)))	;; n-vectors

(define f32vector-invert-c
  (pointer->procedure int
                      (dynamic-func "f32vector_invert_c"
                                    %libguile-cv)
                      (list '*		;; from
                            int		;; n-cell
                            '*)))	;; to

(define f32vector-and-vectors-c
  (pointer->procedure int
                      (dynamic-func "f32vector_and_vectors_c"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n-cell
                            '*		;; v-ptr[]
                            int)))	;; n-vectors

(define f32vector-or-vectors-c
  (pointer->procedure int
                      (dynamic-func "f32vector_or_vectors_c"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n-cell
                            '*		;; v-ptr[]
                            int)))	;; n-vectors

(define f32vector-xor-vectors-c
  (pointer->procedure int
                      (dynamic-func "f32vector_xor_vectors_c"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n-cell
                            '*		;; v-ptr[]
                            int)))	;; n-vectors

(define f32vector-equal-vectors-c
  (pointer->procedure int
                      (dynamic-func "f32vector_equal_vectors_c"
                                    %libguile-cv)
                      (list int		;; n-cell
                            '*		;; v-ptr[]
                            int		;; n-vectors
                            float)))	;; precision

(define f32vector-binary-vectors-c
  (pointer->procedure int
                      (dynamic-func "f32vector_binary_vectors_c"
                                    %libguile-cv)
                      (list int		;; n-cell
                            '*		;; v-ptr[]
                            int)))	;; n-vectors

(define f32vector-is-a-seed-c
  (pointer->procedure int
                      (dynamic-func "f32vector_is_a_seed_c"
                                    %libguile-cv)
                      (list '*		;; i-chan
                            int		;; n-cell
                            '*)))	;; s-chan

(define f32vector-scale-c
  (pointer->procedure int
                      (dynamic-func "f32vector_scale_c"
                                    %libguile-cv)
                      (list '*		;; chan
                            int		;; n-cell
                            float	;; o-gl
                            float	;; n-gl
                            '*)))	;; to

(define f32vector-to-s32vector-c
  (pointer->procedure int
                      (dynamic-func "f32vector_to_s32vector_c"
                                    %libguile-cv)
                      (list '*    ;; chan
                            int   ;; n-cell
                            '*))) ;; to


;;;
;;; glcm
;;;

(define glcm-c
  (pointer->procedure int
                      (dynamic-func "glcm_c"
                                    %libguile-cv)
                      (list '*		;; chan
                            int		;; width
                            int		;; height
                            '*		;; g0-chan
                            '*		;; g45-chan
                            '*		;; g90-chan
                            '*		;; g135-chan
                            int		;; n-gl
                            int)))	;; dist

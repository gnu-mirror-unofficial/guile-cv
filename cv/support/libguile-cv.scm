;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2017
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

  #:export (float-to-int-c
            pointer-address-size-c
            f32vector-min-c
            f32vector-max-c
            f32vector-range-c
            f32vector-scrap-c
            f32vector-add-value-c
            f32vector-add-vectors-c
            f32vector-subtract-value-c
            f32vector-subtract-vectors-c
            f32vector-multiply-vectors-c))


(define float-to-int-c
  (pointer->procedure int
                      (dynamic-func "float_to_int_c"
                                    %libguile-cv)
                      (list float)))

(define pointer-address-size-c
  (pointer->procedure size_t
                      (dynamic-func "pointer_address_size_c"
                                    %libguile-cv)
                      (list)))

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

(define f32vector-add-value-c
  (pointer->procedure int
                      (dynamic-func "f32vector_add_value_c"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n-cell
                            float)))	;; value

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
                      (list '*		;; to
                            int		;; n-cell
                            float)))	;; value

(define f32vector-subtract-vectors-c
  (pointer->procedure int
                      (dynamic-func "f32vector_subtract_vectors_c"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n-cell
                            '*		;; v-ptr[]
                            int)))	;; n-vectors

(define f32vector-multiply-value-c
  (pointer->procedure int
                      (dynamic-func "f32vector_multiply_value_c"
                                    %libguile-cv)
                      (list '*		;; to
                            int		;; n-cell
                            float)))	;; value

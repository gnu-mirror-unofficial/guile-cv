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
            f32vector-min-c
            f32vector-max-c
            f32vector-range-c
            f32vector-scrap-c))


(define float-to-int-c
  (pointer->procedure int
                      (dynamic-func "float_to_int_c"
                                    %libguile-cv)
                      (list float)))

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

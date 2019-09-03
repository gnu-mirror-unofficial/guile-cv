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


(define-module (cv support s32vector)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:use-module (system foreign)
  #:use-module (cv init)
  #:use-module (cv support utils)
  #:use-module (cv support float)
  #:use-module (cv support libguile-cv)

  #:export (s32vector-range
            s32vector-min
	    s32vector-max
            s32vector-reduce
            s32vector->f32vector
            glcm))


(define (s32vector-range v)
  (let ((n-cell (s32vector-length v)))
    (case n-cell
      ((0)
       (error "Empty vector: " v))
      (else
       (do ((mini (s32vector-ref v 0))
            (maxi (s32vector-ref v 0))
            (p-mini 0)
            (p-maxi 0)
            (i 1
               (+ i 1)))
           ((= i n-cell)
            (list mini p-mini maxi p-maxi))
         (let ((val (s32vector-ref v i)))
           (when (< val mini)
             (set! mini val)
             (set! p-mini i))
           (when (> val maxi)
             (set! maxi val)
             (set! p-maxi i))))))))

(define (s32vector-min v)
  (match (s32vector-range v)
    ((mini p-mini maxi p-maxi)
     (values mini p-mini))))

(define (s32vector-max v)
  (match (s32vector-range v)
    ((mini p-mini maxi p-maxi)
     (values maxi p-maxi))))

(define* (s32vector-reduce v proc default #:key (n-cell #f))
  (let ((n-cell (or n-cell
                    (s32vector-length v))))
    (if (= n-cell 0)
        default
        (do ((i 1
                (+ i 1))
             (prev (s32vector-ref v 0)
                   (proc (s32vector-ref v i) prev)))
            ((= i n-cell) prev)))))

(define (s32vector->f32vector v)
  (let* ((n-cell (s32vector-length v))
	 (to (make-f32vector n-cell)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell) to)
      (f32vector-set! to i
                      (s32vector-ref v i)))))

(define (glcm chan width height g0 g45 g90 g135 n-gl dist)
  (glcm_c (bytevector->pointer chan)
          width
          height
          (bytevector->pointer g0)
          (bytevector->pointer g45)
          (bytevector->pointer g90)
          (bytevector->pointer g135)
          n-gl
          dist))

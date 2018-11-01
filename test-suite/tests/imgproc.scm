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


(define-module (tests imgproc)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (unit-test)
  #:use-module (cv))


(define %i-dir
  (getenv "TEST_IMAGES_PATH"))

(define %i-pp-17-bf
  (im-load (string-append %i-dir "/pp-17-bf.png")))

(define %i-minima
  (list 9 5 1
        (list #f32(0 0 0 0 0 0 0 0 0
                   0 4 4 4 0 0 2 0 0
                   0 4 3 4 0 2 1 2 0
                   0 4 4 4 0 0 2 0 0
                   0 0 0 0 0 0 0 0 0))))

(define %i-minima-plateaus
    (list 10 6 1
          (list #f32(0 0 0 0 0 0 0 0 0 0
                     0 4 4 4 0 0 2 2 0 0
                     0 4 3 4 0 2 1 1 2 0
                     0 4 3 4 0 0 2 2 0 0
                     0 4 4 4 0 0 0 0 0 0
                     0 0 0 0 0 0 0 0 0 0))))

(define %i-result-default
  (list 9 5 1
        (list #f32(0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0
                   0 0 1 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0))))

(define %i-result-4con
  (list 9 5 1
        (list #f32(0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0
                   0 0 1 0 0 0 1 0 0
                   0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0))))

(define %i-result-plateaus
  (list 10 6 1
        (list #f32(0 0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0
                   0 0 1 0 0 0 0 0 0 0
                   0 0 1 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0))))

(define %i-result-plateaus-4con
  (list 10 6 1
        (list #f32(0 0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0
                   0 0 1 0 0 0 1 1 0 0
                   0 0 1 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0))))

(define %i-maxima
    (list 9 5 1
          (list #f32(0 0 0 0 0 0 0 0 0
                     0 4 4 4 0 0 2 0 0
                     0 4 5 4 0 2 3 2 0
                     0 4 4 4 0 4 2 0 0
                     0 0 0 0 0 0 0 0 0))))

(define %i-maxima-plateaus
    (list 10 6 1
          (list #f32(0 0 0 0 0 0 0 0 0 0
                     0 4 4 4 0 0 2 2 0 0
                     0 4 5 4 0 2 3 3 2 0
                     0 4 5 4 0 4 2 2 0 0
                     0 4 4 4 0 0 0 0 0 0
                     0 0 0 0 0 0 0 0 0 0))))

(define %i-result-maxima-default
  (list 9 5 1
        (list #f32(0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0
                   0 0 1 0 0 0 0 0 0
                   0 0 0 0 0 1 0 0 0
                   0 0 0 0 0 0 0 0 0))))

(define %i-result-maxima-4con
  (list 9 5 1
        (list #f32(0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0
                   0 0 1 0 0 0 1 0 0
                   0 0 0 0 0 1 0 0 0
                   0 0 0 0 0 0 0 0 0))))

(define %i-result-maxima-plateaus
  (list 10 6 1
        (list #f32(0 0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0
                   0 0 1 0 0 0 0 0 0 0
                   0 0 1 0 0 1 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0))))

(define %i-result-maxima-plateaus-4con
  (list 10 6 1
        (list #f32(0 0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0
                   0 0 1 0 0 0 1 1 0 0
                   0 0 1 0 0 1 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0))))


(define-class <guile-cv-tests-imgproc> (<test-case>))

(define-method (test-im-local-minima (self <guile-cv-tests-imgproc>))
  (assert (im-local-minima %i-pp-17-bf))
  (assert (im-local-minima %i-pp-17-bf
                           #:con 4 #:marker 60 #:threshold 150 #:borders? #t
                           #:plateaus? #t #:epsilon 1.0e-3))
  (assert-true (im-=? (im-local-minima %i-minima)
                      %i-result-default))
  (assert-true (im-=? (im-local-minima %i-minima #:con 4)
                      %i-result-4con))
  (assert-true (im-=? (im-local-minima %i-minima-plateaus #:plateaus? #t)
                      %i-result-plateaus))
  (assert-true (im-=? (im-local-minima %i-minima-plateaus #:plateaus? #t #:con 4)
                      %i-result-plateaus-4con)))

(define-method (test-im-local-maxima (self <guile-cv-tests-imgproc>))
  (assert (im-local-maxima %i-pp-17-bf))
  (assert (im-local-maxima %i-pp-17-bf
                           #:con 4 #:marker 150 #:threshold 60 #:borders? #t
                           #:plateaus? #t #:epsilon 1.0e-3))
  (assert-true (im-=? (im-local-maxima %i-maxima)
                      %i-result-maxima-default))
  (assert-true (im-=? (im-local-maxima %i-maxima #:con 4)
                      %i-result-maxima-4con))
  (assert-true (im-=? (im-local-maxima %i-maxima-plateaus #:plateaus? #t)
                      %i-result-maxima-plateaus))
  (assert-true (im-=? (im-local-maxima %i-maxima-plateaus #:plateaus? #t #:con 4)
                      %i-result-maxima-plateaus-4con)))


(exit-with-summary (run-all-defined-test-cases))

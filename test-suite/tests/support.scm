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


(define-module (tests support)
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (cv))


(define-class <guile-cv-tests-support> (<test-case>))


(define-method (test-float (self <guile-cv-tests-support>))
  (assert-false (float-zero? 0.001))
  (assert-true (float-zero? 0.0001))
  (assert-true (float=? 0.0001 0.0001))
  (assert-false (float=? 0.0001 -0.0001))
  (assert-false (float<? 0.0001 -0.0001))
  (assert-false (float<? -0.0001 -0.0001))
  (assert-false (float<? -0.0002 -0.0001))
  (assert-true (float<? -0.002 -0.001))
  (assert-false (float>? -0.0001 -0.0001))
  (assert-true (float>? 0.0001 -0.0001))
  (assert-true (float>? 0.001 0.0001))
  (assert-true (float<=? 0.0001 0.0001))
  (assert-true (float>=? 0.0001 0.0001))
  (assert-numeric-= 13.346 (float-round 13.3456723 3) 1.0e-3)
  (assert-numeric-= 13.35 (float-round 13.3456723) 1.0e-2))

(define-method (test-f32vector (self <guile-cv-tests-support>))
  (let ((v1 #f32(-2.0 -10.0 0.0 255.0 20.0))
	(v2 #f32(0.0 128.0 196.0 255.0))
	(v3 #f32(255.0 127.0 59.0 0.0))
	(v4 #f32(255.0 127.0 59.0 0.0))
	(pred1 (lambda (val) (float=? val 0.0)))
	(pred2 (lambda (val) (float=? val 255.0)))
	(pred3 (lambda (val) (float=? val 256.0))))
    (receive (val pos)
        (f32vector-min v1)
      (assert-numeric-= val -10.0 1.0e-4)
      (assert-numeric-= pos 1 1.0e-4))
    (receive (val pos)
        (f32vector-max v1)
      (assert-numeric-= val 255.0 1.0e-4)
      (assert-numeric-= pos 3 1.0e-4))
    (assert-true (f32vector=? v1 (f32vector-copy v1)))
    (assert-true (f32vector=? v3 (f32vector-complement v2)))
    (assert-false (f32vector-and-at-offset (list v2 v3) 0))
    (assert-true (f32vector-and-at-offset (list v2 v3) 1))
    (assert-numeric-= 255.0 (f32vector-sum-at-offset (list v2 v3) 0) 1.0e-1)
    (assert-numeric-= 255.0 (f32vector-sum-at-offset (list v2 v3) 1) 1.0e-1)
    (assert-numeric-= 255.0 (f32vector-sum-at-offset (list v2 v3) 2) 1.0e-1)
    (assert-numeric-= 255.0 (f32vector-sum-at-offset (list v2 v3) 3) 1.0e-1)
    (assert-numeric-= 127.5 (f32vector-mean-at-offset (list v2 v3) 0) 1.0e-1)
    (assert-numeric-= 127.5 (f32vector-mean-at-offset (list v2 v3) 1) 1.0e-1)
    (assert-numeric-= 127.5 (f32vector-mean-at-offset (list v2 v3) 2) 1.0e-1)
    (assert-numeric-= 127.5 (f32vector-mean-at-offset (list v2 v3) 3) 1.0e-1)
    (assert-false (f32vector=-at-offset? (list v1 v2) 0))
    (assert-false (f32vector-pred-at-offset? pred1 (list v2 v3) 0))
    (assert-true (f32vector-pred-at-offset? pred2 (list v3 v4) 0))
    (assert-numeric-= 0 (f32vector-index pred2 v3 v4) 1.0e-1)
    (assert-false (f32vector-index pred3 v3 v4))))

(define %v1 #f32(2.0 4.0 4.0 4.0 5.0 5.0 7.0 9.0))

(define-method (test-f32vector-reduce (self <guile-cv-tests-support>))
  (let ((v1 %v1))
    (assert-numeric-= 40.0 (f32vector-reduce v1 + 0) 0)))

(define-method (test-f32vector-mean (self <guile-cv-tests-support>))
  (let* ((v1 %v1)
         (mean (f32vector-mean v1)))
    (assert-numeric-= 5.0 mean 0)))

(define-method (test-f32vector-std-dev (self <guile-cv-tests-support>))
  (let* ((v1 %v1)
         (std-dev (f32vector-std-dev v1)))
    (assert-numeric-= 2.0 std-dev 0)))


;;;
;;; s32vector
;;;

(define-method (test-s32vector-min-max (self <guile-cv-tests-support>))
  (let ((v1 #s32(-2 -10 0 255 20))
	(v2 #s32(0 128 196 255)))
    (receive (val pos)
        (s32vector-min v1)
      (assert-true (= val -10))
      (assert-true (= pos 1)))
    (receive (val pos)
        (s32vector-max v1)
      (assert-true (= val 255))
      (assert-true (= pos 3)))))


(define %s32-v1 #s32(2 4 4 4 5 5 7 9))

(define-method (test-s32vector-reduce (self <guile-cv-tests-support>))
  (assert-true (= 40 (s32vector-reduce %s32-v1 + 0))))


;;;
;;; libguile-cv
;;;

(define-method (test-libguile-cv (self <guile-cv-tests-support>))
  (assert-true (= 1 (float->int 1.0)))
  (assert-true (= 1 (float->int 1.52)))
  (assert-true (= 1 (float->int 1.99))))

(exit-with-summary (run-all-defined-test-cases))

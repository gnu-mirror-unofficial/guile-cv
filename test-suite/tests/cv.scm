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


(define-module (tests cv)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (unit-test)
  #:use-module (cv))


(define-class <guile-cv-tests-cv> (<test-case>))


(define (make-test-image-rgb w h)
  (let ((image (im-make w h 3))
	(n-cell (* w h)))
    (match image
      ((width height n-chan idata)
       (match idata
	 ((r g b)
	  (for-each (lambda (i)
		      (f32vector-set! r i (* i 1.0)))
	      (iota n-cell))
	  (for-each (lambda (i)
		      (f32vector-set! g i (* (+ i n-cell) 1.0)))
	      (iota n-cell))
	  (for-each (lambda (i)
		      (f32vector-set! b i (* (+ i (* 2 n-cell)) 1.0)))
	      (iota n-cell))))))
    image))

(define (make-test-image-gray w h)
  (let ((image (im-make w h 1))
	(n-cell (* w h)))
    (match image
      ((width height n-chan idata)
       (match idata
	 ((c)
	  (for-each (lambda (i)
		      (f32vector-set! c i (* i 1.0)))
	      (iota n-cell))))))
    image))

(define (make-test-image-gray-type type)
  (let* ((img (im-make 3 3 1))
	 (chan (im-channel img 0)))
    (case type
      ((empty)
       img)
      ((center)
       (f32vector-set! chan 4 255.0))
      ((corner)
       (for-each (lambda (i)
		   (f32vector-set! chan i 255.0))
	   '(0 2 6 8)))
      ((diamond)
       (for-each (lambda (i)
		   (f32vector-set! chan i 255.0))
	   '(1 3 5 7)))
      ((square)
       (for-each (lambda (i)
		   (f32vector-set! chan i 255.0))
	   '(0 1 2 3 5 6 7 8)))
      ((full)
       (for-each (lambda (i)
		   (f32vector-set! chan i 255.0))
	   '(0 1 2 3 4 5 6 7 8)))
      (else
       (error "No such image gray type:" type)))
    img))

(define (make-test-image-logical type)
  (let* ((img (im-make 3 3 1))
	 (chan (im-channel img 0)))
    (case type
      ((center)
       (f32vector-set! chan 4 255.0))
      ((corner)
       (for-each (lambda (i)
		   (f32vector-set! chan i (exact->inexact i)))
	   '(0 2 6 8)))
      ((diamond)
       (for-each (lambda (i)
		   (f32vector-set! chan i (exact->inexact i)))
	   '(1 3 5 7)))
      ((square)
       (for-each (lambda (i)
		   (f32vector-set! chan i (exact->inexact i)))
	   '(0 1 2 3 5 6 7 8))))
    img))

(define (make-disc-erode-image)
  (let ((img (im-make 11 11 1 1.0)))
    (im-set! img 1 5 0.0)
    img))

(define (make-disc-dilate-image)
  (let ((img (im-make 11 11 1 1.0)))
    (for-each (lambda (pos)
		(match pos
		  ((i j) (im-set! img i j 0.0))))
	'((0 0)
	  (0 1) (0 2) (0 3) (0 4) (0 5) (0 6) (0 7) (0 8) (0 9) (0 10)
	  (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0) (9 0) (10 0)
	  (10 1) (10 2) (10 3) (10 4) (10 5) (10 6) (10 7) (10 8) (10 9)
	  (1 10) (2 10) (3 10) (4 10) (5 10) (6 10) (7 10) (8 10) (9 10)
	  (1 5) (2 5) (1 6) (2 6)
	  (8 8) (8 9) (8 10) (9 8) (9 9) (9 10) (10 8) (10 9) (10 10)
	  (5 3) (5 4) (5 5) (6 3) (6 4) (6 5) (7 3) (7 4) (7 5)
	  ))
    img))


(define-method (test-idata-1 (self <guile-cv-tests-cv>))
  (let ((img-1 (make-test-image-rgb 4 3)))
    (assert-exception (im-ref img-1 -1 0))
    (assert-exception (im-ref img-1 -1 0 1))
    (assert-exception (im-ref img-1 3 0))
    (assert-exception (im-ref img-1 3 0 1))
    (assert-exception (im-ref img-1 0 4))
    (assert-exception (im-ref img-1 0 4 1))
    (assert-exception (im-ref img-1 0 0 -1))
    (assert-exception (im-ref img-1 0 0 3))))


(define-method (test-idata-2 (self <guile-cv-tests-cv>))
  (let* ((img-1 (make-test-image-rgb 3 2))
	 (img-2 (make-test-image-rgb 3 2))
	 (img-3 (im-make 3 2 3))
	 (img-4 (im-make 3 2 1))
	 (img-5 (im-make 4 3 1))
	 (img-6 (im-make 3 2 4))
	 (list-1 (list img-1 img-2))
	 (list-2 (list img-1 img-5)))
    (assert-true (im-collect list-1 'size))
    (assert-true (im-collect list-1 'width))
    (assert-true (im-collect list-1 'height))
    (assert-true (im-collect list-1 'n-channel))
    (assert-true (im-collect list-1 'channels))
    (assert-true (im-collect list-1 'chan-0))
    (assert-true (im-collect list-1 'gray))
    (assert-true (im-collect list-1 'red))
    (assert-true (im-collect list-1 'chan-1))
    (assert-true (im-collect list-1 'green))
    (assert-true (im-collect list-1 'chan-2))
    (assert-true (im-collect list-1 'blue))
    (assert-true (im-collect list-1 'blue))
    (assert-true (im-collect (list img-6) 'chan-3))))


(define-method (test-idata-3 (self <guile-cv-tests-cv>))
  (let* ((empty (make-test-image-gray-type 'empty))
	 (center (make-test-image-gray-type 'center))
	 (square (make-test-image-gray-type 'square))
	 (diamond (make-test-image-gray-type 'diamond))
	 (img-1 (make-test-image-rgb 3 2)))
    (assert-true (im-image? empty))
    (assert-true (im-image? center))
    (assert-true (im-image? square))
    (assert-true (im-image? diamond))
    (assert-true (im-gray? empty))
    (assert-false (im-rgb? empty))
    (assert-true (im-rgb? img-1))
    (assert-false (im-gray? img-1))
    (im-fast-set! empty 0 0 -1.0)
    (assert-false (im-image? empty))
    (im-fast-set! empty 0 0 256.0)
    (assert-false (im-image? empty))))


(define-method (test-impex (self <guile-cv-tests-cv>))
  (let* ((i-dir (getenv "TEST_IMAGES_PATH"))
	 (t-file "/tmp/test.png")
	 (b-file (string-append i-dir "/pp-17-label.png"))
	 (l-file (string-append i-dir "/pp-17-bf.png")))
    (assert-equal '(85 95 1) (im-size b-file))
    (assert ((@@ (cv impex) vigra-load-gray-image) b-file 85 95))
    (assert-equal '(85 95 3) (im-size l-file))
    (assert ((@@ (cv impex) vigra-load-rgb-image) l-file 85 95))
    (let ((bw (im-load b-file))
	  (bf (im-load l-file)))
      (assert-equal '(85 95 1) (im-size bw))
      (assert ((@@ (cv impex) vigra-save-gray-image) bw t-file 85 95))
      (assert-equal '(85 95 3) (im-size bf))
      (assert ((@@ (cv impex) vigra-save-rgb-image) bf t-file 85 95)))))


(define-method (test-im-min/max (self <guile-cv-tests-cv>))
  (let* ((gray (make-test-image-gray 3 2))
         (rgb (make-test-image-rgb 3 2)))
    (receive (val row col)
        (im-min gray)
      (assert-numeric-= val 0.0 1.0e-4)
      (assert-numeric-= row 0 1.0e-4)
      (assert-numeric-= col 0 1.0e-4))
    (receive (val row col)
        (im-max gray)
      (assert-numeric-= val 5.0 1.0e-4)
      (assert-numeric-= row 1 1.0e-4)
      (assert-numeric-= col 2 1.0e-4))
    (let ((rgb-mins (im-min rgb))
          (expected-mins '((0.0 0 0) (6.0 0 0) (12.0 0 0)))
          (rgb-maxs (im-max rgb))
          (expected-maxs '((5.0 1 2) (11.0 1 2) (17.0 1 2))))
      (for-each (lambda (item)
                  (match item
                    ((a b)
                     (for-each (lambda (vals)
                                 (match vals
                                   ((need got)
                                    (assert-numeric-= need got 1.0e-4))))
                       (zip a b)))))
          (zip rgb-mins expected-mins))
      (for-each (lambda (item)
                  (match item
                    ((a b)
                     (for-each (lambda (vals)
                                 (match vals
                                   ((need got)
                                    (assert-numeric-= need got 1.0e-4))))
                       (zip a b)))))
          (zip rgb-maxs expected-maxs)))))


(define-method (test-im-and (self <guile-cv-tests-cv>))
  (let* ((empty (make-test-image-gray-type 'empty))
	 (center (make-test-image-gray-type 'center))
	 (square (make-test-image-gray-type 'square))
	 (diamond (make-test-image-gray-type 'diamond)))
    (assert-true (im-=? diamond (im-and square diamond)))
    (assert-true (im-=? empty (im-and diamond center)))))

(define-method (test-im-or (self <guile-cv-tests-cv>))
  (let* ((empty (make-test-image-gray-type 'empty))
	 (center (make-test-image-gray-type 'center))
         (corner (make-test-image-gray-type 'corner))
	 (square (make-test-image-gray-type 'square))
	 (diamond (make-test-image-gray-type 'diamond))
         (full  (make-test-image-gray-type 'full)))
    (assert-true (im-=? square (im-or corner square)))
    (assert-true (im-=? full (im-or center square)))))

(define-method (test-im-xor (self <guile-cv-tests-cv>))
  (let* ((empty (make-test-image-gray-type 'empty))
	 (center (make-test-image-gray-type 'center))
         (corner (make-test-image-gray-type 'corner))
	 (square (make-test-image-gray-type 'square))
	 (diamond (make-test-image-gray-type 'diamond)))
    (assert-true (im-=? diamond (im-xor corner square)))
    (assert-true (im-=? corner (im-xor corner square square)))))


(define (make-add-1-a)
  `(2 3 1 (,#f32(1.0 2.0 3.0 4.0 5.0 6.0))))

(define (make-add-1-a')
  `(3 2 1 (,#f32(1.0 3.0 5.0 2.0 4.0 6.0))))

(define-method (test-im-transpose (self <guile-cv-tests-cv>))
  (let ((a (make-add-1-a))
        (a' (make-add-1-a')))
    (assert-true (im-=? (im-transpose a) a'))
    (assert-true (im-=? (im-transpose a') a))))

(define-method (test-im-inverse (self <guile-cv-tests-cv>))
  (let* ((d `(2 3 1 (,#f32(2.0 4.0 8.0 16.0 32.0 64.0))))
         (di (im-inverse d))
         (di-chan (im-channel di 0))
         (di-check '(1/2 1/4 1/8 1/16 1/32 1/64)))
    (do ((i 0
            (+ i 1)))
        ((= i 6))
      (assert-true (= (inexact->exact (f32vector-ref di-chan i))
                      (list-ref di-check i))))))

(define (make-add-2-a)
  `(2 3 1 (,#f32(1.0 2.0 3.0 4.0 5.0 6.0))))

(define (make-add-2-b)
  `(2 3 1 (,#f32(2.0 3.0 4.0 5.0 6.0 7.0))))

(define (make-add-2-c)
  `(2 3 1 (,#f32(2.0 4.0 6.0 8.0 10.0 12.0))))

(define (make-add-2-a+b)
  `(2 3 1 (,#f32(3.0 5.0 7.0 9.0 11.0 13.0))))

(define (make-add-2-a-b)
  `(2 3 1 (,#f32(-1.0 -1.0 -1.0 -1.0 -1.0 -1.0))))

(define (make-add-2-a')
  `(3 2 1 (,#f32(1.0 3.0 5.0 2.0 4.0 6.0))))

(define (make-add-2-a*a')
  `(3 3 1 (,#f32(5.0 11.0 17.0 11.0 25.0 39.0 17.0 39.0 61.0))))

(define-method (test-im-add (self <guile-cv-tests-cv>))
  (let ((a (make-add-2-a))
        (b (make-add-2-b))
        (a+b (make-add-2-a+b)))
    (assert-true (im-=? (im-add a 1.0) b))
    (assert-true (im-=? (im-add a b) a+b))))

(define-method (test-im-subtract (self <guile-cv-tests-cv>))
  (let ((a (make-add-2-a))
        (b (make-add-2-b))
        (a-b (make-add-2-a-b)))
    (assert-true (im-=? (im-subtract b 1.0) a))
    (assert-true (im-=? (im-subtract a b) a-b))))

(define-method (test-im-multiply (self <guile-cv-tests-cv>))
  (let ((a (make-add-2-a))
        (c (make-add-2-c))
        (a' (make-add-2-a'))
        (a*a' (make-add-2-a*a')))
    (assert-true (im-=? (im-multiply a 2.0) c))
    (assert-true (im-=? (im-multiply a a') a*a'))))

(define-method (test-im-divide (self <guile-cv-tests-cv>))
  (let* ((a `(2 3 1 (,#f32(2.0 4.0 8.0 16.0 32.0 64.0))))
         (d (im-inverse
             (im-transpose
              `(2 3 1 (,#f32(2.0 4.0 8.0 16.0 32.0 64.0))))))
         (a/d (im-multiply a d))
         (a/d-chan (im-channel a/d 0))
         (a/d-check '(2 1/2 1/8 8 2 1/2 32 8 2)))
    (do ((i 0
            (+ i 1)))
        ((= i 9))
      (assert-true (= (inexact->exact (f32vector-ref a/d-chan i))
                      (list-ref a/d-check i))))))

(define (make-scrap-test-image)
  (let ((img (im-make 12 12 1)))
    (for-each (lambda (pos)
		(match pos
		  ((i j) (im-set! img i j 1.0))))
	'((1 1) (1 2)
          (2 1) (2 2)
          (4 4) (4 5) (4 6)
          (5 4) (5 5) (5 6)
          (6 4) (6 5) (6 6)
          (8 8) (8 9) (8 10) (8 11)
          (9 8) (9 9) (9 10) (9 11)
          (10 8) (10 9) (10 10) (10 11)
          (11 8) (11 9) (11 10) (11 11)
	  ))
    img))

(define (make-scrap-test-<)
  (let ((img (im-make 12 12 1)))
    (for-each (lambda (pos)
		(match pos
		  ((i j) (im-set! img i j 1.0))))
	'((4 4) (4 5) (4 6)
          (5 4) (5 5) (5 6)
          (6 4) (6 5) (6 6)
          (8 8) (8 9) (8 10) (8 11)
          (9 8) (9 9) (9 10) (9 11)
          (10 8) (10 9) (10 10) (10 11)
          (11 8) (11 9) (11 10) (11 11)
	  ))
    img))

(define (make-scrap-test-=)
  (let ((img (im-make 12 12 1)))
    (for-each (lambda (pos)
		(match pos
		  ((i j) (im-set! img i j 1.0))))
	'((1 1) (1 2)
          (2 1) (2 2)
          (8 8) (8 9) (8 10) (8 11)
          (9 8) (9 9) (9 10) (9 11)
          (10 8) (10 9) (10 10) (10 11)
          (11 8) (11 9) (11 10) (11 11)
	  ))
    img))

(define (make-scrap-test->)
  (let ((img (im-make 12 12 1)))
    (for-each (lambda (pos)
		(match pos
		  ((i j) (im-set! img i j 1.0))))
	'((1 1) (1 2)
          (2 1) (2 2)
          (4 4) (4 5) (4 6)
          (5 4) (5 5) (5 6)
          (6 4) (6 5) (6 6)))
    img))

(define-method (test-im-scrap (self <guile-cv-tests-cv>))
  (let ((a (make-scrap-test-image))
        (b (make-scrap-test-<))
        (c (make-scrap-test-=))
        (d (make-scrap-test->)))
    (assert-true (im-=? (im-scrap a 5) b))
    (assert-true (im-=? (im-scrap a 9 #:pred =) c))
    (assert-true (im-=? (im-scrap a 9 #:pred >) d))))

(define (make-features-images)
  (let ((idx '((0 0)
               (0 1) (0 2) (0 3)
               (1 0) (2 0) (3 0)
               (1 3) (2 3) (3 3)
               (3 0) (3 1) (3 2)
               (3 3)))
        (img-1 (im-make 4 4 1 1.0))
        (img-2 (im-make 4 4 3 1.0))
        (img-3 (im-make 4 4 4 1.0)))
    (for-each (lambda (pos)
		(match pos
		  ((i j)
                   (im-set! img-1 i j 0.0))))
	idx)
    (for-each (lambda (pos)
		(match pos
		  ((i j)
                   (im-set! img-2 i j 0 0.0)
                   (im-set! img-2 i j 1 0.0)
                   (im-set! img-2 i j 2 0.0))))
	idx)
    (values img-1 img-2 img-3)))

(define-method (test-features (self <guile-cv-tests-cv>))
  (receive (img-1 img-2 img-3)
      (make-features-images)
    (let ((l-img (im-label img-1)))
      (assert-exception (im-features img-3 l-img))
      (assert-exception (im-features img-1 img-2))
      (assert (im-features img-1 l-img)))))

(define %texture-test-image
  (list 4 4 1
        (list #f32(0 0 1 1 0 0 1 1 0 2 2 2 2 2 3 3))))

(define %glcm-test-result
  (list 4 4 4
        (list #f32(2.0  2.0  1.0  0.0
                   0.0  2.0  0.0  0.0
                   0.0  0.0  3.0  1.0
                   0.0  0.0  0.0  1.0)
              #f32(2.0  1.0  0.0  0.0
                   0.0  1.0  0.0  0.0
                   0.0  2.0  2.0  0.0
                   0.0  0.0  1.0  0.0)
              #f32(3.0  0.0  0.0  0.0
                   0.0  2.0  0.0  0.0
                   2.0  2.0  1.0  0.0
                   0.0  0.0  2.0  0.0)
              #f32(1.0  0.0  0.0  0.0
                   1.0  1.0  0.0  0.0
                   3.0  1.0  0.0  0.0
                   0.0  0.0  2.0  0.0))))

(define-method (test-im-reduce (self <guile-cv-tests-cv>))
  (assert-numeric-= (im-reduce %texture-test-image + 0) 20.0 1.0e-4)
  (for-each (lambda (vals)
              (match vals
                ((got need)
                 (assert-numeric-= got need 1.0e-4))))
      (zip (im-reduce %glcm-test-result + 0)
           '(12.0 9.0 12.0 9.0))))

(exit-with-summary (run-all-defined-test-cases))

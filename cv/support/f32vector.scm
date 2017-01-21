;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016
;;;; Free Software Foundation, Inc.

;;;; This file is part of GNU Guile-CV

;;;; GNU Guile-CV is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public License
;;;; as published by the Free Software Foundation; either version 3 of
;;;; the License, or (at your option) any later version.

;;;; GNU Guile-CV is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.

;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with GNU Guile-CV.  If not, see
;;;; <https://www.gnu.org/licenses/lgpl.html>.
;;;;

;;; Commentary:

;;; Code:


(define-module (cv support f32vector)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:use-module (cv support utils)
  #:use-module (cv support float)

  #:export (f32vector-min
	    f32vector-max
	    f32vector-copy
	    f32vector-complement
	    f32vector-and-at-offset
	    f32vector-or-at-offset
	    f32vector-sum-at-offset
	    f32vector-mean-at-offset
	    f32vector=-at-offset?
	    f32vector-list=?
	    f32vector=?
	    f32vector-pred-at-offset?
	    f32vector-index
	    f32vector-count-distinct))


(define* (f32vector-min v #:optional (prec 1.0e-4))
  (let ((n-cell (f32vector-length v)))
    (case n-cell
      ((0) (error "Empty vector: " v))
      ((1) (f32vector-ref v 0))
      (else
       (let ((mini (f32vector-ref v 0)))
	 (do ((i 1
		 (+ i 1)))
	     ((= i n-cell) mini)
	   (let ((val (f32vector-ref v i)))
	     (when (float<?  val mini prec)
	       (set! mini val)))))))))

(define* (f32vector-max v #:optional (prec 1.0e-4))
  (let ((n-cell (f32vector-length v)))
    (case n-cell
      ((0) (error "Empty vector: " v))
      ((1) (f32vector-ref v 0))
      (else
       (let ((maxi (f32vector-ref v 0)))
	 (do ((i 1
		 (+ i 1)))
	     ((= i n-cell) maxi)
	   (let ((val (f32vector-ref v i)))
	     (when (float>? val maxi)
	       (set! maxi val)))))))))

(define (f32vector-copy vector)
  (let* ((n-cell (f32vector-length vector))
	 (copy (make-f32vector n-cell)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell) copy)
      (f32vector-set! copy i (f32vector-ref vector i)))))

(define* (f32vector-complement vector #:key (of 255.0))
  (let* ((n-cell (f32vector-length vector))
	 (copy (make-f32vector n-cell)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell) copy)
      (f32vector-set! copy i
		      (- of (f32vector-ref vector i))))))

(define* (f32vector-and-at-offset vectors i
				  #:optional (prec 1.0e-4))
  (let ((n-vec (length vectors))
	(result #t))
    (do ((j 0
	    (+ j 1)))
	((or (= j n-vec)
	     (not result)) result)
      (unless (float>? (f32vector-ref (list-ref vectors j) i) 0.0 prec)
	(set! result #f)))))

(define* (f32vector-or-at-offset vectors i
				 #:optional (prec 1.0e-4))
  (float>? (f32vector-sum-at-offset vectors i) 0.0 prec))

(define (f32vector-sum-at-offset vectors i)
  (reduce +
	  0.0
	  (map (lambda (vector)
		 (f32vector-ref vector i))
	    vectors)))

(define (f32vector-mean-at-offset vectors i)
  (let ((n-vec (length vectors)))
    (/ (f32vector-sum-at-offset vectors i)
       n-vec)))

(define* (f32vector=-at-offset? vectors i
				#:optional (prec 1.0e-4))
  ;; this procedure assumes:
  ;;   all vectors are of the same length
  ;;   vectors length > 0
  ;;   i is a valid indice
  (let ((n-vec (length vectors))
	(result #t))
    (do ((j 1 ;; we compare all with vec 0, see below
	    (+ j 1)))
	((or (= j n-vec)
	     (not result)) result)
      (unless (float=? (f32vector-ref (list-ref vectors j) i)
		       (f32vector-ref (list-ref vectors 0) i)
		       prec)
	(set! result #f)))))

(define* (f32vector-list=? vectors
			   #:optional (prec 1.0e-4))
  ;; this procedure assumes:
  ;;   all vectors are of the same length
  (match vectors
    ((vector . rest)
     (let ((n-cell (f32vector-length vector))
	   (result #t))
       (case n-cell
	 ((0) #t)
	 (else
	  (do ((i 0
		  (+ i 1)))
	      ((or (= i n-cell)
		   (not result)) result)
	    (unless (f32vector=-at-offset? vectors i prec)
	      (set! result #f)))))))
    ((vector) #t)
    (() #t)))

(define (f32vector=? . vectors)
  ;; this procedure assumes:
  ;;   all vectors are of the same length
  (match vectors
    ((precision . rest)
     (if (number? precision)
	 (f32vector-list=? rest precision)
	 (f32vector-list=? vectors)))
    ((vector) #t)
    (() #t)))

;; the version below is 58% faster. we keep this as an example and for
;; the record.
#;(define* (f32vector-pred-at-offset? pred vectors i)
  (let ((n-vec (length vectors)))
    (catch 'exit
      (lambda ()
	(do ((j 0
		(+ j 1)))
	    ((= j n-vec) #t)
	  (unless (pred (f32vector-ref (list-ref vectors j) i))
	    (throw 'exit #f))))
      (lambda (key value)
	value))))

(define* (f32vector-pred-at-offset? pred vectors i)
  (let ((n-vec (length vectors))
	(result #t))
    (do ((j 0
	    (+ j 1)))
	((or (= j n-vec)
	     (not result)) result)
      (unless (pred (f32vector-ref (list-ref vectors j) i))
	(set! result #f)))))

(define (f32vector-index pred . vectors)
  (let ((n-cell (apply min (map f32vector-length vectors)))
	(result #f))
    (do ((i 0
	    (+ i 1)))
	((or (= i n-cell)
	     result) result)
      (when (f32vector-pred-at-offset? pred vectors i)
	(set! result i)))))

(define* (f32vector-count-distinct v #:optional (prec 1.0e-4))
  (let ((n-cell (f32vector-length v)))
    (case n-cell
      ((0) (values 0 '()))
      ((1) (values 1 (list (f32vector-ref v 0))))
      (else
       (let ((vals (list (f32vector-ref v 0))))
	 (do ((i 1
		 (+ i 1)))
	     ((= i n-cell) (values (length vals) vals))
	   (let ((val (f32vector-ref v i)))
	     (unless (float-member val vals prec)
	       (set! vals
		     (cons val vals))))))))))

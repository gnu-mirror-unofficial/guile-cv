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


(define-module (cv support f64vector)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:use-module (system foreign)
  #:use-module (cv init)
  #:use-module (cv support utils)
  #:use-module (cv support float)
  #:use-module (cv support libguile-cv)

  #:export (f64vector-range
            f64vector-min
	    f64vector-max
            f64vector-reduce
            f64vector-mean
            f64vector-std-dev
	    f64vector-copy
	    f64vector-complement
            f64vector-ref-at-offset
	    f64vector-and-at-offset
	    f64vector-or-at-offset
            f64vector-xor-at-offset
	    f64vector-sum-at-offset
	    f64vector-mean-at-offset
	    f64vector=-at-offset?
	    f64vector-list=?
	    f64vector=?
	    f64vector-pred-at-offset?
	    f64vector-index
	    f64vector-count-distinct
            f64vector-invert
            f64vector-matrix-multiply
            f64vector->f32vector))


(define* (f64vector-range v #:key (prec 1.0e-4))
  (let ((n-cell (f64vector-length v)))
    (case n-cell
      ((0) (error "Empty vector: " v))
      (else
       (do ((mini (f64vector-ref v 0))
            (maxi (f64vector-ref v 0))
            (p-mini 0)
            (p-maxi 0)
            (i 1
               (+ i 1)))
           ((= i n-cell)
            (list mini p-mini maxi p-maxi))
         (let ((val (f64vector-ref v i)))
           (when (float<? val mini prec)
             (set! mini val)
             (set! p-mini i))
           (when (float>? val maxi)
             (set! maxi val)
             (set! p-maxi i))))))))

(define* (f64vector-min v #:key (prec 1.0e-4))
  (match (f64vector-range v #:prec prec)
    ((mini p-mini maxi p-maxi)
     (values mini p-mini))))

(define* (f64vector-max v #:key (prec 1.0e-4))
  (match (f64vector-range v #:prec prec)
    ((mini p-mini maxi p-maxi)
     (values maxi p-maxi))))

(define* (f64vector-reduce v proc default #:key (n-cell #f))
  (let ((n-cell (or n-cell
                    (f64vector-length v))))
    (if (= n-cell 0)
        default
        (do ((i 1
                (+ i 1))
             (prev (f64vector-ref v 0)
                   (proc (f64vector-ref v i) prev)))
            ((= i n-cell) prev)))))

(define* (f64vector-mean v #:key (n-cell #f))
  (let ((n-cell (f64vector-length v)))
    (/ (f64vector-reduce v + 0 #:n-cell n-cell) n-cell)))

(define* (f64vector-std-dev v #:key (n-cell #f) (mean #f))
  (let* ((n-cell (or n-cell (f64vector-length v)))
         (mean (or mean (f64vector-mean v #:n-cell n-cell)))
         (deviations 0.0))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell)
         (sqrt (/ deviations n-cell)))
      (set! deviations
            (+ deviations (expt (- (f64vector-ref v i) mean) 2))))))

(define (f64vector-copy from)
  (let* ((n-cell (f64vector-length from))
	 (copy (make-f64vector n-cell)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell) copy)
      (f64vector-set! copy i (f64vector-ref from i)))))

(define* (f64vector-complement vector #:key (of 255.0))
  (let* ((n-cell (f64vector-length vector))
	 (copy (make-f64vector n-cell)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell) copy)
      (f64vector-set! copy i
		      (- of (f64vector-ref vector i))))))

(define (f64vector-ref-at-offset vectors i)
  (map (lambda (vector)
         (f64vector-ref vector i))
    vectors))

(define* (f64vector-and-at-offset vectors i
				  #:key (prec 1.0e-4))
  (let ((n-vec (length vectors))
	(result #t))
    (do ((j 0
	    (+ j 1)))
	((or (= j n-vec)
	     (not result)) result)
      (unless (float>? (f64vector-ref (list-ref vectors j) i) 0.0 prec)
	(set! result #f)))))

(define* (f64vector-or-at-offset vectors i
				 #:key (prec 1.0e-4))
  (float>? (f64vector-sum-at-offset vectors i) 0.0 prec))

(define (pixel-logior-1 vals result)
  (if (null? vals)
      result
      (let ((a result)
            (b (car vals)))
        (pixel-logior-1 (cdr vals)
                        (logior (logand a (- 255 b))
                                (logand (- 255 a) b))))))

(define (pixel-logior vals)
  (match vals
    ((a) a)
    ((a . rests)
     (pixel-logior-1 rests a))
    (else
     (error "Invalid argument:" vals))))

(define (f64vector-xor-at-offset vectors i)
  (pixel-logior (map (lambda (vector)
                       (float->int (f64vector-ref vector i)))
                  vectors)))

(define (f64vector-sum-at-offset vectors i)
  (reduce +
	  0.0
          (f64vector-ref-at-offset vectors i)))

(define (f64vector-mean-at-offset vectors i)
  (let ((n-vec (length vectors)))
    (/ (f64vector-sum-at-offset vectors i)
       n-vec)))

(define* (f64vector=-at-offset? vectors i
				#:key (prec 1.0e-4))
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
      (unless (float=? (f64vector-ref (list-ref vectors j) i)
		       (f64vector-ref (list-ref vectors 0) i)
		       prec)
	(set! result #f)))))

(define* (f64vector-list=? vectors
			   #:key (prec 1.0e-4))
  ;; this procedure assumes:
  ;;   all vectors are of the same length
  (match vectors
    ((vector . rest)
     (let ((n-cell (f64vector-length vector))
	   (result #t))
       (case n-cell
	 ((0) #t)
	 (else
	  (do ((i 0
		  (+ i 1)))
	      ((or (= i n-cell)
		   (not result)) result)
	    (unless (f64vector=-at-offset? vectors i #:prec prec)
	      (set! result #f)))))))
    ((vector) #t)
    (() #t)))

(define (f64vector=? . vectors)
  ;; this procedure assumes:
  ;;   all vectors are of the same length
  (match vectors
    ((precision . rest)
     (if (number? precision)
	 (f64vector-list=? rest #:prec precision)
	 (f64vector-list=? vectors)))
    ((vector) #t)
    (() #t)))

;; the version below is 58% faster. we keep this as an example and for
;; the record.
#;(define* (f64vector-pred-at-offset? pred vectors i)
  (let ((n-vec (length vectors)))
    (catch 'exit
      (lambda ()
	(do ((j 0
		(+ j 1)))
	    ((= j n-vec) #t)
	  (unless (pred (f64vector-ref (list-ref vectors j) i))
	    (throw 'exit #f))))
      (lambda (key value)
	value))))

(define* (f64vector-pred-at-offset? pred vectors i)
  (let ((n-vec (length vectors))
	(result #t))
    (do ((j 0
	    (+ j 1)))
	((or (= j n-vec)
	     (not result)) result)
      (unless (pred (f64vector-ref (list-ref vectors j) i))
	(set! result #f)))))

(define (f64vector-index pred . vectors)
  (let ((n-cell (apply min (map f64vector-length vectors)))
	(result #f))
    (do ((i 0
	    (+ i 1)))
	((or (= i n-cell)
	     result) result)
      (when (f64vector-pred-at-offset? pred vectors i)
	(set! result i)))))

(define* (f64vector-count-distinct v #:key (prec 1.0e-4))
  (let ((n-cell (f64vector-length v)))
    (case n-cell
      ((0) (values 0 '()))
      ((1) (values 1 (list (f64vector-ref v 0))))
      (else
       (let ((vals (list (f64vector-ref v 0))))
	 (do ((i 1
		 (+ i 1)))
	     ((= i n-cell) (values (length vals) vals))
	   (let ((val (f64vector-ref v i)))
	     (unless (float-member val vals prec)
	       (set! vals
		     (cons val vals))))))))))


;;;
;;; Matrix ops
;;;

(define (f64vector-matrix-multiply v1 width-1 height-1 v2 width-2)
  ;; In math, we'd write:
  ;; 	A[n,m] and B[m,p]
  ;;	n = the number of lines of A
  ;;	m = the number of columns of A
  ;;	    the number of lines of B
  ;;	p = the number of columns of B
  ;; In guile-cv, an image is represented as a list
  ;;	(width height n-chan idata)
  ;; So:
  ;;	n = height-1
  ;;	m = width-1
  ;;	p = width-2
  ;; Here is a 'naive' implementation, till we bind a fast linear algebra
  ;; library, gsl or cblas, but this is a rabbit hole task!
  (let* ((a v1)
         (b v2)
         (n height-1)
         (m width-1)
         (p width-2)
         (n-cell (* n p))
	 (ab (make-f64vector n-cell)))
    (do ((i 0
	    (+ i 1)))
	((= i n) ab)
      (do ((j 0
              (+ j 1)))
          ((= j p))
        (do ((sub 0)
             (k 0
                (+ k 1)))
            ((= k m)
             (f64vector-set! ab (+ (* i p) j) sub))
          (set! sub
                (+ sub (* (f64vector-ref a (+ (* i m) k))
                          (f64vector-ref b (+ (* k p) j))))))))))

(define (f64vector-invert v)
  (let* ((n-cell (f64vector-length v))
	 (to (make-f64vector n-cell)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell) to)
      (f64vector-set! to i
                      (expt (f64vector-ref v i) -1)))))

(define (f64vector->f32vector v)
  (let* ((n-cell (f64vector-length v))
	 (to (make-f32vector n-cell)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell) to)
      (f32vector-set! to i
                      (f64vector-ref v i)))))

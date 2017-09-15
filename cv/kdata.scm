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


(define-module (cv kdata)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:use-module (cv init)
  #:use-module (cv support)


  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (kernel?

            k-make
            k-make-circular-mask
            
            k-width
            k-height
            k-size
            k-kdata
	    
	    k-ref
            k-fast-ref
	    k-set!
	    im-fast-set!

            k-offset
            k-fast-offset

	    k-display

            %k-identity
            %k-edge0
            %k-edge1
            %k-edge2
            %k-sharpen
            %k-box-blur
            %k-gaussian-blur0
            %k-gaussian-blur1
            %k-unsharp
            %k-emboss
            %k-laplacian
            %k-prewitt
            %k-sobel
            %k-prewitt'
            %k-sobel'))


#;(g-export )


;;;
;;; Dimensions
;;;

(define (k-width kernel)
  (match kernel
    ((width height _) width)))

(define (k-height kernel)
  (match kernel
    ((width height _) height)))

(define (k-size kernel)
  (match kernel
    ((width height _) (list width height))))

(define (k-kdata kernel)
  (match kernel
    ((_ _ kdata) kdata)))


;;;
;;; Is?
;;;

(define (kernel? kernel)
  (and (list? kernel)
       (= (length kernel) 3)
       (match kernel
         ((width height kdata)
          (and (odd? width)
               (odd? height)
               (f64vector? kdata)
               (= (f64vector-length kdata)
                  (* width height)))))))


;;;
;;; Make
;;;

(define (k-make-n-val width height vals norm)
  "This returns #f or a NUMBER, value to be used to normalize the
kernel."
  (and vals
       norm
       (if (number? norm)
           norm
           (match vals
             ((a . rest)
              (reduce + 0 vals))
             (a (* width height a))))))

(define (k-make-check-vals width height vals)
  "This returns the result of (* WIDTH HEIGHT) if VALS is valid, and
otherwise raises an error. VALS is valid if it is #f, a number or a list
of (* WIDTH HEIGHT) numbers."
  (let ((n-cell (* width height)))
    (match vals
      ((a . rest)
       (if (= (length vals) n-cell)
           n-cell
           (error
            "Kernel vals dimension mismatch:" width height (length vals))))
      (#f n-cell)
      (a (if (and (number? a)
                  (not (zero? a)))
             n-cell
             (error "Kernel wrong nomr argument:" a))))))

(define* (k-make width height #:optional (vals #f) (norm #f))
  (if (and (odd? width)
           (odd? height))
      (let* ((n-cell (k-make-check-vals width height vals))
             (to (make-f64vector n-cell))
             (n-val (k-make-n-val width height vals norm)))
        (do ((i 0
                (+ i 1)))
            ((= i n-cell)
             (list width height
                   (if vals
                       to
                       (let ((i (float->int (/ height 2)))
                             (j (float->int (/ width 2))))
                         (f64vector-set! to
                                         (k-fast-offset i j width)
                                         1.0)
                         to))))
          (f64vector-set! to i
                          (match vals
                            ((a . rest)
                             (if n-val
                                 (/ (list-ref vals i) n-val)
                                 (list-ref vals i)))
                            (#f 0.0)
                            (a (if n-val (/ a n-val) a))))))
      (error "Kernel width and height must be odd values: " width height)))

(define (make-lradii radius)
  (let* ((radius (cond ((and (float>=? radius 1.5)
                             (float<=? radius 1.75))
                        1.75)
                       ((and (float>=? radius 2.5)
                             (float<=? radius 2.85))
                        2.85)
                       (else
                        radius)))
         (r2 (float->int (+ (* radius radius) 1)))
         (kradius (float->int (sqrt (+ r2 1e-10))))
         (2kradius (* 2 kradius))
         (2kradius1 (+ 2kradius 1))
         (kheight 2kradius1)
         (lradii (make-s32vector (* 2 kheight) 0.0))
         (npoints kheight))
    (s32vector-set! lradii 2kradius (- kradius))
    (s32vector-set! lradii 2kradius1 kradius)
    #;(dimfi 2kradius 2kradius1 lradii npoints)
    (do ((y 1
            (+ y 1)))
        ((> y kradius)
         (values lradii npoints kradius))
      (let* ((dx (float->int (sqrt (+ (- r2 (* y y)) 1e-10))))
             (-dx (- dx))
             (a (* 2 (- kradius y)))
             (b (+ a 1))
             (c (* 2 (+ kradius y)))
             (d (+ c 1)))
        (s32vector-set! lradii a -dx)
        (s32vector-set! lradii b dx)
        (s32vector-set! lradii c -dx)
        (s32vector-set! lradii d dx)
        (set! npoints (+ npoints (+ (* 4 dx) 2)))))))

(define* (k-make-circular-mask radius #:optional (val 1) (norm #f))
  (if (float>=? radius 0.5)
      (receive (lradii npoints kradius)
          (make-lradii radius)
        #;(dimfi lradii npoints kradius)
        (let* ((width (+ (* 2 kradius) 1))
               (w/2 (float->int (/ width 2)))
               (height width)
               (kernel (make-f64vector (* width height) 0.0))
               (k-val (if norm
                          (/ val (* val npoints))
                          val)))
          (do ((y 0
                  (+ y 1)))
              ((= y height)
               (list width height kernel))
            (do ((x (+ w/2 (s32vector-ref lradii (* 2 y)))
                    (+ x 1)))
                ((> x (+ w/2 (s32vector-ref lradii (+ (* 2 y) 1)))))
              (f64vector-set! kernel
                              (+ x (* y width))
                              k-val)))))
      (error "Radius must be a float > 0:" radius)))


;;;
;;; Accessors
;;;

(define (k-offset i j width height)
  (if (and (>= i 0)
	   (>= j 0)
	   (< i height)
	   (< j width))
      (+ (* i width) j)
      (error "Out of bound: " i j)))

(define (k-fast-offset i j width)
  (+ (* i width) j))

(define (k-ref kernel i j)
  (match kernel
    ((width height kdata)
     (f64vector-ref kdata (k-offset i j width height)))))

(define (k-fast-ref kernel i j width)
  (match kernel
    ((width height kdata)
     (f64vector-ref kdata (k-fast-offset i j width)))))

(define (k-set! kernel i j val)
  (match kernel
    ((width height kdata)
     (f64vector-set! kdata (k-offset i j width height) val))))
     
(define (k-fast-set! kernel i j val)
  (match kernel
    ((width height kdata)
     (f64vector-set! kdata (k-fast-offset i j width) val))))


;;;
;;; Display
;;;

(define* (k-display kernel
                    #:key (proc #f)
                    (port (current-output-port)))
  (let ((proc (if proc
                  proc
                  (lambda (val)
                    (if (float>=? val 1000.0 0)
                        (format #f "~9e" val)
                        (format #f "~9,5,,,f" val))))))
    (newline port)
    (match kernel
      ((width height kdata)
       (do ((i 0
               (+ i 1)))
           ((= i height))
         (do ((j 0
                 (+ j 1)))
             ((>= j width) (newline port))
           (format port "  ~A"
                   (proc (k-fast-ref kernel i j width)))))))
    (newline port)))


;;;
;;; Kernels
;;;

(define %k-identity
  (k-make 3 3))

(define %k-edge0
  (k-make 3 3
          '(1 0 -1 0 0 0 -1 0 1)))

(define %k-edge1
  (k-make 3 3
          '(0 1 0 1 -4 1 0 1 0)))

(define %k-edge2
  (k-make 3 3
          '(-1 -1 -1 -1 8 -1 -1 -1 -1)))

(define %k-sharpen
  (k-make 3 3
          '(0 -1 0 -1 5 -1 0 -1 0)))

(define %k-box-blur
  (k-make 3 3
          '(1 1 1 1 1 1 1 1 1)
          #t))

(define %k-gaussian-blur0
  (k-make 3 3
          '(1 2 1 2 4 2 1 2 1)
          #t)) ;; 16

(define %k-gaussian-blur1
  (k-make 5 5 
          '(1 4 6 4 1
            4 16 24 16 4
            6 24 36 24 6
            4 16 24 16 4
            1 4 6 4 1)
          #t)) ;; 256

(define %k-unsharp
  (k-make 5 5 
          '(1 4 6 4 1
            4 16 24 16 4
            6 24 -476 24 6
            4 16 24 16 4
            1 4 6 4 1)
          #t)) ;; -256

(define %k-emboss
  (k-make 3 3
          '(-2 -2 0 -2 6 0 0 0 0)))

(define %k-laplacian
  (k-make 3 3
          '(0.375 0.25 0.375 0.25 -2.5 0.25 0.375 0.25 0.375)))

(define %k-prewitt
  (k-make 3 3
          '(1 1 1 0 0 0 -1 -1 -1)))

(define %k-prewitt'
  (k-make 3 3
          '(1 0 -1 1 0 -1 1 0 -1)))

(define %k-sobel
  (k-make 3 3
          '(1 2 1 0 0 0 -1 -2 -1)))

(define %k-sobel'
  (k-make 3 3
          '(1 0 -1 2 0 -2 1 0 -1)))

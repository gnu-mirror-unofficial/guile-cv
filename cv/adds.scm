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


(define-module (cv adds)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)
  
  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (#;im-map
	    im-rgb->grey
            im-rgba->rgb
            im-rgba->grey
	    im-threshold
            im-add
            im-add-channel
            im-substract
            im-substract-channel
            im-multiply
            im-multiply-channel
            im-divide
            im-divide-channel
	    im-and
	    im-or
	    im-complement
	    im-min
	    im-max
            im-transpose
            im-transpose-channel
            im-normalize
            im-normalize-channel))


#;(g-export )


;;;
;;; Guile-CV additional API
;;;

(define (im-rgb->grey-1 c r g b i mini maxi total n-cell)
  (if (= i n-cell)
      (list mini maxi (round (/ total n-cell)) n-cell)
      (let ((k (/ (+ (f32vector-ref r i)
		     (f32vector-ref g i)
		     (f32vector-ref b i))
		  3)))
	(f32vector-set! c i k)
	(im-rgb->grey-1 c r g b
			(+ i 1)
			(float-round (min mini k) 1)
			(float-round (max maxi k) 1)
			(+ total k)
			n-cell))))

(define (im-rgb->grey image)
  (match image
    ((width height n-chan idata)
     (case n-chan
       ((1)
	image)
       ((3)
	(match idata
	  ((r g b)
	   (let* ((c (im-make-channel width height))
		  (vals (im-rgb->grey-1 c r g b 0 0 0 0 (* width height))))
	     (values (list width height 1 (list c))
		     vals)))))
       (else
	(error "Not an RGB (nor a GREY) image."))))))

#!
Source => Target = (BGColor + Source) =
Target.R = ((1 - Source.A) * BGColor.R) + (Source.A * Source.R)
Target.G = ((1 - Source.A) * BGColor.G) + (Source.A * Source.G)
Target.B = ((1 - Source.A) * BGColor.B) + (Source.A * Source.B)
!#

(define* (im-rgba->rgb image #:key (bg '(0.0 0.0 0.0)))
  (match bg
    ((bg-r bg-g bg-b)
     (match image
       ((width height n-chan idata)
        (case n-chan
          ((4)
           (match idata
             ((r g b a)
              (let ((a-norm (im-normalize-channel a width height)))
                (list width height 3
                      (par-map (lambda (vals)
                                 (match vals
                                   ((c bg)
                                    (im-rgba-channel->rgb-channel c width height
                                                                  a-norm #:bg bg))))
                          (list (list r bg-r)
                                (list g bg-g)
                                (list b bg-b))))))))
          (else
           (error "Not an RGBA image."))))))
    (else
     (error "Invalid background colour: " bg))))

(define* (im-rgba-channel->rgb-channel c width height a-norm #:key (bg 0.0))
  (let ((c-norm (im-normalize-channel c width height))
        (bg-norm (/ bg 255.0))
        (to (im-make-channel width height))
        (n-cell (* width height)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell))
      (f32vector-set! to i
                      (* (if (= bg 0.0)
                             (* (f32vector-ref a-norm i) (f32vector-ref c-norm i))
                             (+ (* (- 1.0 (f32vector-ref a-norm i)) bg-norm)
                                (* (f32vector-ref a-norm i) (f32vector-ref c-norm i))))
                         255.0)))
    to))

(define* (im-rgba->grey image #:key (bg '(0.0 0.0 0.0)))
  (match image
    ((_ _ n-chan idata)
     (case n-chan
       ((3 1)
        (im-rgb->grey image))
       ((4)
        (im-rgb->grey (im-rgba->rgb image #:bg bg)))
       (else
	(error "Not an RGBA (nor an RGB neither a GREY) image."))))))

(define* (im-threshold image threshold #:key (bg 'dark) (prec 1.0e-4))
  (if (and (>= threshold 0.0)
	   (<= threshold 255.0))
      (match (match image
               ((_ _ n-chan _)
                (case n-chan
                  ((1) image)
                  ((3) (im-rgb->grey image))
                  ((4) (im-rgba->grey image))
                  (else
                   (error "Not a GREY, RGB, nor an RGBA image.")))))
	((width height n-chan idata)
	 (match idata
	   ((c)
	    (let ((c-copy (im-make-channel width height))
		  (op (case bg
			((dark) float>=?)
			((light) float<=?)
			(else
			 (error "Invalid background: " bg)))))
	      (do ((i 0
		      (+ i 1)))
		  ((= i (* width height))
		   (list width height 1 (list c-copy)))
		(when (op (f32vector-ref c i) threshold prec)
		  (f32vector-set! c-copy i 255.0))))))))
      (error "Invalid threshold: " threshold)))

(define (im-add image val)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
	   (let ((map-proc (if (> n-chan 1) par-map map)))
	     (map-proc (lambda (channel)
			 (im-add-channel channel width height val))
	       idata))))))

(define (im-add-channel channel width height val)
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell))
      (let ((new-val (+ (f32vector-ref channel i) val)))
        (f32vector-set! to i (if (float>=? new-val 255.0) 255.0 new-val))))
    to))

(define (im-substract image val)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
	   (let ((map-proc (if (> n-chan 1) par-map map)))
	     (map-proc (lambda (channel)
			 (im-substract-channel channel width height val))
	       idata))))))

(define (im-substract-channel channel width height val)
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell))
      (let ((new-val (- (f32vector-ref channel i) val)))
        (f32vector-set! to i (if (float<=? new-val 0.0) 0.0 new-val))))
    to))

(define (im-multiply image val)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
	   (let ((map-proc (if (> n-chan 1) par-map map)))
	     (map-proc (lambda (channel)
			 (im-multiply-channel channel width height val))
	       idata))))))

(define (im-multiply-channel channel width height val)
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell))
      (let ((new-val (* (f32vector-ref channel i) val)))
        (f32vector-set! to i (if (float>=? new-val 255.0) 255.0 new-val))))
    to))

(define (im-divide image val)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
	   (let ((map-proc (if (> n-chan 1) par-map map)))
	     (map-proc (lambda (channel)
			 (im-divide-channel channel width height val))
	       idata))))))

(define (im-divide-channel channel width height val)
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell))
      (let ((new-val (/ (f32vector-ref channel i) val)))
        (f32vector-set! to i (if (float<=? new-val 0.0) 0.0 new-val))))
    to))

(define (im-and . images)
  (match images
    ((image . rest)
     (match image
       ((width height n-chan _)
	(if (and (apply = (cons width (im-collect rest 'width)))
		 (apply = (cons height (im-collect rest 'height))))
	    (let ((img-2 (im-copy image))
		  (n-cell (* width height))
		  (c-channels (im-collect (map im-rgb->grey rest) 'grey)))
	      (list width height n-chan
		    (let ((map-proc (if (> n-chan 1) par-map map)))
		      (map-proc
		       (lambda (channel)
			 (do ((i 0
				 (+ i 1)))
			     ((= i n-cell) channel)
			   (unless (f32vector-and-at-offset c-channels i)
			     (f32vector-set! channel i 0.0))))
		       (match img-2 ((_ _ _ idata) idata))))))
	    (error "Size mismatch.")))))
    ((image) image)
    (() (error "Invalid argument: " images))))

(define (im-or . images)
  (match images
    ((image . rest)
     (match image
       ((width height n-chan _)
	(if (and (apply = (cons width (im-collect rest 'width)))
		 (apply = (cons height (im-collect rest 'height))))
	    (let ((img-2 (im-copy image))
		  (n-cell (* width height))
		  (c-channels (im-collect (map im-rgb->grey rest) 'grey)))
	      (list width height n-chan
		    (let ((map-proc (if (> n-chan 1) par-map map)))
		      (map-proc
		       (lambda (channel)
			 (do ((i 0
				 (+ i 1)))
			     ((= i n-cell) channel)
			   (unless (> (f32vector-ref channel i) 0.0)
			     (f32vector-set! channel i
					     (f32vector-mean-at-offset c-channels i)))))
		       (match img-2 ((_ _ _ idata) idata))))))
	    (error "Size mismatch.")))))
    ((image) image)
    (() (error "Invalid argument: " images))))

(define (im-complement image)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
	   (let ((map-proc (if (> n-chan 1) par-map map)))
	     (map-proc
	      (lambda (channel)
		(f32vector-complement channel))
	      idata))))))

(define (im-min image)
    (match image
    ((width height n-chan idata)
     (let ((map-proc (if (> n-chan 1) par-map map)))
       (map-proc
	(lambda (channel)
	  (f32vector-min channel))
	idata)))))

(define (im-max image)
    (match image
    ((width height n-chan idata)
     (let ((map-proc (if (> n-chan 1) par-map map)))
       (map-proc
	(lambda (channel)
	  (f32vector-max channel))
	idata)))))

(define (im-transpose image)
  (match image
    ((width height n-chan idata)
     (list height width n-chan
	   (let ((map-proc (if (> n-chan 1) par-map map)))
	     (map-proc (lambda (channel)
			 (im-transpose-channel channel width height))
	       idata))))))

(define (im-transpose-channel channel width height)
  (let ((t-width height)
        (to (im-make-channel height width)))
    (do ((i 0
	    (+ i 1)))
	((= i height))
      (do ((j 0
	      (+ j 1)))
	  ((= j width))
        (im-fast-channel-set! to j i t-width
                              (im-fast-channel-ref channel i j width))))
    to))

(define (im-normalize image)
  (match image
    ((width height n-chan idata)
     (list height width n-chan
	   (let ((map-proc (if (> n-chan 1) par-map map)))
	     (map-proc (lambda (channel)
			 (im-normalize-channel channel width height))
	       idata))))))

(define (im-normalize-channel channel width height)
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell))
      (f32vector-set! to i (/ (f32vector-ref channel i)
                              255)))
    to))

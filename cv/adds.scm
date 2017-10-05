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


(define-module (cv adds)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (search basic)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)
  #:use-module (cv imgproc)
  #:use-module (cv segmentation)
  #:use-module (cv features)
  
  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (#;im-map
	    im-rgb->gray
            im-rgba->rgb
            im-rgba->gray
	    im-threshold
	    im-and
	    im-or
            im-xor
	    im-complement
            im-range
	    im-min
	    im-max
            im-map
            im-map-channel
            im-reduce
            im-reduce-channel
            im-invert
            im-invert-channel
            im-transpose
            im-transpose-channel
            im-normalize
            im-normalize-channel
            im-scrap
            im-particles
            im-particle-clean))


(g-export im-add
          im-add-channel
          im-subtract
          im-subtract-channel
          im-multiply
          im-multiply-channel
          im-divide
          im-divide-channel)


;;;
;;; Guile-CV additional API
;;;

(define (im-rgb->gray-1 c r g b i mini maxi total n-cell)
  (if (= i n-cell)
      (list mini maxi (round (/ total n-cell)) n-cell)
      (let ((k (/ (+ (f32vector-ref r i)
		     (f32vector-ref g i)
		     (f32vector-ref b i))
		  3)))
	(f32vector-set! c i k)
	(im-rgb->gray-1 c r g b
			(+ i 1)
			(float-round (min mini k) 1)
			(float-round (max maxi k) 1)
			(+ total k)
			n-cell))))

(define (im-rgb->gray image)
  (match image
    ((width height n-chan idata)
     (case n-chan
       ((1)
	image)
       ((3)
	(match idata
	  ((r g b)
	   (let* ((c (im-make-channel width height))
		  (vals (im-rgb->gray-1 c r g b 0 0 0 0 (* width height))))
	     (values (list width height 1 (list c))
		     vals)))))
       (else
	(error "Not an RGB (nor a GRAY) image."))))))

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
                      (let ((map-proc (if (%use-par-map) par-map map)))
                        (map-proc (lambda (vals)
                                    (match vals
                                      ((c bg)
                                       (im-rgba-channel->rgb-channel c width height
                                                                     a-norm #:bg bg))))
                                  (list (list r bg-r)
                                        (list g bg-g)
                                        (list b bg-b)))))))))
          (else
           (error "Not an RGBA image."))))))
    (else
     (error "Invalid background color: " bg))))

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

(define* (im-rgba->gray image #:key (bg '(0.0 0.0 0.0)))
  (match image
    ((_ _ n-chan idata)
     (case n-chan
       ((3 1)
        (im-rgb->gray image))
       ((4)
        (im-rgb->gray (im-rgba->rgb image #:bg bg)))
       (else
	(error "Not an RGBA (nor an RGB neither a GRAY) image."))))))

(define* (im-threshold image threshold #:key (bg 'black) (prec 1.0e-4))
  (if (and (>= threshold 0.0)
	   (<= threshold 255.0))
      (match (match image
               ((_ _ n-chan _)
                (case n-chan
                  ((1) image)
                  ((3) (im-rgb->gray image))
                  ((4) (im-rgba->gray image))
                  (else
                   (error "Not a GRAY, RGB, nor an RGBA image.")))))
	((width height n-chan idata)
	 (match idata
	   ((c)
	    (let ((c-copy (im-make-channel width height))
		  (op (case bg
			((black) float>=?)
			((white) float<=?)
			(else
			 (error "Invalid background: " bg)))))
	      (do ((i 0
		      (+ i 1)))
		  ((= i (* width height))
		   (list width height 1 (list c-copy)))
		(when (op (f32vector-ref c i) threshold prec)
		  (f32vector-set! c-copy i 255.0))))))))
      (error "Invalid threshold: " threshold)))

(define (im-matrix-op image img-2 op)
  (match image
    ((width height n-chan idata)
     (match img-2
       ((width-2 height-2 n-chan-2 idata-2)
        (if (and (= width width-2)
                 (= height height-2)
                 (= n-chan n-chan-2))
            (list width height n-chan
                  (let ((map-proc (if (and (> n-chan 1)
                                           (%use-par-map)) par-map map)))
                    (map-proc (lambda (channels)
                                (match channels
                                  ((c1 c2)
                                   (im-matrix-channel-op c1 width height c2 op))))
                              (zip idata idata-2))))
            (error "Size missmatch.")))))))

(define (im-matrix-channel-op channel width height channel-2 op)
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell) to)
    (f32vector-set! to i (op (f32vector-ref channel i)
                             (f32vector-ref channel-2 i))))))

(define (matrix-multiply c1 width-1 height-1 c2 width-2)
  (f32vector-matrix-multiply c1 width-1 height-1 c2 width-2))

(define (matrix-divide c1 width-1 height-1 c2 width-2)
  (f32vector-matrix-multiply c1 width-1 height-1
                             (f32vector-invert c2) width-2))

(define (im-matrix-multdiv-op img-1 img-2 op)
  ;; The product is defined only if the number of columns in img-1 is
  ;; equal to the number of rows in img-2.
  ;; The division is a multiplication by the inverse (of img-2)
  (match img-1
    ((width-1 height-1 n-chan-1 idata-1)
     (match img-2
       ((width-2 height-2 n-chan-2 idata-2)
        (if (= width-1 height-2)
            (list width-2 height-1 n-chan-1
                  (let ((map-proc (if (and (> n-chan-1 1)
                                           (%use-par-map)) par-map map)))
                    (map-proc (lambda (channels)
                                (match channels
                                  ((c1 c2)
                                   (op c1 width-1 height-1 c2 width-2))))
                              (zip idata-1 idata-2))))
            (error "Size missmatch.")))))))


(define-method (im-add image (val <number>))
  (im-map (lambda (p-val) (+ p-val val)) image))

(define-method (im-add-channel channel width height (val <number>))
  (im-map-channel (lambda (p-val) (+ p-val val)) width height channel))

(define-method (im-add . images)
  (apply im-map + images))

(define-method (im-add-channel width height . channels)
  (apply im-map-channel + width height channels))


(define-method (im-subtract image (val <number>))
  (im-map (lambda (p-val) (- p-val val)) image))

(define-method (im-subtract-channel channel width height (val <number>))
  (im-map-channel (lambda (p-val) (- p-val val)) width height channel))

(define-method (im-subtract . images)
  (apply im-map - images))

(define-method (im-subtract-channel width height . channels)
  (apply im-map-channel - width height channels))


(define-method (im-multiply image (val <number>))
  (im-map (lambda (p-val) (* p-val val)) image))

(define-method (im-multiply-channel channel width height (val <number>))
  (im-map-channel (lambda (p-val) (* p-val val)) width height channel))

(define (im-multiply-1 prev images)
  (if (null? images)
      prev
      (match images
        ((ii . rest)
         (im-multiply-1 (im-matrix-multdiv-op prev ii matrix-multiply)
                        rest)))))

(define-method (im-multiply . images)
  (match images
    ((i1 . rest)
     (im-multiply-1 i1 rest))
    (else
     (error "Wrong arguments:" images))))

(define (im-multiply-channel-1 channel width height rest)
  (if (null? rest)
      (values channel width height)
      (match rest
        ((channel-i width-i height-i . rest)
         (im-multiply-channel-1 (f32vector-matrix-multiply channel width height
                                                           channel-i width-i)
                                width-i height rest)))))

(define-method (im-multiply-channel . rest)
  (match rest
    ((channel width height . rest)
     (im-multiply-channel-1 channel width height rest))
    (else
     (error "Wrong arguments:" rest))))


(define-method (im-divide image (val <number>))
  (im-map (lambda (p-val) (/ p-val val)) image))

(define-method (im-divide-channel channel width height (val <number>))
  (im-map-channel (lambda (p-val) (/ p-val val)) width height channel))


(define (im-divide-1 prev images)
  (if (null? images)
      prev
      (match images
        ((ii . rest)
         (im-divide-1 (im-matrix-multdiv-op prev ii matrix-divide)
                        rest)))))

(define-method (im-divide . images)
  (match images
    ((i1 . rest)
     (im-divide-1 i1 rest))
    (else
     (error "Wrong arguments:" images))))

(define (im-divide-channel-1 channel width height rest)
  (if (null? rest)
      (values channel width height)
      (match rest
        ((channel-i width-i height-i . rest)
         (im-divide-channel-1 (f32vector-matrix-multiply channel width height
                                                         (f32vector-invert channel-i)
                                                         width-i)
                                width-i height rest)))))

(define-method (im-divide-channel . rest)
  (match rest
    ((channel width height . rest)
     (im-divide-channel-1 channel width height rest))
    (else
     (error "Wrong arguments:" rest))))


(define (im-range image)
  (match image
    ((width height n-chan idata)
     (match idata
       ((c)
        (im-range-channel c width))
       (else
        (let ((map-proc (if (%use-par-map) par-map map)))
          (map-proc (lambda (channel)
                      (im-range-channel channel width))
              idata)))))))

(define (im-range-channel channel width)
  (match (f32vector-range channel)
    ((mini p-mini maxi p-maxi)
     (list mini (quotient p-mini width) (remainder p-mini width)
           maxi (quotient p-maxi width) (remainder p-maxi width)))))

(define (im-min image)
  (match image
    ((width height n-chan idata)
     (match idata
       ((c)
        (im-min-channel c width))
       (else
        (let ((map-proc (if (%use-par-map) par-map map)))
          (map-proc (lambda (channel)
                      (receive (val row col)
                          (im-min-channel channel width)
                        (list val row col)))
              idata)))))))

(define (im-min-channel channel width)
  (receive (val pos)
      (f32vector-min channel)
    (values val (quotient pos width) (remainder pos width))))

(define (im-max image)
  (match image
    ((width height n-chan idata)
     (match idata
       ((c)
        (im-max-channel c width))
       (else
        (let ((map-proc (if (%use-par-map) par-map map)))
          (map-proc (lambda (channel)
                      (receive (val row col)
                          (im-max-channel channel width)
                        (list val row col)))
              idata)))))))

(define (im-max-channel channel width)
  (receive (val pos)
      (f32vector-max channel)
    (values val (quotient pos width) (remainder pos width))))

(define (im-reduce image proc default)
  (match image
    ((width height n-chan idata)
     (match idata
       ((c)
        (im-reduce-channel c proc default))
       (else
        (let ((map-proc (if (%use-par-map) par-map map)))
          (map-proc (lambda (channel)
                      (im-reduce-channel channel proc default))
              idata)))))))

(define (im-reduce-channel channel proc default)
  (f32vector-reduce channel proc default))

(define (im-map proc . images)
  (match images
    ((image . rest)
     (match image
       ((width height n-chan _)
	(if (and (apply = (apply im-collect 'width rest))
		  (apply = (apply im-collect 'height rest))
		  (apply = (apply im-collect 'n-channel rest)))
            (list width height n-chan
                  (let ((map-proc (if (and (> n-chan 1)
                                           (%use-par-map)) par-map map)))
                    (map-proc (lambda (channels)
                                (apply im-map-channel proc width height channels))
                        (apply zip (apply im-collect 'channels images)))))
	    (error "Size mismatch.")))))
    ((image)
     (match image
       ((width height n-chan idata)
        (list width height n-chan
              (let ((map-proc (if (and (> n-chan 1)
                                       (%use-par-map)) par-map map)))
                (map-proc (lambda (channel)
                            (im-map-channel proc width height channel))
                    idata))))))
    (()
     (error "Invalid argument: " images))))

(define (im-map-channel proc width height . channels)
  (let ((to (im-make-channel width height))
        (n-cell (* width height))
        (n-channels (length channels)))
    (if (> n-channels 1)
        (do ((i 0
                (+ i 1)))
            ((= i n-cell))
          (f32vector-set! to i
                          (apply proc
                                 (f32vector-ref-at-offset channels i))))
        (do ((c (car channels))
             (i 0
                (+ i 1)))
            ((= i n-cell))
          (f32vector-set! to i
                            (proc (f32vector-ref c i)))))
    to))

(define (im-and . images)
  (match images
    ((image . rest)
     (match image
       ((width height n-chan _)
	(if (and (apply = (apply im-collect 'width rest))
		  (apply = (apply im-collect 'height rest))
		  (apply = (apply im-collect 'n-channel rest)))
	    (let ((img-2 (im-copy image))
		  (n-cell (* width height))
		  (c-channels (apply im-collect 'gray (map im-rgb->gray rest))))
	      (list width height n-chan
                    (let ((map-proc (if (and (> n-chan 1)
                                             (%use-par-map)) par-map map)))
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
	(if (and (apply = (apply im-collect 'width rest))
		  (apply = (apply im-collect 'height rest))
		  (apply = (apply im-collect 'n-channel rest)))
	    (let ((img-2 (im-copy image))
		  (n-cell (* width height))
		  (c-channels (apply im-collect 'gray (map im-rgb->gray rest))))
	      (list width height n-chan
                    (let ((map-proc (if (and (> n-chan 1)
                                             (%use-par-map)) par-map map)))
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

(define (im-xor . images)
  (match images
    ((image . rest)
     (match image
       ((width height n-chan _)
	(if  (and (apply = (apply im-collect 'width rest))
		  (apply = (apply im-collect 'height rest))
		  (apply = (apply im-collect 'n-channel rest)))
            (list width height n-chan
                  (let ((map-proc (if (and (> n-chan 1)
                                           (%use-par-map)) par-map map)))
                    (map-proc (lambda (channels)
                                (im-xor-channels channels width height))
                        (apply zip (apply im-collect 'channels images)))))
	    (error "Size mismatch.")))))
    ((image) image)
    (() (error "Invalid argument: " images))))

(define (im-xor-channels channels width height)
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell))
      (f32vector-set! to i
                      (f32vector-xor-at-offset channels i)))
    to))

(define (im-complement image)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc
	      (lambda (channel)
		(f32vector-complement channel))
	      idata))))))

(define (im-transpose image)
  (match image
    ((width height n-chan idata)
     (list height width n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
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

(define (im-invert image)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-invert-channel channel width height))
                       idata))))))

(define (im-invert-channel channel width height)
  (f32vector-invert channel))

(define* (im-normalize image #:key (val 255.0))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-normalize-channel channel width height #:val val))
	       idata))))))

(define* (im-normalize-channel channel width height #:key (val 255.0))
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell))
      (f32vector-set! to i (/ (f32vector-ref channel i)
                              val)))
    to))

(define* (im-scrap image val #:key (pred <) (con 8) (bg 'black))
  ;; (im-binary? image) is rather expensive
  (match image
    ((width height n-chan idata)
     ;; so we only check for n-chan
     (match idata
       ((channel)
        (receive (l-image n-label)
            (im-label image #:con con #:bg bg)
          (match l-image
            ((_ _ _ l-idata)
             (match l-idata
               ((l-channel)
                (let* ((features (im-features image l-image #:n-label n-label))
                       (n-feature (length features))
                       (to-scrap (fold (lambda (feature i prev)
                                         (match feature
                                           ((area . rest)
                                            (if (pred area val)
                                                (cons i prev)
                                                prev))))
                                       '()
                                       features
                                       (iota n-feature))))
                  (list width height 1
                        (list (im-scrap-channel channel l-channel width height
                                                to-scrap n-label))))))))))
       (else
        (error "Not a binary image."))))))

#!
;; nice but too slow
(define (im-scrap-channel channel l-channel width height to-scrap)
  (let ((to (im-copy-channel channel width height))
        (n-cell (* width height))
        (to-scrap (list->vector (reverse! to-scrap))))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell))
      (let ((val (f32vector-ref l-channel i)))
        (f32vector-set! to i
                        (if (or (= val 0.0)
                                (binary-search-sorted-vector to-scrap val))
                            0.0
                            (f32vector-ref channel i)))))
    to))
!#

(define (scrap-cache to-scrap n-label)
  (let ((n-scrap (length to-scrap))
         (cache (make-vector (+ n-label 1) #f)))
    (do ((i 0
            (+ i 1)))
	((= i n-scrap))
      (vector-set! cache (list-ref to-scrap i) #t))
    cache))

(define* (im-scrap-channel channel l-channel width height to-scrap n-label)
  (let* ((to (im-copy-channel channel width height))
         (n-cell (* width height))
         (cache (scrap-cache to-scrap n-label)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell))
      (let ((val #;(inexact->exact (f32vector-ref l-channel i))
                 (float->int (f32vector-ref l-channel i))))
        (f32vector-set! to i
                        (if (or (zero? val)
                                (vector-ref cache val))
                            0.0
                            (f32vector-ref channel i)))))
    to))

(define* (im-particles image features #:key (clean #t))
  (let ((map-proc (if (%use-par-map) par-map map)))
    (map-proc (lambda (prop)
                (match prop
                  ((area left top right bottom . rest)
                   (parameterize ((%use-par-map #f))
                     (let ((particle (im-crop image left top (+ right 1) (+ bottom 1))))
                       (if clean
                           (im-particle-clean particle)
                           particle))))))
              (cdr features))))

(define (im-particle-clean particle)
  (let* ((binary? (im-binary? particle))
         (binary (if binary? particle (im-threshold particle 1.0)))
         (cleaned (match binary
                    ((width height n-chan idata)
                     (receive (label-im n-label)
                         (im-label binary)
                       (let* ((r (- width 1))
                              (b (- height 1))
                              (l-channel (im-channel label-im 0))
                              (n-cel (* width height))
                              (to-remove (fold (lambda (prop i prev)
                                                 (match prop
                                                   ((size left top right bottom . rest)
                                                    (if (or (not (= left 0))
                                                            (not (= top 0))
                                                            (not (= right r))
                                                            (not (= bottom b)))
                                                        (cons i prev)
                                                        prev))))
                                               '()
                                               (im-features binary label-im
                                                            #:n-label n-label)
                                               (iota (+ n-label 1))))
                              (cache (scrap-cache to-remove n-label)))
                         (do ((i 0
                                 (+ i 1)))
                             ((= i n-cel)
                              (list width height n-chan (list l-channel)))
                           (let ((val
                                  (float->int (f32vector-ref l-channel i))))
                             (when (vector-ref cache val)
                               (f32vector-set! l-channel i 0.0))))))))))
    (if binary? cleaned (im-and particle cleaned))))

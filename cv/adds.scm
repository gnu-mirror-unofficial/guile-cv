;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2017
;;;; David Pirotte <david at altosw dot be>

;;;; This file is part of Guile-CV

;;;; Guile-CV is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU Lesser General Public License as
;;;; published by the Free Software Foundation; either version 3 of the
;;;; License, or (at your option) any later version.

;;;; Guile-CV is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.

;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with Guile-CV.  If not, see
;;;; <https://www.gnu.org/licenses/lgpl.html>.
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
  #:use-module (cv properties)
  
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
	    im-and
	    im-or
	    im-complement
	    im-min
	    im-max
            im-inverse
            im-inverse-channel
            im-transpose
            im-transpose-channel
            im-normalize
            im-normalize-channel
            im-scrap
            im-particles
            im-particle-clean
            im-histogram
            im-glue))


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

(define (im-scalar-op image val op)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-scalar-channel-op channel width height val op))
                       idata))))))

(define (im-scalar-channel-op channel width height val op)
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (do ((i 0
	    (+ i 1)))
	((= i n-cell) to)
      (f32vector-set! to i (op (f32vector-ref channel i) val)))))

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
  (im-scalar-op image val +))

(define-method (im-add-channel channel width height (val <number>))
  (im-scalar-channel-op channel width height val +))

(define-method (im-add image (img-2 <list>))
  (im-matrix-op image img-2 +))

(define-method (im-add-channel channel width height (channel-2 <uvec>))
  (im-matrix-channel-op channel width height channel-2 +))


(define-method (im-subtract image (val <number>))
  (im-scalar-op image val -))

(define-method (im-subtract-channel channel width height (val <number>))
  (im-scalar-channel-op channel width height val -))

(define-method (im-subtract image (img-2 <list>))
  (im-matrix-op image img-2 -))

(define-method (im-subtract-channel channel width height (channel-2 <uvec>))
  (im-matrix-channel-op channel width height channel-2 -))


(define-method (im-multiply image (val <number>))
  (im-scalar-op image val *))

(define-method (im-multiply-channel channel width height (val <number>))
  (im-scalar-channel-op channel width height val *))

(define-method (im-multiply img-1 (img-2 <list>))
  (im-matrix-multdiv-op img-1 img-2 im-multiply-channel))

(define-method (im-multiply-channel c1 width-1 height-1 (c2 <uvec>) width-2)
  (f32vector-matrix-multiply c1 width-1 height-1 c2 width-2))


(define-method (im-divide image (val <number>))
  (im-scalar-op image val /))

(define-method (im-divide-channel channel width height (val <number>))
  (im-scalar-channel-op channel width height val /))

(define-method (im-divide img-1 (img-2 <list>))
  (im-matrix-multdiv-op img-1 img-2 im-divide-channel))

(define-method (im-divide-channel c1 width-1 height-1 (c2 <uvec>) width-2)
  (f32vector-matrix-multiply c1 width-1 height-1 (f32vector-inverse c2) width-2))


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
	(if (and (apply = (cons width (im-collect rest 'width)))
		 (apply = (cons height (im-collect rest 'height))))
	    (let ((img-2 (im-copy image))
		  (n-cell (* width height))
		  (c-channels (im-collect (map im-rgb->grey rest) 'grey)))
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

(define (im-min image)
    (match image
      ((width height n-chan idata)
       (let ((map-proc (if (and (> n-chan 1)
                                (%use-par-map)) par-map map)))
         (map-proc
          (lambda (channel)
            (f32vector-min channel))
          idata)))))

(define (im-max image)
    (match image
      ((width height n-chan idata)
       (let ((map-proc (if (and (> n-chan 1)
                                (%use-par-map)) par-map map)))
         (map-proc
          (lambda (channel)
            (f32vector-max channel))
          idata)))))

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

(define (im-inverse image)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-inverse-channel channel width height))
                       idata))))))

(define (im-inverse-channel channel width height)
  (f32vector-inverse channel))

(define (im-normalize image)
  (match image
    ((width height n-chan idata)
     (list height width n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
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

(define* (im-scrap image val #:key (pred <) (con 8) (bg 'dark))
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
                (let* ((properties (im-properties image l-image #:n-label n-label))
                       (n-property (length properties))
                       (to-scrap (fold (lambda (property i prev)
                                         (match property
                                           ((area . rest)
                                            (if (pred area val)
                                                (cons i prev)
                                                prev))))
                                       '()
                                       properties
                                       (iota n-property))))
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

(define* (im-particles image properties #:key (clean #t))
  (let ((map-proc (if (%use-par-map) par-map map)))
    (map-proc (lambda (prop)
                (match prop
                  ((area left top right bottom . rest)
                   (parameterize ((%use-par-map #f))
                     (let ((particle (im-crop image left top (+ right 1) (+ bottom 1))))
                       (if clean
                           (im-particle-clean particle)
                           particle))))))
              (cdr properties))))

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
                                               (im-properties binary label-im
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


;;;
;;; Histograms
;;;

(define %h-width 256)
(define %h-height-grey 130)
(define %h-height-rgb 80)
(define %h-padd 11)
(define %h-padd-colour '(255 255 255))

(define %hl-height 14)
(define %hl-padd 2)

(define (im-histogram image)
  (case (im-n-channel image)
    ((1) (im-histogram-grey image))
    ((3) (im-histogram-rgb image))
    (else
     (error "Not a GREY not an RGB image."))))

(define (im-histogram-grey image)
  (match image
    ((width height n-chan idata)
     (match idata
       ((channel)
        (let ((histogram (im-padd (list %h-width %h-height-grey n-chan
                                        (list (im-histogram-channel channel width height
                                                                    %h-height-grey)))
                                  0 0 0 %hl-padd #:colour %h-padd-colour))
              (legend (make-histogram-legend 'grey)))
          (im-padd (im-glue histogram legend 'below 'left)
                   %hl-padd 13 %hl-padd %hl-padd #:colour %h-padd-colour)))))))

(define (im-histogram-rgb image)
  (match image
    ((width height n-chan idata)
     (let* ((h-padd %h-padd) ;; pixels
            (h-padd-colour %h-padd-colour)
            (hl-padd %hl-padd)
            (map-proc (if (%use-par-map) par-map map))
            (h-channels
             (map-proc (lambda (channel)
                         (im-histogram-channel channel width height %h-height-rgb))
                 idata)))
       (match (map-proc (lambda (h-channel-c-type)
                          (match h-channel-c-type
                            ((h-channel c-type)
                             (case c-type
                               ((red)
                                (im-padd (im-histogram-rgb-with-legend h-channel c-type)
                                         0 h-padd 0 h-padd #:colour h-padd-colour))
                               ((green)
                                (im-padd (im-histogram-rgb-with-legend h-channel c-type)
                                         0 0 0 h-padd #:colour h-padd-colour))
                               ((blue)
                                (im-histogram-rgb-with-legend h-channel c-type))))))
                  (zip h-channels '(red green blue)))
         ((hr hg hb)
          (im-padd (im-glue hr (im-glue hg hb 'below 'left) 'below 'left)
                   hl-padd hl-padd hl-padd hl-padd #:colour h-padd-colour)))))))

(define (im-histogram-rgb-with-legend h-channel c-type)
  (let* ((width %h-width)
         (height %h-height-rgb)
         (h-padd-colour %h-padd-colour)
         (idata (list h-channel
                      (im-copy-channel h-channel width height)
                      (im-copy-channel h-channel width height)))
        (legend (make-histogram-legend c-type)))
    (im-glue (im-padd (list width height 3 idata) 0 0 0 %hl-padd #:colour h-padd-colour)
             legend 'below 'left)))

#!
(do ((i 0
        (+ i 1)))
    ((= i n-grey))
  (f32vector-set! h-vals i
                  (/ (f32vector-ref h-vals i) n-cell)))
!#

(define (im-histogram-channel channel width height hi-height)
  (let* ((n-grey 256)
         (n-cell (* width height))
         (h-vals (make-f32vector n-grey 0.0)))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell))
      (let ((val #;(inexact->exact (f32vector-ref channel i))
             (float->int (f32vector-ref channel i))))
        (f32vector-set! h-vals val
                        (+ (f32vector-ref h-vals val) 1))))
    (let ((c-min (f32vector-min channel))
          (c-max (f32vector-max channel))
          (c-mean (float-round (f32vector-mean channel #:n-cell n-cell) 3))
          (c-std-dev (float-round (f32vector-std-dev channel #:n-cell n-cell) 3)))
      (receive (maxi mode)
          (f32vector-max h-vals)
        (values (make-histogram-channel h-vals hi-height)
                n-cell
                c-min
                c-max
                c-mean
                c-std-dev
                h-vals
                mode
                maxi)))))

(define (make-histogram-channel h-vals hi-height)
  (let* ((hi-width %h-width)
         (h-max (f32vector-max h-vals))
         (hi-max (- hi-height 1))
         (factor (/ hi-max h-max))
         (hi-chan (im-make-channel hi-width hi-height 255.0)))
    (do ((k 0
            (+ k 1)))
        ((= k 256))
      (let* ((h-val (f32vector-ref h-vals k))
             (hi-val (float->int (* h-val factor)))
             (start #;(if (= hi-val 0) hi-height (- hi-max hi-val))
              (- hi-height hi-val)))
        (do ((i start
                (+ i 1)))
            ((= i hi-height))
          (f32vector-set! hi-chan (+ (* i hi-width) k) 0.0))))
    hi-chan))

(define* (make-histogram-legend #:optional (type 'grey))
  (let* ((hl-width %h-width)
         (hl-height %hl-height)
         (hl-chan (im-make-channel hl-width hl-height)))
    (do ((k 0
            (+ k 1)))
        ((= k 256))
      (do ((i 0
              (+ i 1)))
          ((= i hl-height))
        (f32vector-set! hl-chan (+ (* i hl-width) k) k)))
    (case type
      ((grey)
       (list 256 14 1 (list hl-chan)))
      (else
       (let ((ec1 (im-make-channel hl-width hl-height))
             (ec2 (im-make-channel hl-width hl-height)))
         (list 256 14 3
               (case type
                 ((red) (list hl-chan ec1 ec2))
                 ((green) (list ec1 hl-chan ec2))
                 ((blue) (list ec1 ec2 hl-chan)))))))))


;;;
;;; Glue
;;;

(define (im-glue img-1 img-2 position alignment)
  (match img-1
    ((width-1 height-1 n-chan-1 idata-1)
     (match img-2
       ((width-2 height-2 n-chan-2 idata-2)
        (if (= n-chan-1 n-chan-2)
            (case position
              ((above)
               (im-glue-below img-2 img-1 alignment))
              ((below)
               (im-glue-below img-1 img-2 alignment)))
            (error "Channel number mismatch: " n-chan-1 n-chan-2)))))))

(define (im-glue-above img-1 img-2 alignment)
  (im-glue-below img-2 img-1 alignment))

(define (im-glue-below img-1 img-2 alignment)
  (match img-1
    ((width-1 height-1 n-chan-1 idata-1)
     (match img-2
       ((width-2 height-2 n-chan-2 idata-2)
        (list width-1 (+ height-1 height-2) n-chan-1
              (let ((map-proc (if (and (> n-chan-1 1)
                                       (%use-par-map)) par-map map)))
                (map-proc (lambda (channels)
                            (match channels
                              ((c1 c2)
                               (im-glue-below-channel c1 width-1 height-1
                                                      c2 width-2 height-2))))
                    (zip idata-1 idata-2)))))))))

(define (im-glue-above-channel c1 width-1 height-1
                               c2 width-2 height-2)
  (im-glue-below-channel c2 width-2 height-2 c1 width-1 height-1))

(define (im-glue-below-channel c1 width-1 height-1
                               c2 width-2 height-2)
  (let ((n-cell-1 (* width-1 height-1))
        (n-cell-2 (* width-2 height-2))
        (to (im-make-channel width-1 (+ height-1 height-2))))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell-1))
      (f32vector-set! to i (f32vector-ref c1 i)))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell-2))
      (f32vector-set! to (+ i n-cell-1) (f32vector-ref c2 i)))
    to))

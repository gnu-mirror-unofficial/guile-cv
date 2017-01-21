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
	    im-threshold
	    im-and
	    im-or
	    im-complement
	    im-min
	    im-max))


#;(g-export )

;;;
;;; Guile-CV additional API
;;;

;;(define (im-map 

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


(define* (im-threshold image threshold #:key (bg 'dark) (prec 1.0e-4))
  (if (and (>= threshold 0.0)
	   (<= threshold 255.0))
      (match (if (im-grey? image)
		 image
		 (im-rgb->grey image))
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

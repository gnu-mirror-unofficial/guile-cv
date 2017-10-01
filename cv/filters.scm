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


(define-module (cv filters)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-gaussian-blur
	    im-gaussian-blur-channel
	    im-gaussian-gradient
	    im-gaussian-gradient-channel
            im-gaussian-sharp
	    im-gaussian-sharp-channel
            im-sharpen
            im-sharpen-channel
	    im-convolve
            im-convolve-channel))


#;(g-export )


;;;
;;; Guile-CV API
;;;

(define (im-gaussian-blur image sigma)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-gaussian-blur-channel channel width height sigma))
		       idata))))))

(define (im-gaussian-blur-channel channel width height sigma)
  (let ((to (im-make-channel width height)))
    (case (vigra-gaussian-smoothing channel to width height sigma)
      ((0) to)
      (else
       (error "Gaussian blur failed.")))))

(define (im-gaussian-gradient image sigma)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-gaussian-gradient-channel channel width height sigma))
		       idata))))))

(define (im-gaussian-gradient-channel channel width height sigma)
  (let ((to (im-make-channel width height)))
    (case (vigra-gaussian-gradient channel to width height sigma)
      ((0) to)
      (else
       (error "Gaussian gradient failed.")))))

(define (im-gaussian-sharp image factor scale)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-gaussian-sharp-channel channel width height factor scale))
		       idata))))))

(define (im-gaussian-sharp-channel channel width height factor scale)
  (let ((to (im-make-channel width height)))
    (case (vigra-gaussian-sharpening channel to width height factor scale)
      ((0) to)
      (else
       (error "Gaussian sharp failed.")))))

(define (im-sharpen image factor)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-sharpen-channel channel width height factor))
		       idata))))))

(define (im-sharpen-channel channel width height factor)
  (let ((to (im-make-channel width height)))
    (case (vigra-simple-sharpening channel to width height factor)
      ((0) to)
      (else
       (error "Sharpen failed.")))))

(define (convolve-obs->int obs)
  (case obs
    ((avoid) 0)
    ((clip) 1)
    ((repeat) 2)
    ;; vigra says 'reflect, I prefer 'mirror
    ((mirror) 3)
    ((wrap) 4)
    ((zero) 5)
    (else
     (error "Unkown out-of-bound stratgy: " obs))))

(define* (im-convolve image kernel #:key (obs 'repeat))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
             (match kernel
               ((k-width k-height kernel)
                (map-proc (lambda (channel)
                            (im-convolve-channel channel width height
                                                 kernel k-width k-height
                                                 #:obs obs))
                    idata))))))))

(define* (im-convolve-channel channel width height kernel k-width k-height
                              #:key (obs 'repeat))
  (let ((obs (convolve-obs->int obs))
        (to (im-make-channel width height)))
    (case (vigra-convolve-channel channel to width height
                                  kernel k-width k-height obs)
      ((0) to)
      ((1)
       (error "Convolve failed."))
      ((2)
       (error "Kernel dimensions must be odd."))
      ((3)
       (error "Invalid out-of-bound strategy.")))))


;;;
;;; Guile vigra low level API
;;;

(define (vigra-gaussian-smoothing from to width height sigma)
  (vigra-gaussian-smoothing-c (bytevector->pointer from)
			      (bytevector->pointer to)
			      width
			      height
			      sigma))

(define (vigra-gaussian-gradient from to width height sigma)
  (vigra-gaussian-gradient-c (bytevector->pointer from)
			      (bytevector->pointer to)
			      width
			      height
			      sigma))

(define (vigra-gaussian-sharpening from to width height factor scale)
  (vigra-gaussian-sharpening-c (bytevector->pointer from)
                               (bytevector->pointer to)
                               width
                               height
                               factor
                               scale))

(define (vigra-simple-sharpening from to width height factor)
  (vigra-simple-sharpening-c (bytevector->pointer from)
                               (bytevector->pointer to)
                               width
                               height
                               factor))

(define (vigra-convolve-channel from to width height kernel k-width k-height obs)
  (vigra-convolve-channel-c (bytevector->pointer from)
                            (bytevector->pointer kernel)
                            (bytevector->pointer to)
                            width
                            height
                            k-width
                            k-height
                            obs))


;;;
;;; Vigra_c bindings
;;;

(define vigra-gaussian-smoothing-c
  (pointer->procedure int
		      (dynamic-func "vigra_gaussiansmoothing_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
			    float))) ;; sigma

(define vigra-gaussian-gradient-c
  (pointer->procedure int
		      (dynamic-func "vigra_gaussiangradientmagnitude_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
			    float))) ;; sigma

(define vigra-gaussian-sharpening-c
  (pointer->procedure int
		      (dynamic-func "vigra_gaussiansharpening_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
                            float    ;; factor
			    float))) ;; scale

(define vigra-simple-sharpening-c
  (pointer->procedure int
		      (dynamic-func "vigra_simplesharpening_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
                            float))) ;; factor

(define vigra-convolve-channel-c
  (pointer->procedure int
		      (dynamic-func "vigra_convolveimage_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
                            '*       ;; kernel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
                            int	     ;; kernel width
			    int      ;; kernel height
                            int)))   ;; border treatment

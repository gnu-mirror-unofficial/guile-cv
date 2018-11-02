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


(define-module (cv imgproc)
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

  #:export (im-resize
	    im-resize-channel
	    im-rotate
	    im-rotate-channel
	    im-flip
	    im-flip-channel
	    im-crop-size
	    im-crop
	    im-crop-channel
	    im-padd-size
	    im-padd
	    im-padd-channel
	    im-unpadd-size
	    im-unpadd
	    im-unpadd-channel
            im-clip
            im-clip-channel
            im-local-minima
            im-local-minima-channel
            im-local-maxima
            im-local-maxima-channel))


#;(g-export )


;;;
;;; Guile-CV API
;;;

(define* (im-resize image new-w new-h
		    #:key (i-mode 'bilinear))
  (match image
    ((width height n-chan idata)
     (list new-w new-h n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-resize-channel channel width height new-w new-h
					    #:i-mode i-mode))
	       idata))))))

(define* (im-resize-channel channel width height new-w new-h
			    #:key (i-mode 'bilinear))
  (let ((to (im-make-channel new-w new-h)))
    (case (vigra-resize-channel channel to width height new-w new-h
				(resize-mode->number i-mode))
      ((0) to)
      (else
       (error "Resize failed.")))))

(define (resize-mode->number i-mode)
  (case i-mode
    ((none) 0)
    ((bilin bilinear) 1)
    ((biquad biquadratic) 2)
    ((bicub bicubic) 3)
    ((trilin trilinear) 4)
    (else
     (error "No such resize interpolation mode: " i-mode))))

(define* (im-rotate image angle
		    #:key (i-mode 'bilinear))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-rotate-channel channel width height angle
					    #:i-mode i-mode))
	       idata))))))

(define* (im-rotate-channel channel width height angle
			    #:key (i-mode 'bilinear))
  (let ((to (im-make-channel width height)))
    (case (vigra-rotate-channel channel to width height angle
				(rotate-mode->number i-mode))
      ((0) to)
      (else
       (error "Rotation failed.")))))

(define (rotate-mode->number i-mode)
  (case i-mode
    ((bilin bilinear) 1)
    ((biquad biquadratic) 2)
    ((bicub bicubic) 3)
    ((trilin trilinear) 4)
    (else
     (error "No such rotate interpolation mode: " i-mode))))

(define (im-flip image plane)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-flip-channel channel width height plane))
	       idata))))))

(define (im-flip-channel channel width height plane)
  (let ((to (im-make-channel width height)))
    (case (vigra-flip-channel channel to width height
			      (flip-plane->axis plane))
      ((0) to)
      (else
       (error "Flip failed.")))))

(define (flip-plane->axis plane)
  (case plane
    ;; vigra uses axis, not plane, so:
    ;;   horizontal plane -> vigra vertical axis, which is the integer 2
    ;;   vertical plane -> vigra horizontal axis, which is the integer 1
    ((hori horizontal) 2)
    ((vert vertical) 1)
    ((both) 3)
    (else
     (error "No such flip plane: " plane))))

(define (im-crop-size width height left top right bottom)
  (let ((new-w (- right left))
	(new-h (- bottom top)))
    (if (and (< left right)
	     (< top bottom)
	     (<= new-w width)
	     (<= new-h height))
	(list new-w new-h)
	(error "Invalid crop indices:" left top right bottom))))

(define (im-crop image left top right bottom)
  (match image
    ((width height n-chan idata)
     (match (im-crop-size width height left top right bottom)
       ((new-w new-h)
	(list new-w new-h n-chan
	      (let ((map-proc (if (and (> n-chan 1)
                                       (%use-par-map)) par-map map)))
		(map-proc (lambda (channel)
			    (im-crop-channel channel width height left top right bottom
					     #:new-w new-w #:new-h new-h))
			  idata))))))))

(define* (im-crop-channel channel width height left top right bottom
			  #:key (new-w #f) (new-h #f))
  (let ((to (if (and new-w new-h)
		(im-make-channel new-w new-h)
		(match (im-crop-size width height left top right bottom)
		  ((new-w new-h)
		   (im-make-channel new-w new-h))))))
    (case (vigra-crop-channel channel to width height left top right bottom)
      ((0) to)
      (else
       (error "Crop failed.")))))

(define (im-padd-size width height left top right bottom)
  (if (and-l (map (lambda (padd) (>= padd 0))
	       (list left top right bottom)))
      (list (+ left width right)
	    (+ top height bottom))
      (error "Invalid padd value(s): " left top right bottom)))

(define* (im-padd image left top right bottom #:key (color '(0.0 0.0 0.0)))
  (match image
    ((width height n-chan idata)
     (match (im-padd-size width height left top right bottom)
       ((new-w new-h)
	(list new-w new-h n-chan
              (match idata
                ((c)
                 (list (im-padd-channel c width height left top right bottom
                                        #:new-w new-w #:new-h new-h
                                        #:value (/ (reduce + 0 color) 3))))
                ((r g b)
                 (let ((map-proc (if (%use-par-map) par-map map)))
                   (map-proc (lambda (chaco)
                               (match chaco
                                 ((channel value)
                               (im-padd-channel channel width height left top right bottom
                                                #:new-w new-w #:new-h new-h #:value value))))
			  (zip idata color)))))))))))

(define* (im-padd-channel channel width height left top right bottom
			  #:key (new-w #f) (new-h #f) (value 0.0))
  (let ((to (if (and new-w new-h)
		(im-make-channel new-w new-h value)
		(match (im-padd-size width height left top right bottom)
		  ((new-w new-h)
		   (im-make-channel new-w new-h value))))))
    (case (vigra-padd-channel channel to width height left top right bottom)
      ((0) to)
      (else
       (error "Padd failed.")))))

(define (im-unpadd-size width height left top right bottom)
  (if (and-l (map (lambda (padd) (>= padd 0.0))
	       (list left top right bottom)))
      (list (- width left right)
	    (- height top bottom))
      (error "Invalid unpadd value(s): " left top right bottom)))

(define (im-unpadd image left top right bottom)
  (match image
    ((width height n-chan idata)
     (match (im-unpadd-size width height left top right bottom)
       ((new-w new-h)
	(list new-w new-h n-chan
	      (let ((map-proc (if (and (> n-chan 1)
                                       (%use-par-map)) par-map map)))
		(map-proc (lambda (channel)
			    (im-unpadd-channel channel width height left top right bottom
					       #:new-w new-w #:new-h new-h))
			  idata))))))))

(define* (im-unpadd-channel channel width height left top right bottom
			    #:key (new-w #f) (new-h #f))
  (let ((to (if (and new-w new-h)
		(im-make-channel new-w new-h)
		(match (im-unpadd-size width height left top right bottom)
		  ((new-w new-h)
		   (im-make-channel new-w new-h))))))
    (case (vigra-crop-channel channel to
			      width height left top (+ left new-w) (+ top new-h))
      ((0) to)
      (else
       (error "Unpadd failed.")))))

(define* (im-clip image #:key (lower 0.0) (upper 255.0))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-clip-channel channel width height #:lower lower #:upper upper))
	       idata))))))

(define* (im-clip-channel channel width height #:key (lower 0.0) (upper 255.0))
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (case (vigra-clip-channel channel to width height lower upper)
      ((0) to)
      (else
        (error "Clip failed.")))))

(define* (im-local-minima image
                          #:key (con 8)
                          (marker 1.0)
                          (threshold +float-max+)
                          (borders? #f)
                          (plateaus? #f)
                          (epsilon 1.0e-4))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-local-minima-channel channel width height
                                                  #:con con
                                                  #:marker marker
                                                  #:threshold threshold
                                                  #:borders? borders?
                                                  #:plateaus? plateaus?
                                                  #:epsilon epsilon))
	       idata))))))

(define* (im-local-minima-channel channel width height
                                  #:key (con 8)
                                  (marker 1.0)
                                  (threshold +float-max+)
                                  (borders? #f)
                                  (plateaus? #f)
                                  (epsilon 1.0e-4))
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (case (vigra-local-minima channel to width height con marker threshold
                              borders? plateaus? epsilon)
      ((0) to)
      (else
        (error "Local minima failed.")))))

(define* (im-local-maxima image
                          #:key (con 8)
                          (marker 1.0)
                          (threshold (- +float-max+))
                          (borders? #f)
                          (plateaus? #f)
                          (epsilon 1.0e-4))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-local-maxima-channel channel width height
                                                  #:con con
                                                  #:marker marker
                                                  #:threshold threshold
                                                  #:borders? borders?
                                                  #:plateaus? plateaus?
                                                  #:epsilon epsilon))

	       idata))))))

(define* (im-local-maxima-channel channel width height
                                  #:key (con 8)
                                  (marker 1.0)
                                  (threshold (- +float-max+))
                                  (borders? #f)
                                  (plateaus? #f)
                                  (epsilon 1.0e-4))
  (let ((to (im-make-channel width height))
        (n-cell (* width height)))
    (case (vigra-local-maxima channel to width height con marker threshold
                              borders? plateaus? epsilon)
      ((0) to)
      (else
        (error "Local maxima failed.")))))


;;;
;;; Guile vigra low level API
;;;

(define (vigra-resize-channel from to width height new-w new-h i-mode)
  (vigra-resize-channel-c (bytevector->pointer from)
			  (bytevector->pointer to)
			  width
			  height
			  new-w
			  new-h
			  i-mode))

(define (vigra-rotate-channel from to width height angle i-mode)
  (vigra-rotate-channel-c (bytevector->pointer from)
			  (bytevector->pointer to)
			  width
			  height
			  angle
			  i-mode))

(define (vigra-flip-channel from to width height axis)
  (vigra-flip-channel-c (bytevector->pointer from)
			(bytevector->pointer to)
			width
			height
			axis))

(define (vigra-crop-channel from to width height left top right bottom)
  (vigra-crop-channel-c (bytevector->pointer from)
			(bytevector->pointer to)
			width
			height
			left
			top
			right
			bottom))

(define (vigra-padd-channel from to width height left top right bottom)
  (vigra-padd-channel-c (bytevector->pointer from)
			(bytevector->pointer to)
			width
			height
			left
			top
			right
			bottom))

(define (vigra-clip-channel from to width height lower upper)
  (vigra-clip-channel-c (bytevector->pointer from)
			(bytevector->pointer to)
			width
			height
			lower
			upper))

(define (vigra-local-minima from to width height con marker threshold
                            borders? plateaus? epsilon)
  (vigra-local-minima-c (bytevector->pointer from)
                        (bytevector->pointer to)
                        width
                        height
                        (case con
                          ((8) 1)
                          ((4) 0)
                          (else
                           (error "No such connectivity: " con)))
                        marker
                        threshold
                        (if borders? 1 0)
                        (if plateaus? 1 0)
                        epsilon))

(define (vigra-local-maxima from to width height con marker threshold
                            borders? plateaus? epsilon)
  (vigra-local-maxima-c (bytevector->pointer from)
                        (bytevector->pointer to)
                        width
                        height
                        (case con
                          ((8) 1)
                          ((4) 0)
                          (else
                           (error "No such connectivity: " con)))
                        marker
                        threshold
                        (if borders? 1 0)
                        (if plateaus? 1 0)
                        epsilon))


;;;
;;; Vigra_c bindings
;;;

(define vigra-resize-channel-c
  (pointer->procedure int
		      (dynamic-func "vigra_resizeimage_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int		;; width
			    int		;; height
			    int		;; new width
			    int		;; new height
			    int)))	;; interpolation mode

(define vigra-rotate-channel-c
  (pointer->procedure int
		      (dynamic-func "vigra_rotateimage_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int		;; width
			    int		;; height
			    float	;; angle
			    int)))	;; interpolation mode

(define vigra-flip-channel-c
  (pointer->procedure int
		      (dynamic-func "vigra_reflectimage_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int		;; width
			    int		;; height
			    int)))	;; flip axis

(define vigra-crop-channel-c
  (pointer->procedure int
		      (dynamic-func "vigra_subimage_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int		;; from width
			    int		;; from height
			    int		;; left
			    int		;; top
			    int		;; right
			    int)))	;; bottom

(define vigra-padd-channel-c
  (pointer->procedure int
		      (dynamic-func "vigra_paddimage_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int		;; from width
			    int		;; from height
			    int		;; left
			    int		;; top
			    int		;; right
			    int)))	;; bottom

(define vigra-clip-channel-c
  (pointer->procedure int
		      (dynamic-func "vigra_clipimage_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int		;; from width
			    int		;; from height
			    float	;; lower
			    float)))	;; upper

(define vigra-local-minima-c
    (pointer->procedure int
		      (dynamic-func "vigra_localminima_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int		;; from width
			    int		;; from height
                            int		;; 8-con?
                            float	;; marker
                            float	;; threshold
                            int		;; borders?
                            int		;; plateaus?
                            float)))	;; epsilon

(define vigra-local-maxima-c
    (pointer->procedure int
		      (dynamic-func "vigra_localmaxima_c"
				    %libvigra-c)
		      (list '*		;; from channel
			    '*		;; to channel
			    int		;; from width
			    int		;; from height
                            int		;; 8-con?
                            float	;; marker
                            float	;; threshold
                            int		;; borders?
                            int		;; plateaus?
                            float)))	;; epsilon

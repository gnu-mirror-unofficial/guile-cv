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


(define-module (cv morphology)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)
  #:use-module (cv imgproc)
  #:use-module (cv segmentation)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-disc-erode
	    im-disc-erode-channel
	    im-disc-dilate
	    im-disc-dilate-channel
	    im-open
	    im-open-channel
	    im-close
	    im-close-channel
	    im-fill
	    im-fill-channel))


#;(g-export )


;;;
;;; Guile-CV API
;;;

(define (im-disc-erode image radius)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
	   (let ((map-proc (if (> n-chan 1) par-map map)))
	     (map-proc (lambda (channel)
			 (im-disc-erode-channel channel width height radius))
		       idata))))))

(define (im-disc-erode-channel channel width height radius)
  (let ((to (im-make-channel width height)))
    (case (vigra-disc-erode channel to width height radius)
      ((0) to)
      (else
       (error "Disc erode failed.")))))

(define (im-disc-dilate image radius)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
	   (let ((map-proc (if (> n-chan 1) par-map map)))
	     (map-proc (lambda (channel)
			 (im-disc-dilate-channel channel width height radius))
		       idata))))))

(define (im-disc-dilate-channel channel width height radius)
  (let ((to (im-make-channel width height)))
    (case (vigra-disc-dilate channel to width height radius)
      ((0) to)
      (else
       (error "Disc dilate failed.")))))

(define (im-open image radius)
  (im-disc-dilate (im-disc-erode image radius) radius))

(define (im-open-channel channel width height radius)
  (im-disc-dilate-channel (im-disc-erode-channel channel width height radius)
			  width height radius))

(define (im-close image radius)
  (im-disc-erode (im-disc-dilate image radius) radius))

(define (im-close-channel channel width height radius)
  (im-disc-erode-channel (im-disc-dilate-channel channel width height radius)
			 width height radius))

(define* (im-fill image #:key (con 8))
  ;; (im-binary? image) is rather expensive
  (match image
    ((width height n-chan idata)
     ;; so we only check for n-chan
     (case n-chan
       ((1)
	(list width height n-chan
	      (map (lambda (channel)
		     (im-fill-channel channel width height #:con con))
		idata)))
       (else
	(error "Not a binary image."))))))

(define* (im-fill-channel channel width height #:key (con 8))
  (let* ((new-w (+ width 2))
	 (new-h (+ height 2))
	 (p-channel (im-padd-channel channel width height 1 1 1 1
				     #:new-w new-w #:new-h new-h))
	 (l-channel (im-label-all-channel p-channel new-w new-h #:con con))
	 (bg-label (f32vector-ref l-channel 0)))
    (do ((i 0
	    (+ i 1)))
	((= i (* new-w new-h)))
      (if (float=? (f32vector-ref l-channel i) bg-label)
	  (f32vector-set! l-channel i 0.0)
	  (f32vector-set! l-channel i 255.0)))
    (im-unpadd-channel l-channel new-w new-h 1 1 1 1
		       #:new-w width #:new-h height)))


;;;
;;; Guile vigra low level API
;;;

(define (vigra-disc-erode from to width height radius)
  (vigra-disc-erode-c (bytevector->pointer from)
		 (bytevector->pointer to)
		 width
		 height
		 radius))

(define (vigra-disc-dilate from to width height radius)
  (vigra-disc-dilate-c (bytevector->pointer from)
		       (bytevector->pointer to)
		       width
		       height
		       radius))


;;;
;;; Vigra_c bindings
;;;

(define vigra-disc-erode-c
  (pointer->procedure int
		      (dynamic-func "vigra_discerosion_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int      ;; height
			    int)))   ;; radius

(define vigra-disc-dilate-c
  (pointer->procedure int
		      (dynamic-func "vigra_discdilation_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int      ;; height
			    int)))   ;; radius

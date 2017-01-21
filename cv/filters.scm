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
	    im-gaussian-gradient-channel))


#;(g-export )


;;;
;;; Guile-CV API
;;;

(define (im-gaussian-blur image sigma)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
	   (let ((map-proc (if (> n-chan 1) par-map map)))
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
	   (let ((map-proc (if (> n-chan 1) par-map map)))
	     (map-proc (lambda (channel)
			 (im-gaussian-gradient-channel channel width height sigma))
		       idata))))))

(define (im-gaussian-gradient-channel channel width height sigma)
  (let ((to (im-make-channel width height)))
    (case (vigra-gaussian-gradient channel to width height sigma)
      ((0) to)
      (else
       (error "Gradient failed.")))))


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

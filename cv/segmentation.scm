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


(define-module (cv segmentation)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-label
	    im-label-channel
	    im-label-all
	    im-label-all-channel
	    im-label-properties-channel
	    #;im-watershed
	    #;im-watershed-channel))


#;(g-export )


;;;
;;; Guile-CV API
;;;

(define* (im-label image #:key (con 8) (bg 'dark))
  ;; (im-binary? image) is rather expensive
  (match image
    ((width height n-chan idata)
     ;; so we only check for n-chan
     (case n-chan
       ((1)
	(match idata
	  ((c)
	   (receive (l-channel n-label)
	       (im-label-channel c width height #:con con #:bg bg)
	     (values (list width height 1 (list l-channel))
		     n-label)))))
       (else
	(error "Not a binary image."))))))

(define* (im-label-channel channel width height #:key (con 8) (bg 'dark))
  (let* ((to (im-make-channel width height))
	 (result (vigra-label channel to width height con bg)))
    (case result
      ((-1)
       (error "Label failed."))
      (else
       (values to result)))))

(define* (im-label-all image #:key (con 8))
  ;; (im-binary? image) is rather expensive
  (match image
    ((width height n-chan idata)
     ;; so we only check for n-chan
     (case n-chan
       ((1)
	(match idata
	  ((c)
	   (receive (l-channel n-label)
	       (im-label-all-channel c width height #:con con)
	     (values (list width height 1 (list l-channel))
		     n-label)))))
	       (else
	(error "Not a binary image."))))))

(define* (im-label-all-channel channel width height #:key (con 8))
  (let* ((to (im-make-channel width height))
	 (result (vigra-label-all channel to width height con)))
    (case result
      ((-1)
       (error "Label failed."))
      (else
       (values to result)))))

#;(define (im-watershed image)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
	   (let ((map-proc (if (> n-chan 1) par-map map)))
	     (map-proc (lambda (channel)
			 (im-watershed-channel channel width height))
		       idata))))))

#;(define (im-watershed-channel channel width height)
  (let ((to (im-make-channel width height)))
    (case (vigra-watershed channel to width height)
      ((-1)
       (error "Watershed failed."))
      (else
       to))))

(define* (im-label-properties-channel g-channel l-channel width height
				      #:key (n-label #f))
  (let* ((n-label (or n-label
		      (inexact->exact (f32vector-max l-channel))))
	 (properties (im-make-channel 11 (+ n-label 1)))
	 (result (vigra-label-properties g-channel l-channel
					 properties width height n-label)))
    (case result
      ((1)
       (error "Label properties failed."))
      (else
       properties))))


;;;
;;; Guile vigra low level API
;;;

(define (vigra-label from to width height con bg)
  (vigra-label-c (bytevector->pointer from)
		 (bytevector->pointer to)
		 width
		 height
		 (case con
		   ((8) 1)
		   ((4) 0)
		   (else
		    (error "No such connectivity: " con)))
		 (case bg
		   ((dark) 0.0)
		   ((light) 255.0)
		   (else
		    (error "No such background: " bg)))))

(define (vigra-label-all from to width height con)
  (vigra-label-all-c (bytevector->pointer from)
		     (bytevector->pointer to)
		     width
		     height
		     (case con
		       ((8) 1)
		       ((4) 0)
		       (else
			(error "No such connectivity: " con)))))

#;(define (vigra-watershed from to width height)
  (vigra-watershed-c (bytevector->pointer from)
		     (bytevector->pointer to)
		     width
		     height))

(define (vigra-label-properties from labels results width height n-label)
  (vigra-label-properties-c (bytevector->pointer from)
			    (bytevector->pointer labels)
			    (bytevector->pointer results)
			    width
			    height
			    n-label))


;;;
;;; Vigra_c bindings
;;;

(define vigra-label-c
  (pointer->procedure int
		      (dynamic-func "vigra_labelimagewithbackground_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
			    int	     ;; 8-con?
			    float))) ;; background

(define vigra-label-all-c
  (pointer->procedure int
		      (dynamic-func "vigra_labelimage_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
			    int)))   ;; 8-con?

#;(define vigra-watershed-c
  (pointer->procedure int
		      (dynamic-func "vigra_watersheds_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int)))   ;; height

(define vigra-label-properties-c
  (pointer->procedure int
		      (dynamic-func "vigra_extractfeatures_gray_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; labels channel
			    '*	     ;; results vector
			    int	     ;; width
			    int	     ;; height
			    int)))   ;; n-label

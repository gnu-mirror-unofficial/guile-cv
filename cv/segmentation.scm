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
	    #;im-watershed
	    #;im-watershed-channel
            im-canny
            im-canny-channel))


#;(g-export )


;;;
;;; Guile-CV API
;;;

(define* (im-label image #:key (con 8) (bg 'black))
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

(define* (im-label-channel channel width height #:key (con 8) (bg 'black))
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
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
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

(define* (im-canny image
                   #:key (sigma 1.0) (threshold 0.0) (marker 255.0))
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
	     (map-proc (lambda (channel)
			 (im-canny-channel channel width height
                                           #:sigma sigma
                                           #:threshold threshold
                                           #:marker marker))
                 idata))))))

(define* (im-canny-channel channel width height
                           #:key (sigma 1.0) (threshold 0.0) (marker 255.0))
  (let ((to (im-make-channel width height)))
    (case (vigra-canny-edge-channel channel to width height sigma threshold marker)
      ((0) to)
      (else
       (error "Canny failed.")))))


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
		   ((black) 0.0)
		   ((white) 255.0)
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

(define (vigra-canny-edge-channel from to width height sigma threshold marker)
  (vigra-canny-edge-channel-c (bytevector->pointer from)
                              (bytevector->pointer to)
                              width
                              height
                              sigma
                              threshold
                              marker))


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

(define vigra-canny-edge-channel-c
  (pointer->procedure int
		      (dynamic-func "vigra_cannyedgeimage_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; to channel
			    int	     ;; width
			    int	     ;; height
                            float    ;; scale
                            float    ;; gradient-threshold
			    float))) ;; edge-marker

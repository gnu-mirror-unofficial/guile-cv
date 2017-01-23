;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2017
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


(define-module (cv properties)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)
  #:use-module (cv segmentation)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-properties
	    im-properties-channel))


#;(g-export )


;;;
;;; Guile-CV API
;;;

(define* (im-properties-channel channel l-channel width height
                                #:key (n-label #f))
  (let* ((n-label (or n-label
		      (inexact->exact (f32vector-max l-channel))))
	 (properties (im-make-channel 11 (+ n-label 1)))
	 (result (vigra-properties channel l-channel
                                   properties width height n-label)))
    (case result
      ((1)
       (error "Properties failed."))
      (else
       properties))))


;;;
;;; Guile vigra low level API
;;;

(define (vigra-properties from labels results width height n-label)
  (vigra-properties-c (bytevector->pointer from)
                      (bytevector->pointer labels)
                      (bytevector->pointer results)
                      width
                      height
                      n-label))


;;;
;;; Vigra_c bindings
;;;

(define vigra-properties-c
  (pointer->procedure int
		      (dynamic-func "vigra_extractfeatures_gray_c"
				    %libvigra-c)
		      (list '*	     ;; from channel
			    '*	     ;; labels channel
			    '*	     ;; results vector
			    int	     ;; width
			    int	     ;; height
			    int)))   ;; n-label

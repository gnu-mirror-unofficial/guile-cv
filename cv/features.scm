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


(define-module (cv features)
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

  #:export (im-features
	    im-features-channel))


#;(g-export )


;;;
;;; Guile-CV API
;;;

(define* (im-features image l-image #:key (n-label #f))
  (match l-image
    ((_ _ _ l-idata)
     (match l-idata
       ((l-c)
        (let ((n-label (or n-label
                           (float->int (f32vector-max l-c)))))
          (match image
            ((width height n-chan idata)
             (match idata
               ((c)
                (im-features-gray c l-c width height #:n-label n-label))
               ((r g b)
                (im-features-rgb r g b l-c width height
                                   #:n-label n-label))
               (else
                (error "Not a GRAY neither an RGB image.")))))))
       (else
        (error "Not a labeled image."))))))

(define %vigra-property-length-gray 11)

(define* (im-features-gray channel l-c width height #:key (n-label #f))
  (let* ((n-label (or n-label
		      (float->int (f32vector-max l-c))))
         (n-prop (+ n-label 1))
	 (features (im-make-channel %vigra-property-length-gray n-prop))
	 (result (vigra-extract-features-gray channel l-c
                                              features width height n-label)))
    (case result
      ((1)
       (error "Features failed."))
      (else
       (vigra-features->list features n-prop 'gray)))))

(define %vigra-property-length-rgb 19)

(define* (im-features-rgb r g b l-c width height #:key (n-label #f))
  (let* ((n-label (or n-label
		      (float->int (f32vector-max l-c))))
         (n-prop (+ n-label 1))
	 (features (im-make-channel %vigra-property-length-rgb n-prop))
	 (result (vigra-extract-features-rgb r g b l-c
                                             features width height n-label)))
    (case result
      ((1)
       (error "Features failed."))
      (else
       (vigra-features->list features n-prop 'rgb)))))

(define (vigra-features->list features n-prop im-type)
  ;;
  ;; Notes:
  ;;  - n-prop is n-label + 1 (one for the bg, tbc)
  ;;  - features is a 'channel' (f32vector), see
  ;;    below for a full description of a property
  ;;
  (do ((proc (case im-type
               ((gray) vigra-property-gray)
               ((rgb) vigra-property-rgb)))
       (result '())
       (i 0
          (+ i 1)))
      ((= i n-prop)
       (reverse! result))
      (set! result
            (cons (proc features i) result))))

(define (vigra-property-gray features i)
  ;;
  ;;  Gray object's offset and property names are:
  ;;
  ;;  | Index         | Feature                       |
  ;;  | ------------- | ----------------------------- |
  ;;  |  0            | region_size                   |
  ;;  |  1,  2        | upperleft-x and y-coord       |
  ;;  |  3,  4        | lowerright-x and y-coord      |
  ;;  |  5,  6        | mean-x and y-coord            |
  ;;  |  7            | min gray value                |
  ;;  |  8            | max gray value                |
  ;;  |  9            | mean gray value               |
  ;;  | 10            | std.dev. gray value           |
  ;;
  (let* ((p-size %vigra-property-length-gray)
         (offset (* i p-size)))
    (match (map (lambda (k)
                  (f32vector-ref features (+ offset k)))
             (iota p-size))
      ((area
        left top right bottom
        mean-x mean-y
        mini maxi meani
        std-dev)
       (list (float->int (float-round area 0))
             (float->int (float-round left 0))
             (float->int (float-round top 0))
             (float->int (float-round right 0))
             (float->int (float-round bottom 0))
             mean-x mean-y
             mini maxi meani
             std-dev)))))

(define (vigra-property-rgb features i)
  ;;
  ;;  RGB object's offset and property names are:
  ;;
  ;;  | Index         | Feature                       |
  ;;  | ------------- | ----------------------------- |
  ;;  |  0            | region_size                   |
  ;;  |  1,  2        | upperleft-x and y-coord       |
  ;;  |  3,  4        | lowerright-x and y-coord      |
  ;;  |  5,  6        | mean-x and y-coord            |
  ;;  |  7,  8,  9    | min red,green,blue value      |
  ;;  | 10, 11, 12    | max red,green,blue value      |
  ;;  | 13, 14, 15    | mean red,green,blue value     |
  ;;  | 16, 17, 18    | std.dev. red,green,blue value |
  ;;
  (let* ((p-size %vigra-property-length-rgb)
         (offset (* i p-size)))
    (match (map (lambda (k)
                  (f32vector-ref features (+ offset k)))
             (iota p-size))
      ((area
        left top right bottom
        mean-x mean-y
        min-r min-g min-b
        max-r max-g max-b
        mean-r mean-g mean-b
        std-dev-r std-dev-g std-dev-b)
       (list (float->int (float-round area 0))
             (float->int (float-round left 0))
             (float->int (float-round top 0))
             (float->int (float-round right 0))
             (float->int (float-round bottom 0))
             mean-x mean-y
             min-r min-g min-b
             max-r max-g max-b
             mean-r mean-g mean-b
             std-dev-r std-dev-g std-dev-b)))))


;;;
;;; Guile vigra low level API
;;;

(define (vigra-extract-features-gray from labels results width height n-label)
  (vigra-extractfeatures-gray-c (bytevector->pointer from)
                                (bytevector->pointer labels)
                                (bytevector->pointer results)
                                width
                                height
                                n-label))

(define (vigra-extract-features-rgb r g b labels results width height n-label)
  (vigra-extractfeatures-rgb-c (bytevector->pointer r)
                               (bytevector->pointer g)
                               (bytevector->pointer b)
                               (bytevector->pointer labels)
                               (bytevector->pointer results)
                               width
                               height
                               n-label))


;;;
;;; Vigra_c bindings
;;;

(define vigra-extractfeatures-gray-c
  (pointer->procedure int
		      (dynamic-func "vigra_extractfeatures_gray_c"
				    %libvigra-c)
		      (list '*     ;; from channel
			    '*     ;; labels channel
			    '*     ;; results vector
			    int    ;; width
			    int    ;; height
			    int))) ;; n-label

(define vigra-extractfeatures-rgb-c
  (pointer->procedure int
		      (dynamic-func "vigra_extractfeatures_rgb_c"
				    %libvigra-c)
		      (list '*     ;; red channel
                            '*     ;; green channel
                            '*     ;; blue channel
			    '*     ;; labels channel
			    '*     ;; results vector
			    int    ;; width
			    int    ;; height
			    int))) ;; n-label

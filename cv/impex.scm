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


(define-module (cv impex)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-load
	    im-save))


(g-export im-size

	  im-width
	  im-height
	  im-n-channel
	  im-channels

	  im-gray?
	  im-rgb?)


;;;
;;; Guile-CV API
;;;

(define (im-load filename)
  (match (im-size filename)
    ((width height n-chan)
     (case n-chan
       ((1)
        (vigra-load-gray-image filename width height))
       ((3)
        (vigra-load-rgb-image filename width height))
       ((4)
        (vigra-load-rgba-image filename width height))
       (else
        (error "Not a GRAY, RGB nor an RGBA image" filename))))))

(define* (im-save image filename #:optional (scale #f))
  (match image
    ((width height n-chan idata)
     (case n-chan
       ((1)
        (vigra-save-gray-image image filename width height scale))
       ((3)
        (vigra-save-rgb-image image filename width height scale))
       ((4)
        (vigra-save-rgba-image image filename width height scale))
       (else
        (error "Not a GRAY, RGB nor an RGBA image" filename))))))

(define-method (im-size (filename <string>))
  (list (im-width filename)
	(im-height filename)
	(im-n-channel filename)))

(define-method (im-width (filename <string>))
  (vigra-image-width-c (string->pointer filename)))

(define-method (im-height (filename <string>))
  (vigra-image-height-c (string->pointer filename)))

(define-method (im-n-channel (filename <string>))
  (vigra-image-numbands-c (string->pointer filename)))

(define-method (im-channels (filename <string>))
  (let ((port (current-output-port))) 
    (display "This method only works on images, not filenames."
	     port)
    (newline port)))

(define-method (im-gray? (filename <string>))
  (= (im-n-channel filename) 1))

(define-method (im-rgb? (filename <string>))
  (= (im-n-channel filename) 3))

  
;;;
;;;
;;;

(define (vigra-load-gray-image filename width height)
  (let ((c (im-make-channel width height)))
    (case (vigra-importgrayimage-c (bytevector->pointer c)
                                   width
                                   height
                                   (string->pointer filename))
      ((0) (list width height 1 (list c)))
      ((1) (error "Not a GRAY image" filename))
      ((2) (error "Sizes mismatch" filename)))))

(define (vigra-load-rgb-image filename width height)
  (let ((idata (im-make-channels width height 3)))
    (match idata
      ((r g b)
       (case (vigra-importrgbimage-c (bytevector->pointer r)
                                     (bytevector->pointer g)
                                     (bytevector->pointer b)
                                     width
                                     height
                                     (string->pointer filename))
         ((0) (list width height 3 idata))
         ((1) (error "Not an RGB image" filename))
         ((2) (error "Sizes mismatch" filename)))))))

(define (vigra-load-rgba-image filename width height)
  (let ((idata (im-make-channels width height 4)))
    (match idata
      ((r g b a)
       (case (vigra-importrgbaimage-c (bytevector->pointer r)
                                      (bytevector->pointer g)
                                      (bytevector->pointer b)
                                      (bytevector->pointer a)
                                      width
                                      height
                                      (string->pointer filename))
         ((0) (list width height 4 idata))
         ((1) (error "Load RGBA image failed." filename)))))))

(define* (vigra-save-gray-image image filename width height
                                #:optional (scale #f))
  (match (im-channels image)
    ((c)
     (case (vigra-exportgrayimage-c (bytevector->pointer c)
                                    width
                                    height
                                    (string->pointer filename)
                                    (if scale 1 0))
       ((0) #t)
       (else
        (error "Image could not be saved." filename))))))

(define* (vigra-save-rgb-image image filename width height
                               #:optional (scale #f))
  (match (im-channels image)
    ((r g b)
     (case (vigra-exportrgbimage-c (bytevector->pointer r)
                                   (bytevector->pointer g)
                                   (bytevector->pointer b)
                                   width
                                   height
                                   (string->pointer filename)
                                   (if scale 1 0))
       ((0) #t)
       (else
        (error "Image could not be saved." filename))))))

(define* (vigra-save-rgba-image image filename width height
                                #:optional (scale #f))
  (match (im-channels image)
    ((r g b a)
     (case (vigra-exportrgbaimage-c (bytevector->pointer r)
                                    (bytevector->pointer g)
                                    (bytevector->pointer b)
                                    (bytevector->pointer a)
                                    width
                                    height
                                    (string->pointer filename)
                                    (if scale 1 0))
       ((0) #t)
       (else
        (error "Image could not be saved." filename))))))


;;;
;;; Vigra_c bindings
;;;

(define vigra-image-width-c
  (pointer->procedure int
		      (dynamic-func "vigra_imagewidth_c"
				    %libvigra-c)
		      (list '*)))

(define vigra-image-height-c
  (pointer->procedure int
		      (dynamic-func "vigra_imageheight_c"
				    %libvigra-c)
		      (list '*)))

(define vigra-image-numbands-c
  (pointer->procedure int
		      (dynamic-func "vigra_imagenumbands_c"
				    %libvigra-c)
		      (list '*)))

(define vigra-importgrayimage-c
  (pointer->procedure int
		      (dynamic-func "vigra_importgrayimage_c"
				    %libvigra-c)
		      (list '* int int '*)))

(define vigra-importrgbimage-c
  (pointer->procedure int
		      (dynamic-func "vigra_importrgbimage_c"
				    %libvigra-c)
		      (list '* '* '* int int '*)))

(define vigra-importrgbaimage-c
  (pointer->procedure int
		      (dynamic-func "vigra_importrgbaimage_c"
				    %libvigra-c)
		      (list '* '* '* '* int int '*)))

(define vigra-exportgrayimage-c
  (pointer->procedure int
		      (dynamic-func "vigra_exportgrayimage_c"
				    %libvigra-c)
		      (list '* int int '* int)))

(define vigra-exportrgbimage-c
  (pointer->procedure int
		      (dynamic-func "vigra_exportrgbimage_c"
				    %libvigra-c)
		      (list '* '* '* int int '*)))

(define vigra-exportrgbaimage-c
  (pointer->procedure int
		      (dynamic-func "vigra_exportrgbaimage_c"
				    %libvigra-c)
		      (list '* '* '* '* int int '*)))

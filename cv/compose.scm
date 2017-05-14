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


(define-module (cv compose)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (search basic)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)
  
  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-compose))


;;;
;;; Compose
;;;

#;(define (im-compose position alignment . images)
  (match images
    ((image . rest)
     (match image
       ((width height n-chan _)
        (if (apply = (cons n-chan (im-collect rest 'n-channel)))
            (let ((map-proc (if (and (> n-chan 1)
                                     (%use-par-map)) par-map map)))
              (map-proc (lambda (channels)
                          (im-compose-channel width height init-val))
                  (iota n-chan)))
            (error "Channel number mismatch")))))
    ((image) image)
    (()
     (error "The list of images to compose can't be empty"))))

(define (im-compose img-1 img-2 position alignment)
  (match img-1
    ((width-1 height-1 n-chan-1 idata-1)
     (match img-2
       ((width-2 height-2 n-chan-2 idata-2)
        (if (= n-chan-1 n-chan-2)
            (case position
              ((above)
               (im-compose-below img-2 img-1 alignment))
              ((below)
               (im-compose-below img-1 img-2 alignment))
              ((left)
               (im-compose-right img-2 img-1 alignment))
              ((right)
               (im-compose-right img-1 img-2 alignment)))
            (error "Channel number mismatch: " n-chan-1 n-chan-2)))))))

(define (im-compose-above img-1 img-2 alignment)
  (im-compose-below img-2 img-1 alignment))

(define (im-compose-below img-1 img-2 alignment)
  (match img-1
    ((width-1 height-1 n-chan-1 idata-1)
     (match img-2
       ((width-2 height-2 n-chan-2 idata-2)
        (list width-1 (+ height-1 height-2) n-chan-1
              (let ((map-proc (if (and (> n-chan-1 1)
                                       (%use-par-map)) par-map map)))
                (map-proc (lambda (channels)
                            (match channels
                              ((c1 c2)
                               (im-compose-below-channel c1 width-1 height-1
                                                         c2 width-2 height-2))))
                    (zip idata-1 idata-2)))))))))

(define (im-compose-above-channel c1 width-1 height-1
                                  c2 width-2 height-2)
  (im-compose-below-channel c2 width-2 height-2 c1 width-1 height-1))

(define (im-compose-below-channel c1 width-1 height-1
                                  c2 width-2 height-2)
  (let ((n-cell-1 (* width-1 height-1))
        (n-cell-2 (* width-2 height-2))
        (to (im-make-channel width-1 (+ height-1 height-2))))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell-1))
      (f32vector-set! to i (f32vector-ref c1 i)))
    (do ((i 0
            (+ i 1)))
        ((= i n-cell-2))
      (f32vector-set! to (+ i n-cell-1) (f32vector-ref c2 i)))
    to))

(define (im-compose-left img-1 img-2 alignment)
  (im-compose-right img-2 img-1 alignment))

(define (im-compose-right img-1 img-2 alignment)
  (match img-1
    ((width-1 height-1 n-chan-1 idata-1)
     (match img-2
       ((width-2 height-2 n-chan-2 idata-2)
        (list (+ width-1 width-2) height-1 n-chan-1
              (let ((map-proc (if (and (> n-chan-1 1)
                                       (%use-par-map)) par-map map)))
                (map-proc (lambda (channels)
                            (match channels
                              ((c1 c2)
                               (im-compose-right-channel c1 width-1 height-1
                                                      c2 width-2 height-2))))
                    (zip idata-1 idata-2)))))))))

(define (im-compose-left-channel c1 width-1 height-1
                                 c2 width-2 height-2)
  (im-compose-right-channel c2 width-2 height-2 c1 width-1 height-1))

(define (im-compose-right-channel c1 width-1 height-1
                                  c2 width-2 height-2)
  (let* ((to-width (+ width-1 width-2))
         (to (im-make-channel to-width height-1)))
    (do ((i 0
            (+ i 1)))
        ((= i height-1))
      (do ((j 0
              (+ j 1)))
          ((= j width-1))
        (f32vector-set! to
                        (+ (* i to-width) j)
                        (f32vector-ref c1 (+ (* i width-1) j))))
      (do ((j 0
              (+ j 1)))
          ((= j width-2))
        (f32vector-set! to
                        (+ (* i to-width) (+ j width-1))
                        (f32vector-ref c2 (+ (* i width-2) j)))))
    to))

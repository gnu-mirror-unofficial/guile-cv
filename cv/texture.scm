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


(define-module (cv texture)
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
  #:use-module (cv imgproc)
  #:use-module (cv adds)
  
  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-texture
            im-glcm
            im-glcp))


;;;
;;; Texture
;;;

(define* (im-texture image n-gl
                     #:key (dist 1) (i-zero? #t))
  (match image
    ((width height n-chan idata)
     (match idata
       ((c)
        (let ((glcp (im-glcp image n-gl #:dist dist)))
          (im-reduce (im-map glcp (lambda (e) (* e e))) + 0)))
       (else
        (error "Not a GRAY image."))))))

(define (s32-ref channel i j width)
  (s32vector-ref channel
                 (im-fast-channel-offset i j width)))

(define (s32-set! channel i j width val)
  (s32vector-set! channel
                  (im-fast-channel-offset i j width)
                  val))

(define* (im-glcm image n-gl #:key (dist 1) (n-val 255.0))
  (let* ((g-n-cell (* n-gl n-gl))
         (g0 (make-s32vector g-n-cell 0))
         (g45 (make-s32vector g-n-cell 0))
         (g90 (make-s32vector g-n-cell 0))
         (g135 (make-s32vector g-n-cell 0)))
    (match (im-map (lambda (p-val)
                     (round (* (/ p-val n-val) (- n-gl 1))))
               image)
      ((width height n-chan idata)
       (match idata
         ((c)
          (let ((s32c (f32vector->s32vector c)))
            (do ((i 0
                    (+ i 1)))
                ((= i height))
              (do ((j 0
                      (+ j 1)))
                  ((= j width))
                (let ((row (s32-ref s32c i j width)))
                  (if (< j (- width dist))
                      ;; g0
                      (let ((col-g0 (s32-ref s32c i (+ j dist) width)))
                        (s32-set! g0 row col-g0 n-gl
                                  (+ (s32-ref g0 row col-g0 n-gl) 1))
                        (if (> i (- dist 1))
                            ;; g45
                            (let ((col-g45 (s32-ref s32c (- i dist) (+ j dist) width)))
                              (s32-set! g45 row col-g45 n-gl
                                        (+ (s32-ref g45 row col-g45 n-gl) 1))))))
                  (if (> i (- dist 1))
                      ;; g90
                      (let ((col-g90 (s32-ref s32c (- i dist) j width)))
                        (s32-set! g90 row col-g90 n-gl
                                  (+ (s32-ref g90 row col-g90 n-gl) 1))
                        (if (> j (- dist 1))
                            ;; g135
                            (let ((col-g135 (s32-ref s32c (- i dist) (- j dist) width)))
                              (s32-set! g135 row col-g135 n-gl
                                        (+ (s32-ref g135 row col-g135 n-gl) 1)))))))))))
         (else
          (error "Not a GRAY image, n-chan is: " n-chan)))))
    (list n-gl n-gl 4
          (list (s32vector->f32vector g0)
                (s32vector->f32vector g45)
                (s32vector->f32vector g90)
                (s32vector->f32vector g135)))))

(define* (im-glcp image n-gl #:key (dist 1) (n-val 255.0))
  (let* ((glcm (im-glcm image n-gl #:dist dist #:n-val n-val))
         (glcp (im-add glcm (im-transpose glcm))))
    (match glcp
         ((width height n-chan idata)
          (list width height n-chan
                (map (lambda (pr)
                       (match pr
                         ((p r)
                          (im-divide-channel p width height r))))
                  (zip idata
                       (im-reduce glcp + 0))))))))

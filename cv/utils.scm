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


(define-module (cv utils)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (cv init)
  #:use-module (cv support)
  #:use-module (cv idata)
  #:use-module (cv impex)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (%image-cache
	    %image-cache-format
            n-cell->per-core-start-end
            features-bb
            #;bb-points
            bb-point-inside?
            bb-intersect?))


(g-export im-show)

	  
;;;
;;; Guile-CV API
;;;

(define %config (read-config))

(define %image-cache
  (let ((image-cache (and %config
			  (assq-ref %config 'image-cache))))
    (or (and image-cache
	     (expand-tilde image-cache))
	(string-append "/tmp/" (system-get 'uname) "/guile-cv"))))

(define %image-cache-format
  (let ((image-cache-format (and %config
				 (assq-ref %config 'image-cache-format))))
    (or image-cache-format
	"png")))

(define-method (im-show (filename <string>))
  (string-append "#<Image: " filename ">"))

(define-method (im-show (image <list>))
  (unless (access? %image-cache F_OK)
    (mk-dir %image-cache))
  (let ((filename (string-append %image-cache
				 "/"
				 (symbol->string (gensym "im-show-"))
				 "." %image-cache-format)))
    (im-save image filename)
    (im-show filename)))

(define-method (im-show (image <list>) (scale <boolean>))
  (unless (access? %image-cache F_OK)
    (mk-dir %image-cache))
  (let ((filename (string-append %image-cache
				 "/"
				 (symbol->string (gensym "im-show-"))
				 "." %image-cache-format)))
    (im-save image filename scale)
    (im-show filename)))

(define-method (im-show (image <list>) (name <string>))
  (unless (access? %image-cache F_OK)
    (mk-dir %image-cache))
  (let ((filename (string-append %image-cache
				 "/"
				 name
				 "." %image-cache-format)))
    (im-save image filename)
    (im-show filename)))

(define-method (im-show (image <list>) (name <string>) (scale <boolean>))
  (unless (access? %image-cache F_OK)
    (mk-dir %image-cache))
  (let ((filename (string-append %image-cache
				 "/"
				 name
				 "." %image-cache-format)))
    (im-save image filename scale)
    (im-show filename)))

(define* (n-cell->per-core-start-end n-cell #:optional (n-core #f))
  (let* ((n-core (or n-core (current-processor-count)))
         (q (quotient n-cell n-core))
         (r (remainder n-cell n-core))
         (offset1 (+ q r)))
    (map (lambda (i)
           (case i
             ((0)
              (list 0 (+ q r)))
             (else
              (let ((start (+ offset1 (* (- i 1) q))))
                (list start (+ start q))))))
      (iota n-core))))

(define (features-bb features)
  (map (lambda (feature)
         (match feature
           ((area left top right bottom . rest)
            (list left top right bottom))))
    features))

#;(define (bb-points bb)
  (match bb
    ((left top right bottom)
     (list (list left top)		;; x1 y1
           (list right top)		;; x2 y2
           (list left bottom)		;; x3 y3
           (list right bottom)))))	;; x4 y4

#;(define (bb-point-inside? bb pt)
  (match bb
    ((left top right bottom)
     (match pt
       ((pt-x pt-y)
        (and (>= pt-x left)
             (<= pt-x right)
             (>= pt-y top)
             (<= pt-y bottom)))))))

(define (bb-point-inside? bb pt-x pt-y)
  (match bb
    ((left top right bottom)
     (and (>= pt-x left)
          (<= pt-x right)
          (>= pt-y top)
          (<= pt-y bottom)))))

#;(define (bb-intersect? b1 b2)
  (or-l (map (lambda (pt)
               (bb-point-inside? b1 pt))
          (bb-points b2))))

(define (bb-intersect? b1 b2)
  (match b1
    ((left top right bottom)
     (letrec ((point-inside? (lambda (pt-x pt-y)
                               (and (>= pt-x left)
                                    (<= pt-x right)
                                    (>= pt-y top)
                                    (<= pt-y bottom)))))
       (match b2
         ((b2-left b2-top b2-right b2-bottom)
          (or (point-inside? b2-left b2-top)
              (point-inside? b2-right b2-top)
              (point-inside? b2-left b2-bottom)
              (point-inside? b2-right b2-bottom))))))))

;; the following was an attempt to speed this procedure, which gets
;; called millions of times by im-reconstruct (depending on the original
;; image size of course), but it actually is slower. I'll keep it as an
;; example though.
#;(define (bb-intersect? b1 b2)
  (match b1
    ((b1-left b1-top b1-right b1-bottom)
     (match b2
       ((b2-left b2-top b2-right b2-bottom)
        (= (bb-intersect-c b1-left b1-top b1-right b1-bottom
                           b2-left b2-top b2-right b2-bottom)
           1))))))

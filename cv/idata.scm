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


(define-module (cv idata)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:use-module (cv init)
  #:use-module (cv support)


  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (im-image?
	    im-binary?

	    im-=?
	    im-list=?

	    im-make-channel
	    im-make-channels
	    im-make

	    im-copy
	    im-copy-channel

	    im-channel
	    
	    im-channel-offset
	    im-fast-channel-offset
	    im-channel-ref
	    im-fast-channel-ref
	    im-channel-set!
	    im-fast-channel-set!
	    
	    im-ref
	    im-fast-ref
	    im-set!
	    im-fast-set!

	    n-chan->symbol
	    im-collect

	    im-display
            im-display-channel))


(g-export im-size

	  im-width
	  im-height
	  im-n-channel
	  im-channels

	  im-gray?
	  im-rgb?)


;;;
;;; Adds
;;;

(define (im-copy image)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
           (let ((map-proc (if (and (> n-chan 1)
                                    (%use-par-map)) par-map map)))
             (map-proc (lambda (channel)
                         (im-copy-channel channel width height))
                       idata))))))

(define (im-copy-channel channel width height)
  (f32vector-copy channel #:n-cell (* width height)))


;;;
;;; Accessors
;;;

(define-method (im-size (image <list>))
  (list (im-width image)
	(im-height image)
	(im-n-channel image)))

(define-method (im-width (image <list>))
  (match image
    ((width height n-chan idata) width)))

(define-method (im-height (image <list>))
  (match image
    ((width height n-chan idata) height)))

(define-method (im-n-channel (image <list>))
  (match image
    ((width height n-chan idata) n-chan)))

(define-method (im-channels (image <list>))
  (match image
    ((width height n-chan idata) idata)))


;;;
;;; Is?
;;;

(define (im-image? image)
  (and (list? image)
       (= (length image) 4)
       (match image
	 ((width height n-chan idata)
	  (and (integer? width)
	       (>= width 0)
	       (integer? height)
	       (>= height 0)
	       (integer? n-chan)
	       (>= n-chan 1)
               (= n-chan (length idata))
               (and-l (map f32vector? idata)))))))

#!
(apply = (cons n-cell
               (map f32vector-length idata)))
(apply <= (cons 0.0
                (sort (map f32vector-min idata) <)))
(apply >= (cons 255.0
                (sort (map f32vector-max idata) >)))
!#

(define (im-binary? image)
  (match image
    ((width height n-chan idata)
     (and (= n-chan 1)
	  (match idata
	    ((c)
	     (receive (n-val vals)
		 (f32vector-count-distinct c)
               (match vals
                 ((a) (or (float=? a 0.0 0)
                          (float=? a 255.0 0)))
                 ((a b) (or (and (float=? a 0.0 0)
                                 (float=? b 255.0 0))
                            (and (float=? a 255.0 0)
                                 (float=? b 0.0 0))))
                 (else
                  #f)))))))))

(define-method (im-gray? (image <list>))
  (match image
    ((width height n-chan idata)
     (= n-chan 1))))

(define-method (im-rgb? (image <list>))
  (match image
    ((width height n-chan idata)
     (= n-chan 3))))

(define* (im-list=? images #:optional (prec 1.0e-4))
  (match images
    ((image . rest)
     (match image
	((width height n-chan _)
	 (if (and (apply = (apply im-collect 'width rest))
		  (apply = (apply im-collect 'height rest))
		  (apply = (apply im-collect 'n-channel rest)))
	     (catch 'exit
	       (lambda ()
		 (let ((n-cell (* width height)))
		   (for-each (lambda (k)
			       (let* ((chan-k (n-chan->symbol k))
				      (channels (apply im-collect chan-k images)))
				 (unless (f32vector-list=? channels #:prec prec)
				   (throw 'exit #f))))
		       (iota n-chan))
		   #t))
	       (lambda (key index)
		 #t))
	     #f)))) ;; size missmatch
    ((image) #t)
    (() #t)))
  
(define (im-=? . images)
  (match images
    ((prec . rest)
     (if (number? prec)
	 (im-list=? rest prec)
	 (im-list=? images)))
    ((image) #t)
    (() #t)))


;;;
;;; Channels
;;;

(define* (im-make width height n-chan #:optional (init-val 0.0))
  (list width height n-chan
        (im-make-channels width height n-chan init-val)))

(define* (im-make-channels width height n-chan #:optional (init-val 0.0))
  (let ((map-proc (if (and (> n-chan 1)
                           (%use-par-map)) par-map map)))
    (map-proc (lambda (i)
                (im-make-channel width height init-val))
              (iota n-chan))))

(define* (im-make-channel width height #:optional (init-val 0.0))
  (make-f32vector (* width height) init-val))

(define (im-channel image k)
  (match image
    ((width height n-chan idata) (list-ref idata k))))


;;;
;;; Pixels
;;;

(define (im-channel-offset i j width height)
  (if (and (>= i 0)
	   (>= j 0)
	   (< i height)
	   (< j width))
      (+ (* i width) j)
      (error "Out of bound: " i j)))

(define (im-fast-channel-offset i j width)
  (+ (* i width) j))

(define (im-channel-ref channel i j width height)
  (f32vector-ref channel
		 (im-channel-offset i j width height)))

(define (im-fast-channel-ref channel i j width)
  (f32vector-ref channel
		 (im-fast-channel-offset i j width)))

(define (im-channel-set! channel i j width height val)
  (f32vector-set! channel
		  (im-channel-offset i j width height)
		  val))

(define (im-fast-channel-set! channel i j width val)
  (f32vector-set! channel
		  (im-fast-channel-offset i j width)
		  val))

(define (im-ref image i j . n)
  (match image
    ((width height n-chan idata)
     (match n
       (() (im-channel-ref (list-ref idata 0) i j width height))
       ((k)
	(if (and (>= k 0)
		 (< k n-chan))
	    (im-channel-ref (list-ref idata k) i j width height)
	    (error "Out of bound: " k)))))))

(define (im-fast-ref image i j . n)
  (match image
    ((width height n-chan idata)
     (match n
       (() (im-fast-channel-ref (list-ref idata 0) i j width))
       ((k) (im-fast-channel-ref (list-ref idata k) i j width))))))

(define (im-set! image i j . rest)
  (match (match rest
	   ((k val) rest)
	   ((val) (list 0 val))
	   (() (error "Missing value.")))
    ((k val)
     (match image
       ((width height n-chan idata)
        (if (and (>= k 0)
                 (< k n-chan))
            (im-channel-set! (list-ref idata k) i j width height val)
            (error "Out of bound: " k)))))))

(define (im-fast-set! image i j . rest)
  (match (match rest
	   ((k val) rest)
	   ((val) (list 0 val))
	   (() (error "Missing value.")))
    ((k val)
     (match image
       ((width height n-chan idata)
	(im-fast-channel-set! (list-ref idata k) i j width val))))))


;;;
;;; Collect
;;;


(define (n-chan->symbol k)
  (string->symbol (string-append "chan-"
				 (number->string k))))

(define (collect-channel images n)
  (map (lambda (image)
	 (im-channel image n))
    images))

(define (collect-what->chan what)
  (match (string-split (symbol->string what) #\-)
    ((chan id)
     (and (string=? chan "chan")
	  (string->number id)))))

(define (im-collect what . images)
  (case what
    ((width) (map im-width images))
    ((height) (map im-height images))
    ((n-channel) (map im-n-channel images))
    ((size) (map im-size images))
    ((channels) (map im-channels images))
    ((chan-0 gray red) (collect-channel images 0))
    ((chan-1 green) (collect-channel images 1))
    ((chan-2 blue) (collect-channel images 2))
    (else
     (let ((chan-n (collect-what->chan what)))
       (if chan-n
	   (collect-channel images chan-n)
	   (error "Unknown collect symbol: " what))))))


;;;
;;; Display
;;;

(define (channel-name n)
  (string-append "Channel "
		 (number->string (+ n 1))))

(define* (im-display image
		     #:key (proc #f)
		     (port (current-output-port)))
  (match image
    ((width height n-chan idata)
     (for-each (lambda (channel i)
                 (format port "\n~A\n\n" (channel-name i))
		 (im-display-channel channel width height
                                     #:proc proc #:port port))
	 idata
       (iota n-chan))))
  (newline port))

(define* (im-display-channel channel width height
			     #:key (proc #f)
			     (port (current-output-port)))
  (let ((proc (if proc
                  proc
                  (lambda (val)
                    (if (float>=? val 1000.0 0)
                        (format #f "~9e" val)
                        (format #f "~9,5,,,f" val))))))
    (do ((i 0
	    (+ i 1)))
	((= i height))
      (do ((j 0
	      (+ j 1)))
	  ((>= j width) (newline port))
        (format port "  ~A"
                (proc (im-fast-channel-ref channel i j width)))))))

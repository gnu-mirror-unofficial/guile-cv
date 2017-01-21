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


(define-module (cv idata)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
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

	    im-display-channel
	    im-display))


(g-export im-size

	  im-width
	  im-height
	  im-n-channel
	  im-channels

	  im-grey?
	  im-rgb?)


;;;
;;; Adds
;;;

(define (im-copy image)
  (match image
    ((width height n-chan idata)
     (list width height n-chan
	   (case n-chan
	     ((1)
	      (map f32vector-copy idata))
	     (else
	      (par-map f32vector-copy idata)))))))


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
	       (let ((n-cell (* width height)))
		 (and (and-l (map f32vector? idata))
		      (apply = (cons n-cell
				     (map f32vector-length idata)))
		      (apply <= (cons 0.0
				      (sort (map f32vector-min idata) <)))
		      (apply >= (cons 255.0
				      (sort (map f32vector-max idata) >))))))))))

(define (im-binary? image)
  (match image
    ((width height n-chan idata)
     (and (= n-chan 1)
	  (match idata
	    ((c)
	     (receive (n-val vals)
		 (f32vector-count-distinct c)
	       (and (= n-val 2)
		    (float-member 0.0 vals)
		    (float-member 255.0 vals)))))
	  #t))))

(define-method (im-grey? (image <list>))
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
	 (if (and (apply = (cons width (im-collect rest 'width)))
		  (apply = (cons height (im-collect rest 'height)))
		  (apply = (im-collect rest 'n-channel)))
	     (catch 'exit
	       (lambda ()
		 (let ((n-cell (* width height)))
		   (for-each (lambda (k)
			       (let* ((chan-k (n-chan->symbol k))
				      (channels (im-collect images chan-k)))
				 (unless (f32vector-list=? channels prec)
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

(define* (im-make width height n #:optional (init-val 0.0))
  (list width height n
	(im-make-channels width height n init-val)))

(define* (im-make-channels width height n #:optional (init-val 0.0))
  (case n
    ((1) (list (im-make-channel width height init-val)))
    (else
     (par-map (lambda (i)
		(im-make-channel width height init-val))
	 (iota n)))))

(define* (im-make-channel width height #:optional (init-val 0.0))
  (make-f32vector (* width height)
                  init-val))

(define (im-channel image n)
  (match image
    ((width height n-chan idata) (list-ref idata n))))


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
	(if (and (>= val 0.0)
		 (<= val 255.0))
	    (if (and (>= k 0)
		     (< k n-chan))
		(im-channel-set! (list-ref idata k) i j width height val)
		(error "Out of bound: " k))
	    (error "Invalid pixel value: " val)))))))

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

(define (im-collect images what)
  (case what
    ((width) (map im-width images))
    ((height) (map im-height images))
    ((n-channel) (map im-n-channel images))
    ((size) (map im-size images))
    ((channels) (map im-channels images))
    ((chan-0 grey red) (collect-channel images 0))
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

(define* (im-display-channel channel width height
			     #:key (proc identity)
			     (port (current-output-port)))
  (do ((i 0
	    (+ i 1)))
	((= i height))
      (do ((j 0
	      (+ j 1)))
	  ((>= j width)
	   (newline port))
	(display "  ")
	(display (proc (im-fast-channel-ref channel i j width)) port))))

(define* (im-display image
		     #:key (proc identity)
		     (port (current-output-port)))
  (match image
    ((width height n-chan idata)
     (for-each (lambda (channel i)
		 (display (channel-name i) port)
		 (newline port)
		 (im-display-channel channel width height
				     #:proc proc
				     #:port port))
	 idata
       (iota n-chan)))))

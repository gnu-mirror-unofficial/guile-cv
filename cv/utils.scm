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
	    %image-cache-format))


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

(define-method (im-show (image <list>) (name <string>))
  (unless (access? %image-cache F_OK)
    (mk-dir %image-cache))
  (let ((filename (string-append %image-cache
				 "/"
				 name
				 "." %image-cache-format)))
    (im-save image filename)
    (im-show filename)))

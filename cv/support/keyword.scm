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

;; this file is a copy of (grip keyword)
;; http://www.nongnu.org/grip/

;;; Code:


(define-module (cv support keyword)
  #:use-module (cv support push)
  
  #:export (split-keyword-args))


(define (split-keyword-args-1 args grab a b)
  (if (null? args)
      (values a b)
      (if (memq (car args) grab)
	  (split-keyword-args-1 (cddr args)
				grab
				a
				(push*! (car args) (cadr args) b))
	  (split-keyword-args-1 (cddr args)
				grab
				(push*! (car args) (cadr args) a)
				b))))

(define (split-keyword-args args grab-these)
  (split-keyword-args-1 args grab-these (list) (list)))


#!

;; missing good example/mini tests

!#

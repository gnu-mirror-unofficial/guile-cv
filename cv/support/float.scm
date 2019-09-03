;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2019
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


(define-module (cv support float)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (cv support libguile-cv)

  #:export (float-zero?
	    float=?
	    float<?
	    float<=?
	    float>?
	    float>=?
	    float-round
	    float-member
            floatlist-display
            ;; from libguile-cv
            +float-min+
            +float-max+
            float->int))


(define* (float-zero? f1 #:optional (prec 1.0e-4))
  (<= (abs f1) prec))

(define* (float=? f1 f2 #:optional (prec 1.0e-4))
  (<= (abs (- f1 f2)) prec))

(define* (float<? f1 f2 #:optional (prec 1.0e-4))
  (let ((diff (- f1 f2)))
    (and (negative? diff)
	 (> (abs diff) prec))))

(define* (float<=? f1 f2 #:optional (prec 1.0e-4))
  (let ((diff (- f1 f2)))
    (or (and (negative? diff)
	     (> (abs diff) prec))
	(<= (abs diff) prec))))

(define* (float>? f1 f2 #:optional (prec 1.0e-4))
  (let ((diff (- f1 f2)))
    (and (positive? diff)
	 (> (abs diff) prec))))

(define* (float>=? f1 f2 #:optional (prec 1.0e-4))
  (let ((diff (- f1 f2)))
    (or (and (positive? diff)
	     (> (abs diff) prec))
	(<= (abs diff) prec))))

(define (float-round float . dec)
  (let ((m (match dec
	     (() 100)
	     ((k) (expt 10 k)))))
    (/ (round (* m float)) m)))

(define* (float-member f vals #:optional (prec 1.0e-4))
  (let ((n-val (length vals))
	(result #f))
    (case n-val
      ((0) #f)
      (else
       (do ((i 0
	       (+ i 1)))
	   ((or (= i n-val)
		result) result)
	 (when (float=? f (list-ref vals i) prec)
	   (set! result i)))))))

(define* (floatlist-display lst #:key (proc #f)
                            (port (current-output-port)))
  (let ((proc (if proc
                  proc
                  (lambda (val)
                    (if (float>=? val 1000.0 0)
                        (format #f "~9e" val)
                        (format #f "~9,5,,,f" val))))))
    (for-each (lambda (item)
                (format port "  ~A~%" (proc item)))
        lst)))


;;;
;;; From liguile-cv
;;;

(define +float-min+ (float_min))
(define +float-max+ (float_max))

(define float->int float_to_int)

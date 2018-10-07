;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2018
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


(define-module (tests segmentation)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (unit-test)
  #:use-module (cv))


(define %img
  (list 3 3 1
        (list #f32(0 255 255
                   0   0 255
                   0   0   0))))

(define %crack-edge
  (list 5 5 1
        (list #f32(0  127  255  255  255
                   0  127  127  127  255
                   0    0    0  127  255
                   0    0    0  127  127
                   0    0    0    0    0))))


(define-class <guile-cv-tests-segmentation> (<test-case>))

(define-method (test-im-crack-edge (self <guile-cv-tests-segmentation>))
  (assert-true (im-=? (im-crack-edge %img #:marker 127)
                      %crack-edge)))


(exit-with-summary (run-all-defined-test-cases))

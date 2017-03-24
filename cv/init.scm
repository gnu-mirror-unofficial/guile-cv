;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2017
;;;; David Pirotte <david at altosw dot be>

;;;; This file is part of Guile-CV

;;;; Guile-CV is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU Lesser General Public License as
;;;; published by the Free Software Foundation; either version 3 of the
;;;; License, or (at your option) any later version.

;;;; Guile-CV is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.

;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with Guile-CV.  If not, see
;;;; <https://www.gnu.org/licenses/lgpl.html>.
;;;;

;;; Commentary:

;;; Code:


(define-module (cv init)
  #:use-module (system foreign)
  
  #:export (%libvigra-c
            %libguile-cv
            %use-par-map))


(define %libvigra-c (dynamic-link "libvigra_c"))

(define %libguile-cv (dynamic-link "libguile-cv"))

(define %use-par-map (make-parameter #t))

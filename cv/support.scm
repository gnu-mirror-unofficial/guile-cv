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


(define-module (cv support)
  #:use-module (oop goops)
  #:use-module (cv support goops)
  #:use-module (cv support g-export)
  #:use-module (cv support utils)
  #:use-module (cv support float)
  #:use-module (cv support push)
  #:use-module (cv support keyword)
  #:use-module (cv support modules)
  #:use-module (cv support float)
  #:use-module (cv support s32vector)
  #:use-module (cv support f32vector)
  #:use-module (cv support f64vector)
  #:use-module (cv support libguile-cv)
  #:use-module (cv support utils)
  #:use-module (cv support latex)
  #:use-module (cv support pi)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last))


(eval-when (expand load eval)
  (re-export-public-interface (oop goops)
			      (cv support goops)
			      (cv support g-export)
			      (cv support utils)
			      (cv support float)
			      (cv support push)
			      (cv support keyword)
			      (cv support modules)
                              (cv support float)
                              (cv support s32vector)
			      (cv support f32vector)
                              (cv support f64vector)
                              (cv support libguile-cv)
                              (cv support utils)
                              (cv support latex)
                              (cv support pi)))

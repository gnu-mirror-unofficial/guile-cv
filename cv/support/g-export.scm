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

;; this file is a copy of (grip g-export)
;; http://www.nongnu.org/grip/

;;; Code:


(define-module (cv support g-export)
  ;; #:use-module (oop goops)

  #:export (module-g-export!
	    g-export))

(define (module-g-export! m names)
  ;; The following check won't work anymore, because in preparation for
  ;; guile-2.2, which does not query (default-duplicate-binding-handler)
  ;; at module expand eval load time as it did for guile-2.0, a bad
  ;; decision imo, we have to declare #:dulicates in each individual
  ;; module instead, and remove our patched version of guild, which in 2.2
  ;; has a bug wrt this anyway.  I keep this here i case in the future
  ;; things change for better instead of for worst, who knows, in
  ;; guile-3.3 maybe...
  #;(unless (memq 'merge-generics
		(default-duplicate-binding-handler))
    (display "Warning: you are using g-export [goops export], which re-export defined
names and should _only_ be used for getters, setters, accessors and
methods, but you did not ask to merge duplicate generic functions:
unless you really know what you are doing, you should.\n"
	     (current-output-port)))
  (let ((public-i (module-public-interface m)))
    (for-each (lambda (name)
                (let* ((internal-name (if (pair? name) (car name) name))
                       (external-name (if (pair? name) (cdr name) name))
                       (var (module-variable m internal-name)))
		  (if var
		      (module-add! public-i external-name var)
		      (module-add! public-i external-name
				   (module-ensure-local-variable! m internal-name)))))
	names)))

(define-syntax-rule (g-export name ...)
  (eval-when (expand load eval)
    (call-with-deferred-observers
     (lambda ()
       (module-g-export! (current-module) '(name ...))))))

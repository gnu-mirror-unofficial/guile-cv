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


(define-module (build-aux latex)
  #:use-module (ice-9 format)

  #:export (latex-check-for-document-class
	    latex-check-for-package))


(define %latex-document-class-source
  "\\documentclass{~A}
\\begin{document}
  nothing
\\end{document}\n")

(define %latex-package-source
  "\\documentclass{article}
  \\usepackage{~A}
\\begin{document}
  nothing
\\end{document}\n")

(define %check-command
  "cd /tmp; pdflatex --interaction=nonstopmode ~A > /dev/null")

(define (latex-write-check-source filename source check-for)
  (with-output-to-file filename
    (lambda ()
      (format #t "~?" source (list check-for)))))

(define (latex-check-for-document-class name)
  (let* ((filename (string-append "/tmp/" name ".tex"))
	 (cmd (format #f "~?" %check-command (list filename))))
    (latex-write-check-source filename %latex-document-class-source name)
    (status:exit-val (system cmd))))

(define (latex-check-for-package name)
  (let* ((filename (string-append "/tmp/" name ".tex"))
	 (cmd (format #f "~?" %check-command (list filename))))
    (latex-write-check-source filename %latex-package-source name)
    (status:exit-val (system cmd))))

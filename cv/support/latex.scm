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


(define-module (cv support latex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 format)
  #:use-module (cv support utils)

  #:export (%latex-cache
            latex-write-histogram-title
            latex-write-histogram-table
            latex-write-histogram-table-grey
            latex-write-text
            latex-compile
            latex-pdftoppm))

(define %latex-cache
  (let ((in (string-append "/tmp/"
                           (system-get 'uname)
                           "/guile-cv/latex")))
    (unless (access? in F_OK)
      (mk-dir in))
    in))

(define (latex-purge in name)
  (let ((cmd (format #f "cd ~A; rm -f ~A.*" in name)))
    (or (zero? (system cmd))
	(error "subprocess returned non-zero result code" cmd))))

(define (latex-write-histogram-title in subtitle)
  (let ((name (symbol->string (gensym "h-title-"))))
    (latex-purge in name)
    (let* ((b-name (string-append name ".tex"))
           (filename (string-append in "/" b-name))
           (ostream (open-output-file filename)))
      (format ostream "~A" %latex-begin)
      (format ostream "~?" %latex-h-title
              (list subtitle))
      (format ostream "~A" %latex-end)
      (close ostream)
      filename)))

(define (latex-write-histogram-table in vals)
  (match vals
    ((n-cell
      r-mean r-std-dev r-min r-max r-mode r-val
      g-mean g-std-dev g-min g-max g-mode g-val
      b-mean b-std-dev b-min b-max b-mode b-val)
     (let ((name (symbol->string (gensym "h-table-"))))
       (latex-purge in name)
       (let* ((b-name (string-append name ".tex"))
              (filename (string-append in "/" b-name))
              (ostream (open-output-file filename)))
         (format ostream "~A" %latex-begin)
         (format ostream "~?" %latex-h-table
                 (list n-cell
                       r-mean r-std-dev r-min r-max r-mode r-val
                       g-mean g-std-dev g-min g-max g-mode g-val
                       b-mean b-std-dev g-min b-max b-mode b-val))
         (format ostream "~A" %latex-end)
         (close ostream)
         filename)))))

(define (latex-write-histogram-table-grey in n-cell mean std-dev mini maxi mode val)
  (let ((name (symbol->string (gensym "h-table-grey-"))))
    (latex-purge in name)
    (let* ((b-name (string-append name ".tex"))
           (filename (string-append in "/" b-name))
           (ostream (open-output-file filename)))
      (format ostream "~A" %latex-begin)
      (format ostream "~?" %latex-h-table-grey
              (list n-cell mean std-dev mini maxi mode val))
      (format ostream "~A" %latex-end)
      (close ostream)
      filename)))

(define (latex-write-text in text)
  (let ((name (symbol->string (gensym "text-"))))
    (latex-purge in name)
    (let* ((b-name (string-append name ".tex"))
           (filename (string-append in "/" b-name))
           (ostream (open-output-file filename)))
      (format ostream "~A" %latex-begin)
      (format ostream "~?" %latex-text
              (list text))
      (format ostream "~A" %latex-end)
      (close ostream)
      filename)))

(define %latex-compile-cmd
  ;; when/to debug, use this instead: "pdflatex ~A"
  "pdflatex --interaction=nonstopmode ~A > /dev/null")

(define (latex-compile filename)
  (let* ((in (dirname filename))
         (b-name (basename filename))
         (f-name (basename b-name ".tex"))
         (cmd (format #f "cd ~A; ~A"
                      in (format #f "~?" %latex-compile-cmd
                                 (list b-name))))
         (pdf-filename (string-append in "/" f-name ".pdf")))
    (if (zero? (system cmd))
        pdf-filename
	(error "subprocess returned non-zero result code" cmd))))

(define %latex-pdftoppm-cmd "pdftoppm -png -r 72 ~A.pdf > ~A.png")

(define* (latex-pdftoppm filename #:key (res 72) (to "png"))
  ;; Note that 72 is a bit arbitrary, hence optional to coversome other
  ;; needs, but that is the appropirate value for histogram
  ;; tablestables, both so they (still) look nice and fit within the
  ;; width of histograms.
  (let* ((in (dirname filename))
         (b-name (basename filename))
         (f-name (basename b-name ".pdf"))
         (cmd (format #f "cd ~A;  pdftoppm -png -r 72 ~A.pdf > ~A.png"
                      in f-name f-name))
         (png-filename (string-append in "/" f-name ".png")))
    (if (zero? (system cmd))
        png-filename
	(error "subprocess returned non-zero result code" cmd))))

(define (latex-text-to-png text)
  (let ((filename (latex-write-text %latex-cache text))
        (name "text"))
    #t))

;;;
;;; Some 'templates'
;;;

(define %latex-begin
  "% -*- coding: utf-8; -*-

\\documentclass[
  convert,
  11pt]
{standalone}

%%%
%%% Packages
%%%

%% \\usepackage[american]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{lmodern}

\\usepackage[
  table,
  dvipsnames,
  svgnames,
  x11names,
  fixpdftex]
{xcolor}

\\usepackage{booktabs}
\\usepackage{siunitx}
\\usepackage[math]{iwona}

   
%%%
%%% Colours
%%%

\\definecolor{red}{rgb}{0.82, 0.0, 0.11}
\\definecolor{green}{rgb}{0, 1, 0}
\\definecolor{blue}{rgb}{0, 0.08, 0.45}


%%%
%%% document
%%%

\\begin{document}\n\n")

(define %latex-end
  "\\end{document}\n")

(define %latex-h-title
  "\\begin{tabular}
  {c}
  {\\Large \\color{darkgray} Histogram} \\\\
  \\arrayrulecolor{black!60} \\toprule
  \\color{darkgray} ~A
\\end{tabular}\n")

(define %latex-h-table
  "\\begin{tabular}
  %% {l n{3}{2} n{3}{2} n{3}{2} n{5}{2}}
  {l S S r r r}

  \\multicolumn{5}{l}
    {\\textcolor{darkgray}{Nb of pixels: \\tablenum{~A}}}
  \\\\[2mm]


  & \\color{darkgray} {Mean}
  & \\color{darkgray} {Std. Dev.}
  & \\color{darkgray} {Min}
  & \\color{darkgray} {Max}
  & \\multicolumn{1}{c}{\\color{darkgray} {Mode}} \\\\
  \\arrayrulecolor{black!60} \\toprule

  \\textcolor{red}{Red}
  & \\color{red} ~A
  & \\color{red} ~A
  & \\color{red} ~A
  & \\color{red} ~A
  & \\color{red} ~A (~A) \\\\

  \\textcolor{OliveGreen}{Green}
  & \\color{OliveGreen} ~A
  & \\color{OliveGreen} ~A
  & \\color{OliveGreen} ~A
  & \\color{OliveGreen} ~A
  & \\color{OliveGreen} ~A (~A) \\\\

  \\textcolor{blue}{Blue}
  & \\color{blue} ~A
  & \\color{blue} ~A
  & \\color{blue} ~A
  & \\color{blue} ~A
  & \\color{blue} ~A (~A)

\\end{tabular}\n")

(define %latex-h-table-grey
  "\\begin{tabular}
  {l S S c c c}

  \\multicolumn{5}{l}
    {\\textcolor{darkgray}{Nb of pixels: \\tablenum{~A}}}
  \\\\[2mm]

  & \\color{darkgray} {Mean}
  & \\color{darkgray} {Std. Dev.}
  & \\color{darkgray} {Min}
  & \\color{darkgray} {Max}
  & \\multicolumn{1}{c}{\\color{darkgray} {Mode}} \\\\
  \\arrayrulecolor{black!60} \\toprule

  ~~~~
  & \\color{darkgray} ~A
  & \\color{darkgray} ~A
  & \\color{darkgray} ~A
  & \\color{darkgray} ~A
  & \\color{darkgray} ~A (~A)

\\end{tabular}\n")


(define %latex-text
  "\\begin{tabular}
  {c}
  \\color{darkgray} ~A
\\end{tabular}\n")

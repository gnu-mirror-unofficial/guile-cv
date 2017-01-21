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

;; this file is a copy of (grip utils)
;; http://www.nongnu.org/grip/

;;; Code:


(define-module (cv support utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)

  #:export (storage-get
	    storage-set
	    displayln
	    dimfi
	    warning
	    abort
	    and-l
	    identities
	    system-get
	    mk-dir
	    read-config
	    interleavs
	    expand-tilde))


(define storage-get #f)
(define storage-set #f)

(let ((storage (list)))
  (set! storage-get
	(lambda (key)
	  (case key
	    ((all) storage)
	    (else
	     (assq-ref storage key)))))
  (set! storage-set
	(lambda (key value)
	  (set! storage
		(assq-set! storage key value)))))


(define* (displayln msg #:optional (port #f))
  (if port
      (begin
	(display msg port)
	(newline port))
      (begin
	(display msg)
	(newline))))

(define (dimfi . items)
  ;; if the first item is a port, we use it.
  (if (port? (car items))
      (let ((p (car items)))
	(display ";; " p)
	(for-each (lambda (item)
		    (truncated-print item p) (display " " p))
	    (cdr items))
	(display "\n" p))
      (begin
	(display ";; ")
	(for-each (lambda (item)
		    (truncated-print item) (display " " ))
	    items)
	(display "\n")))
  (car (last-pair items)))

(define* (warning what msg port #:key (msg-2 #f))
  (display (string-append "Warning: " what ": " msg) port)
  (newline port)
  (when msg-2
    (display msg-2 port)
    (newline port)))

(define* (abort what msg port #:key (msg-2 #f) (code -1))
  (display (string-append "ERROR: " what ": " msg) port)
  (newline port)
  (when msg-2
    (display msg-2 port)
    (newline port))
  (exit code))

(define (and-l ll)
  (if (null? ll)
      #t
      (if (car ll)
	  (and-l (cdr ll))
	  #f)))

(define (identities . args)
  args)

(define system-get #f)

(let* ((id (geteuid))
       (urec (getpwuid id))
       (gid (passwd:gid urec))
       (grec (getgrgid gid)))
  (set! system-get
	(lambda (what)
	  (case what
	    ((urec) urec)
	    ((uid) (passwd:uid urec))
	    ((uname) (passwd:name urec))
	    ((udir) (passwd:dir urec))
	    ((gid) gid)
	    ((grec) grec)
	    ((gname) (group:name grec))))))

#!

FIXME
put these in the test-suite

(system-get 'urec)
(system-get 'uid)
(system-get 'uname)
(system-get 'udir)
(system-get 'gid)
(system-get 'grec)
(system-get 'gname)

!#

(define (mk-dir dirname)
  ;; the builtin mkdir guile core function calls the operating system one,
  ;; not the GNU Linux shell script, which does not implement the '-p'
  ;; and that is what we want ...
  (let* ((cmd (string-append "mkdir -p " dirname))
	 (status (system cmd)))
    (or (= (status:exit-val status) 0)
	(error "subprocess returned non-zero result code" cmd))))

(define (read-config)
  (catch #t
    (lambda ()
      (call-with-input-file
	  (string-append (system-get 'udir) "/.config/guile-cv.conf")
	read))
    (lambda (key . parameters)
      #f)))

(define (interleave . lls)
  (concatenate (apply map list lls)))

(define (expand-tilde path)
  (match (string-split path #\/)
    (("~" . rest)
     (string-append (system-get 'udir)
		    (string-concatenate
		     (interleave (map (lambda (i)
					"/")
				   (iota (length rest)))
				 rest))))
    (else
     path)))

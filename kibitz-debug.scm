;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; kibitz       - gov.uk Picker Configuration Tools.
;;; kibitz-debug - Helpers for errors and warnings.
;;;
;;; gov.uk Open Registers are a way of expressing an authoritative list that
;;; you can trust.
;;;
;;; kibitz is a set of tools for combining Register data with other data in
;;; order to produce a configuration file that can be used to drive an
;;; interactive, multi-lingual, Location Picker that a user can use to easily
;;; select a valid Location from a list.
;;;
;;; This module provides support for error reporting from the other modules.
;;;
;;;
;;;  Copyright (C) 2017, Andy Bennett, Crown Copyright (Government Digital Service).
;;;
;;;  Permission is hereby granted, free of charge, to any person obtaining a
;;;  copy of this software and associated documentation files (the "Software"),
;;;  to deal in the Software without restriction, including without limitation
;;;  the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;;  and/or sell copies of the Software, and to permit persons to whom the
;;;  Software is furnished to do so, subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be included in
;;;  all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;;  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;  DEALINGS IN THE SOFTWARE.
;;;
;;; Andy Bennett <andyjpb@digital.cabinet-office.gov.uk>, 2017
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module kibitz-debug
	(die
	 warnings
	 warn)

(import chicken scheme)

; Units - http://api.call-cc.org/doc/chicken/language
(use extras)

; Eggs - http://wiki.call-cc.org/chicken-projects/egg-index-4.html
;(use )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers for errors and warnings

(define (die . rest)
  (fprintf (current-error-port) "Error: ")
  (apply fprintf (current-error-port) rest)
  (fprintf (current-error-port) "\n")
  (exit 1))

(define warnings (make-parameter 0))

(define (warn . rest)
  (warnings (add1 (warnings)))
  (fprintf (current-error-port) "Warning: ")
  (apply fprintf (current-error-port) rest)
  (fprintf (current-error-port) "\n"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

)


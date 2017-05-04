;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; kibitz          - gov.uk Picker Configuration Tools.
;;; kibitz-register - Support for loading the records from a register into a
;;;                   kibitz configuration graph.
;;;
;;; gov.uk Open Registers are a way of expressing an authoritative list that
;;; you can trust.
;;;
;;; kibitz is a set of tools for combining Register data with other data in
;;; order to produce a configuration file that can be used to drive an
;;; interactive, multi-lingual, Location Picker that a user can use to easily
;;; select a valid Location from a list.
;;;
;;; This module provides support for parsing a register and using it to build a
;;; basic set of nodes in the configuration graph. For each item in the
;;; register a node is added to the graph. Synonym nodes are then added to
;;; represent alternative ways of describing the same thing: by primary key
;;; and, sometimes, other fields.
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

(module kibitz-register
	(add-nodes-from-records
	 add-nodes-from-rsf
	  )

(import chicken scheme)

; Units - http://api.call-cc.org/doc/chicken/language
(use data-structures srfi-1)

; Eggs - http://wiki.call-cc.org/chicken-projects/egg-index-4.html
(use kibitz-debug kibitz-graph morc)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loaders

(define (add-nodes-from-records state canonical register-name records)
  (assert (string? register-name))

  (let ((register-symbol (string->symbol register-name)))
    (fold
      (lambda (v s)
        (let ((pk      (car v))
              (item    (cdr v))
              (node-id (conc register-name ":" (car v))))
          (if (not (equal? pk (item-ref register-symbol item)))
            (warn "Item key for record ~S in ~S does not match record key: might be an index." pk register-name))
          (if (state-ref node-id s)
	    (error "Node for ~S already exists in graph." node-id))
	  (let* ((name-en-GB    (item-ref 'name item))
		 (official-name (item-ref 'official-name item))
		 (s (state-update
		      node-id
		      (update-node
			(make-node)
			name-en-GB:     name-en-GB
			canonical-mask: (if canonical 1 0)
			canonical:      canonical
			stable-name:    #t
			display-name:   #t)
		      s))
		 (s ; add the official-name if it's different
		   (if (or (not official-name) (equal? official-name name-en-GB))
		     s
		     (add-nym node-id official-name #t s)))
		 (s ; add the code from the primary key
		   (add-nym node-id (item-ref register-symbol item) #t s)))
	    s)))
      state
      records)))

(define (add-nodes-from-rsf state canonical register-name filename)
  (add-nodes-from-records state canonical register-name (rsf->records filename)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

)


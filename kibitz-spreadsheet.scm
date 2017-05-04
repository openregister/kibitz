;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; kibitz             - gov.uk Picker Configuration Tools.
;;; kibitz-spreadsheet - Helpers for parsing configuration data supplied in
;;;                      tabular format.
;;;
;;; gov.uk Open Registers are a way of expressing an authoritative list that
;;; you can trust.
;;;
;;; kibitz is a set of tools for combining Register data with other data in
;;; order to produce a configuration file that can be used to drive an
;;; interactive, multi-lingual, Location Picker that a user can use to easily
;;; select a valid Location from a list.
;;;
;;; This module provides support for parsing a spreadsheet file and using it to
;;; annotate the configuration graph with non-register data. Usually
;;; spreadsheet data is used to link the nodes created by kibitz-register into
;;; a heirarchy and to supply synonyms and other alternatives that describe
;;; them.
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

(module kibitz-spreadsheet
	(; Parsers for CVS input files
	 make-spreadsheet-reader ; Also used by csv-xml
	 add-nodes-from-spreadsheet

	 ; Deserialisers
	 string                  ; Also used by scheme
	 curie
	 string-list
	 curie-list
	 empty
	 ignore

	 ; Field hander ADTs
	 col-spec-ref
	 col-spec-convert
	 col-spec-actions

	 ; Field handlers
	 welsh
	 nyms
	 child-of
	 )

(import chicken scheme)

; Units - http://api.call-cc.org/doc/chicken/language
(use data-structures srfi-1 extras)

; Eggs - http://wiki.call-cc.org/chicken-projects/egg-index-4.html
(require-extension utf8)
(use list-utils clojurian-syntax utf8-srfi-13 csv-xml)
(use kibitz-debug kibitz-graph)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsers for Spreadsheet input files
;;;

; Take a list of field names and a csv-xml's next-row procedure and return a
; procedure that, when called, returns the next row as an alist.
; From Knodium's etl-tools.scm
(define (next-row-as-alist-maker field-names next-row)
  (lambda ()
    (let ((next-row (next-row)))
      (if (null? next-row)
	next-row
	(zip-alist field-names next-row)))))

(define (make-spreadsheet-reader filename
			 #!key
			 (separator-chars           '(#\,))
			 (strip-leading-whitespace  #t)
			 (strip-trailing-whitespace #t)
			 (first-row-is-field-names  #f))

  (assert first-row-is-field-names "Sorry; no support for spreadsheets without fieldnames in the first row!")

  (let* ((make-reader ; Configure a spreadsheet reader
	   (make-csv-reader-maker `((separator-chars            . ,separator-chars)
				    (strip-leading-whitespace?  . ,strip-leading-whitespace)
				    (strip-trailing-whitespace? . ,strip-trailing-whitespace))))
	 (next-row        ; Bind it to a spreadsheet file
	   (make-reader (open-input-file filename)))
	 (field-names     ; Fetch the field name from the first row
	   (next-row))
	 (next-row        ; Make a procedure that returns the next row in the file
	   (next-row-as-alist-maker field-names next-row)))
    next-row))


; state     - The current kibitz-graph.
; col-spec  - Specification of names, deserialisers and handlers for each column in the spreadsheet.
; canonical - How to handle things found in the spreadsheet that are not already in the graph.
;             #t: add a canonical node to the graph and continue.
;             #f: emit a warning, add a non-canonical node to the graph and continue.
; row-key   - The name of the column that contains the primary key for the row.
; reader    - A procedure that returns the next row in the spreadsheet as an alist.
;             Something returned from make-spreadsheet-reader.
(define (add-nodes-from-spreadsheet state col-spec canonical row-key reader)
  (let ((next-row        reader)
	(state           (make-parameter state))
	(row-key-convert (col-spec-convert (col-spec-ref row-key col-spec))))
    (csv-for-each
      (lambda (row)
	;(pp row)
	;(newline)
	(let* ((row-name (row-key-convert (alist-ref row-key row equal?)))
	       (the-state (state))
	       (the-state
		 ; Ensure that this node exists in the graph
		 (if (state-ref row-name the-state)
		   the-state
		   (begin
		     (if (not canonical)
		       (warn "Found node in spreadsheet that does not already appear in the graph: ~S!" row-name))
		     ; FIXME: This code assumes that we are working from
		     ;        David's Spreadsheet. i.e. that columns called
		     ;        "Name" and "Official-name" are present and
		     ;        contain what we think they contain.
		     (let* ((name-en-GB    ((col-spec-convert (col-spec-ref "Name" col-spec))          (alist-ref "Name"          row equal?)))
			    (official-name ((col-spec-convert (col-spec-ref "Official-name" col-spec)) (alist-ref "Official Name" row equal?)))
			    (s the-state)
			    (s (state-update
				 row-name
				 (update-node
				   (make-node)
				   name-en-GB:     name-en-GB
				   canonical-mask: (if canonical 1 0)
				   canonical:      canonical
				   stable-name:    #f
				   display-name:   #t)
				 s))                           ; display-name
			    (s ; add the official-name if it's different
			      (if (or (not official-name) (equal? official-name name-en-GB))
				s
				(add-nym row-name official-name #t s))))
		       s))))
	       (the-state
		 (fold ; process each field in the row
		   (lambda (col state)
		     (let* ((col-name  (car col))
			    (col-spec* (col-spec-ref col-name col-spec))
			    (_         (assert col-spec* (sprintf "~A~S~A~A~S~A~A~S\n" "Could not find col-spec for " col-name ".\n\n"
								  "       Spreadsheet columns are: " (map car row) "\n\n"
								  "       ...and we have col-specs for: " (map car col-spec))))
			    (convert   (col-spec-convert col-spec*))
			    (actions   (col-spec-actions col-spec*))
			    (col-value (convert (cdr col))))
		       (fold
			 (lambda (action state)
			   (action state row-name col-name col-value))
			 state
			 actions)))
		   the-state
		   row)))
	  (state the-state)))
      next-row)
      (state)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deserialisers
;;;
;;; These turn strings from the CVS parser into scheme objects.
;;;

(define (string x)
  (if (string? x)
    x
    (abort (conc x " is not a string!"))))

(define (curie x)
  (if-let* ((_ (string? x))
            (y (string-split x ":"))
            (_ (= 2 (length y)))
            (_ (member (car y) '("country" "territory" "uk" "at"))))
           x
           (abort (conc x " is not a curie!"))))

(define (string-list x)
  (assert (string? x))
  (map
    string-trim-both
    (string-split x ",\n" #f)))

(define (curie-list x)
  (assert (string? x))
  (map
    curie
    (map
      string-trim-both
      (string-split x ",\n" #f))))

(define (empty x)
  (if (equal? "" x)
    #f
    (abort (conc x " is not empty!"))))

(define (ignore x)
  #f)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Field handler ADTs
;;;

;;;
;;; An ADT for specifying how to handle each field found in the spreadsheet
;;; file.
;;;

; Takes a field name and returns the specification for a particular field.
; If the field name cannot be found we look for a field named #f. If it is
; present in the table then it is used to handle the field.
(define (col-spec-ref col col-spec)
  (alist-ref
    col
    col-spec
    equal?
    (alist-ref
      #f
      col-spec
      equal?)))

; Takes a specification for a particular field as returned from col-spec-ref.
; Returns the procedure that can deserialise this kind of field.
(define col-spec-convert first)

; Takes a specification for a particular field as returned from col-spec-ref.
; Returns the list of handlers for this kind of field.
(define col-spec-actions second)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Field handlers
;;;
;;; Take the data in each field and use it to improve the state of the graph.
;;; Takes a graph and some details about the current field and returns a new
;;; graph.
;;;

(define (welsh state country-id col-name welsh-name)
  (assert (string? country-id))
  (assert (string? welsh-name))

  (let ((node (state-ref country-id state)))
    (assert node (conc "welsh: No node for " country-id))
    (assert (not (node-ref '(names cy) node)))

    (state-update
      country-id
      (update-node
	node
	name-cy: welsh-name)
      state)))


; assume all -nyms are in English, not Welsh
(define (nyms #!key (display-name #f))
  (lambda (state country-id col-name nyms)
    (assert (string? country-id))
    (assert (list?   nyms))

    (let ((node (state-ref country-id state)))
      (assert node (conc "nyms: No node for " country-id))

      (fold
        (lambda (nym state)
          (assert (string? nym))
          (add-nym country-id nym display-name state))
        state
        nyms))))


(define (child-of state country-id col-name parents)
  (assert (curie country-id))
  (assert (list? parents))

  (let ((node (state-ref country-id state)))
    (assert node (conc "child-of: No node for " country-id))

    (fold
      (lambda (parent state)
        (assert (curie parent))
        (assert (state-ref country-id state) (conc "child-of: No node for " parent))
        (add-edge country-id parent state))
      state
      parents)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



)


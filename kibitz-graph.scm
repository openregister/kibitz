;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; kibitz       - gov.uk Picker Configuration Tools.
;;; kibitz-graph - Support for the configuration file format itself.
;;;
;;; gov.uk Open Registers are a way of expressing an authoritative list that
;;; you can trust.
;;;
;;; kibitz is a set of tools for combining Register data with other data in
;;; order to produce a configuration file that can be used to drive an
;;; interactive, multi-lingual, Location Picker that a user can use to easily
;;; select a valid Location from a list.
;;;
;;; This module provides support for building and manipulating the graph
;;; structure that constitutes the picker configuration file. The format is
;;; documented in picker-input.sample.scm
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

(module kibitz-graph
	(make-kibitz-graph
	  state-ref
	  state-update
	  make-node
	  node-ref
	  update-node
	  has-edge?
	  add-edge
	  add-nym
	  )

(import chicken scheme)

; Units - http://api.call-cc.org/doc/chicken/language
(use data-structures srfi-1)

; Eggs - http://wiki.call-cc.org/chicken-projects/egg-index-4.html
(use vector-lib)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State ADT
;;;
;;; An ADT for the state objects that are passed around by the state handlers.
;;; This is an in-memory representation of the configuration graph.
;;;
;;;  '(("country:gb" . ((names .
;;;                            ((en-GB . "United Kingdom")
;;;                             (cy    . "Deyrnas Unedig")))
;;;                     (meta  .
;;;                            ((canonical . #t)
;;;                             (canonical-mask . 1)
;;;                             (stable-name . #t)))
;;;                     (edges .
;;;                            ((from .
;;;                                   #())))))
;;;    ("synonym:gb:gb" . ((names .
;;;                               ((en-GB . "gb")))
;;;                        (meta  .
;;;                               ((canonical . #f)
;;;                                (canonical-mask . 0)
;;;                                (stable-name . #f)))
;;;                        (edges .
;;;                               ((from .
;;;                                      #("country:gb")))))))
;;;

(define (make-state)
  '())

(define make-kibitz-graph make-state)

(define (state-ref id state)
  (assert (string? id))
  (alist-ref id state equal?))

(define (state-update id value state)
  (assert (string? id))
  (alist-update id value state equal?))

(define (make-node)
  '((names . ())
    (meta  . ())
    (edges . ())))

; Defaults for the node
(define node-default
  '((names .
           ((en-GB . #f)
            (cy    . #f)))
    (meta  .
           ((canonical      . #f)
            (canonical-mask . 0)
            (stable-name    . #f)
            (display-name   . #f)))
    (edges .
           ((from . #())))))

(define (node-ref path node)

  (assert (list? path))
  (assert (= 2 (length path)))
  (assert (fold (lambda (v s) (or s (symbol? v))) #f path))

  (or
    (alist-ref (second path)
               (alist-ref (first path) node))
    (alist-ref (second path)
               (alist-ref (first path) node-default))))

(define (update-node node
                     #!key
                     (name-en-GB     (node-ref '(names en-GB)          node))
                     (name-cy        (node-ref '(names cy)             node))
                     (canonical      (node-ref '(meta  canonical)      node))
                     (canonical-mask (node-ref '(meta  canonical-mask) node))
                     (stable-name    (node-ref '(meta  stable-name)    node))
                     (display-name   (node-ref '(meta  display-name)   node))
                     (edges-from     (node-ref '(edges from)           node)))

  (assert (string? name-en-GB))
  (assert (or (string? name-cy) (not name-cy)))
  (assert (boolean? canonical))
  (assert (integer? canonical-mask))
  (assert (boolean? stable-name))
  (assert (boolean? display-name))
  (assert (vector?  edges-from))
  (assert (or (equal? #() edges-from) (vector-fold (lambda (i s v) (and s (string? v))) #t edges-from)))

  `((names .
           ((en-GB . ,name-en-GB)
            (cy    . ,name-cy)))
    (meta  .
           ((canonical      . ,canonical)
            (canonical-mask . ,canonical-mask)
            (stable-name    . ,stable-name)
            (display-name   . ,display-name)))
    (edges .
           ((from . ,edges-from)))))

(define (has-edge? edge edges)
  (vector-fold
    (lambda (i s v)
      (or s (equal? v edge)))
    #f
    edges))

; Inefficient but medea dictates a vector if we want a JSON array out and I'm
; too lazy to tranform the data structure before output.
(define (edges-cons edge edges)
  (assert (string? edge))
  (assert (vector? edges))

  (list->vector
    (cons edge (vector->list edges))))

(define (add-edge from to state)
  (let* ((node       (state-ref from state))
         (edges-from (node-ref '(edges from) node)))
    (if (has-edge? to edges-from)
      (begin
        ;(warn "Edge already exists between ~S and ~S. ~S" from to node)
        state)
      (state-update from
                    (update-node node
                                 edges-from: (edges-cons to edges-from))
                    state))))

; Add a node to the graph that represents a -nym for another node.
;   for          - The id of the node to add the nym for.
;   nym          - The English text of the nym.
;   display-name - Whether to display this node to the user.
;   state        - The graph.
(define (add-nym for nym display-name state)
  (let* ((nym-id   (conc "nym:" nym))
         (nym-node (state-ref nym-id state)))
    (if nym-node
      (let ((names-en-GB       (node-ref '(names en-GB)        nym-node))
            (meta-display-name (node-ref '(meta  display-name) nym-node)))
        (cond ; If the node is found, check if it's the same or if it needs to be upgraded to displayable.
          ((and ; important parts of node match
             (equal? names-en-GB       nym)
             (equal? meta-display-name display-name))
           ; just add the edge
           (add-edge nym-id for state))
          ((and ; name matches and current node is displayable
             (equal? names-en-GB nym)
             meta-display-name
             (not display-name))
           ; just add the edge
           (add-edge nym-id for state))
          ((and ; name matches and current node is not displayable
             (equal? names-en-GB nym)
             (not meta-display-name)
             display-name)
           ; make it displayable and add the edge
           (add-edge nym-id for
                     (state-update
                       nym-id
                       (update-node
                         nym-node
                         display-name: display-name)
                       state)))
          (else
            (error "Node for ~S already exists but does not match." nym-id))))
      (state-update
        nym-id
        (update-node
          (make-node)
          name-en-GB:     nym
          canonical-mask: 0
          canonical:      #f
          stable-name:    #t
          display-name:   display-name
          edges-from:     `#(,for))
        state))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

)

; TODO:
;   + rewrite calls to vector-fold with vector-{every,any}


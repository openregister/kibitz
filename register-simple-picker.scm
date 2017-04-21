; Takes an RSF URL and a field name and produces a picker data file.
; csi -s register-simple-picker primary-key name-key
; curl https://country.register.gov.uk/download-rsf | csi -s register-simple-picker.scm country official-name
; curl https://country.register.gov.uk/download-rsf | csi -s register-simple-picker.scm country official-name | csi -s picker-input-to-json.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copied from create-picker-input.scm for expedience

(require-extension utf8)
(use srfi-1)
(use utils list-utils clojurian-syntax utf8-srfi-13 vector-lib morc)

(assert (= 2 (length (command-line-arguments))))
(define register-pk       (first (command-line-arguments)))
(define register-name-key (string->symbol (second (command-line-arguments))))

(define die-on-warnings #f)

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


(define (make-state)
  '())

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
          (let* ((s (add-node
                      s                              ; state
                      canonical                      ; canonical
                      node-id                        ; node-id
                      (item-ref register-name-key item) ; name-en-GB
                      (item-ref register-name-key item) ; official-name
                      (if canonical 1 0)             ; canonical-mask
                      #t                             ; stable-name
                      #t))                           ; display-name
                 (s ; add the country code
                   (add-nym node-id (item-ref register-symbol item) #t s)))
            s)))
      state
      records)))


(define (add-node state canonical node-id name-en-GB official-name canonical-mask stable-name display-name)
  (let* ((s state)
         (s ; add the node to the graph
           (state-update
             node-id
             (update-node
               (make-node)
               name-en-GB:     name-en-GB
               canonical-mask: canonical-mask
               canonical:      canonical
               stable-name:    stable-name
               display-name:   display-name)
             s))
         (s ; add the official-name if it's different
           (if (or (not official-name) (equal? official-name name-en-GB))
             s
             (add-nym node-id official-name display-name s))))
    s))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Program logic
;;;

(define state    (make-parameter (make-state)))

; Import the named Register.
(state
  (fold
    (lambda (r s)
      (add-nodes-from-records
        s
        (second r)  ; canonical?
        (first r)   ; register-name
        (third r))) ; records
    (state)
    `((,register-pk #t ,(read-rsf)))))



(if (and die-on-warnings (not (= 0 (warnings))))
  (die "~S warnings encountered during processing. Aborting!" (warnings))
  (pp (state)))


; TODO
;  + Support multiple named fields
;  + Be tolerant of fields that are empty (assert (string? field))


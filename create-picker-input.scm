; csi -s create-picker-input.scm data/Location\ picker\ data\ -\ Data.csv
; csi -s create-picker-input.scm data/Location\ picker\ data\ -\ Data.csv | csi -s ./visualise-picker-input.scm

(require-extension utf8)
(use srfi-1)
(use csv-xml list-utils clojurian-syntax utf8-srfi-13 vector-lib morc)

(assert (= 1 (length (command-line-arguments))))
(define filename (first (command-line-arguments)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsers for CSV input files
;;;

(define make-demo-csv-reader
  (make-csv-reader-maker '((separator-chars #\,)
                           (strip-leading-whitespace?  . #t)
                           (strip-trailing-whitespace? . #t))))

(define next-row
  (make-demo-csv-reader
    (open-input-file filename)))

; Take a list of field names and a csv-xml's next-row procedure and return a
; procedure that, when called, returns the next row as an alist.
; From Knodium's etl-tools.scm
(define (next-csv-row-as-alist-maker field-names next-row)
  (lambda ()
    (let ((next-row (next-row)))
      (if (null? next-row)
        next-row
        (zip-alist field-names next-row)))))

; first row is field names
(define next-location (next-csv-row-as-alist-maker (next-row) next-row))


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
;;; An ADT for specifying how to handle each field found in the CSV file.
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
;;; State ADT
;;;
;;; An ADT for the state objects that are passed around by the state handlers.
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
;;; Field handler specification
;;;


; Field Name, Type (string / integer / currency), List of places to put it.
(define col-spec
  `(("Andy's key"               ,curie       ())
    ("Type"                     ,ignore      ())
    ("Name"                     ,string      ())
    ("Official-name"            ,ignore      ())
    ("Code"                     ,ignore      ())
    ("Excluded"                 ,ignore      ())
    ("Territory belongs to"     ,ignore      ())
    ("Territory belongs to code",curie-list  (,child-of))
    ("Welsh"                    ,string      (,welsh))
    ("Passport applicant typos" ,string-list (,(nyms display-name: #f)))
    ("Endonyms"                 ,string-list (,(nyms display-name: #t)))
    ("Combined Synonyms"        ,string-list (,(nyms display-name: #t)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Program logic
;;;

(define state    (make-parameter (make-state)))

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
                      (item-ref 'name item)          ; name-en-GB
                      (item-ref 'official-name item) ; official-name
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



(define uk-records (rsf->records "data/uk.rsf"))

; Import the country, territories & uk registers.
(state
  (fold
    (lambda (r s)
      (add-nodes-from-records
        s
        (second r)  ; canonical?
        (first r)   ; register-name
        (third r))) ; records
    (state)
    `(("country"   #t ,(rsf->records "data/country.rsf"))
      ("territory" #t ,(rsf->records "data/territory.rsf"))
      ("uk"        #f ,uk-records))))

; Annotate country:gb with the contents of the uk register



(define row-key "Andy's key")
(define row-key-convert
  (col-spec-convert (col-spec-ref row-key col-spec)))

; Annotate and extend the graph with the contents of David D's spreadsheet.
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
                 (warn "Found node in spreadsheet that does not appear in the registers: ~S!" row-name)
                 (add-node
                   the-state
                   #f ; canonical
                   row-name ; node-id
                   ((col-spec-convert (col-spec-ref "Name" col-spec))          (alist-ref "Name" row equal?)) ; name-en-GB
                   ((col-spec-convert (col-spec-ref "Official-name" col-spec)) (alist-ref "Name" row equal?)) ; official-name
                   0; canonical-mask
                   #f ; stable-name
                   #t)))) ; display-name
           (the-state
             (fold ; process each field in the row
               (lambda (col state)
                 (let* ((col-name  (car col))
                        (col-spec  (col-spec-ref col-name col-spec))
                        (_         (assert col-spec (conc "Could not find col-spec for " col-name)))
                        (convert   (col-spec-convert col-spec))
                        (actions   (col-spec-actions col-spec))
                        (col-value (convert (cdr col))))
                   (fold
                     (lambda (action state)
                       (action state row-name col-name col-value))
                     state
                     actions)))
               the-state
               row)))
      (state the-state)))
  next-location)


(if (and die-on-warnings (not (= 0 (warnings))))
  (die "~S warnings encountered during processing. Aborting!" (warnings))
  (pp (state)))


; TODO:
;   + rewrite calls to vector-fold with vector-{every,any}


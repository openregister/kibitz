; This alist represents a flattened directed acyclic graph of nodes.
; When the user types something in the box we try to match it to one of the
; nodes. We then follow the edges until we reach a "canonical" node and suggest
; that to the user as the preferred value of the field.
;
; This data is constructed from various source datasets such as registers,
; synonyms, spelling mistakes and some config data which determines which nodes
; are considered "canonical".
;
; Each member of the alist is a pair that, specifies a node. The car contains
; the name of the node. The cdr contains several data sections. When a node is
; selected as the value for the field the primary underlying field value is the
; name of the node. The field may also return suplementary values in the form
; of one of the names from the data section in the language that the service
; requires and one of the names from the data section in the language that was
; displayed to the user.
;
; Each node has several data sections thus:
;   names - The free text names that are recognised for this node.
;           If this node is selected as the value of the field then this value,
;           in the appropriate language for the user, is used as the value
;           displayed by the field.
;           This data can also be used to match a user's input to a number of
;           nodes. When a number of nodes have been matched, the edges for each
;           are chased to determine the relevant canonical nodes. The set of
;           canonical nodes are then displayed as a set of suggestions from
;           which the user can select the value of the field.
;   meta  - Flags for the node.
;           canonical      - A boolean describing whether this is a canonical
;                            node.  i.e. one that can be selected as a value
;                            for the field.  This is a helper flag for the MVP.
;                            In the event that it is not present, it can be
;                            calculated from the canonical-mask data.
;           canonical-mask - A bitfield <=53bits wide. The canonical flag can
;                            be calculated from this field by masking it with
;                            the configuration parameter. This allows up to 53
;                            configurations to be represented by a single data
;                            file.
;           stable-name    - A boolean describing whether the name of this node
;                            is guranteed to remain the same across versions of
;                            this data. If this field is #t then nodes with the
;                            same name in different configurations and versions
;                            represent the same entity. If this field is #f
;                            then no gurantees are provided about the name of
;                            the node. A #t value in this field does not imply
;                            that this node will always be present in all
;                            versions and configurations but it does signal
;                            that where it does appear, it refers to the same
;                            entity. The entity refered to by a stable name may
;                            not always resolve to a record in a Register but,
;                            where it doesn't, steps have been taken to ensure
;                            that the entity referred to is, in fact, stable.
;                            It is reasonable to assert that stable-name is #t
;                            when either canonical is #t or canonical-mask is
;                            non-zero.  Node names should only be independently
;                            persisted when stable-name is #t.  Node names with
;                            unstable names MUST NOT be persisted or
;                            transmitted outside the scope of the graph
;                            described by this data.
;           display-name   - A boolean describing whether the name of this node
;                            is suitable for displaying to the user of the
;                            picker. If this field is #t then this node is
;                            suitable for display. This field is typically #f
;                            for nodes that provide misspellings or other data
;                            that is provided in order to increase the matching
;                            ability of the graph.
;   edges - Edges to and from the node. Only the "from" sub-section is required
;           to be populated. Edges have no data of their own.
;           from - The list of edges that point away from this node to other
;                  nodes. This list may contain any number of edges. The list
;                  is a list of strings that represent the node on the other
;                  end of the edge.
;                  This list may be populated for nodes considered canonical in
;                  the current configuration.
;
;
; Andy Bennett <andyjpb@digital.cabinet-office.gov.uk>, 2017/01/04

; csi -s ./picker-input.scm > picker-input.json

'(("country:gb" . ((names .
			  ((en-GB . "United Kingdom")
			   (cy    . "Deyrnas Unedig")))
		   (meta  .
			  ((canonical . #t)
			   (canonical-mask . 1)
			   (stable-name . #t)
			   (display-name . #t)))
		   (edges .
			  ((from .
				 #())))))
  ("synonym:gb:gb" . ((names .
			     ((en-GB . "gb")))
		      (meta  .
			     ((canonical . #f)
			      (canonical-mask . 0)
			      (stable-name . #f)
			      (display-name . #t)))
		      (edges .
			     ((from .
				    #("country:gb"))))))
  ("synonym:gb:uk" . ((names .
			     ((en-GB . "uk")))
		      (meta  .
			     ((canonical . #f)
			      (canonical-mask . 0)
			      (stable-name . #f)
			      (display-name . #t)))
		      (edges .
			     ((from .
				    #("country:gb"))))))
  ("uk:nir" . ((names .
		      ((en-GB . "Northern Ireland")
		       (cy    . "Gogledd Iwerddon")))
	       (meta  .
		      ((canonical . #f)
		       (canonical-mask . 0)
		       (stable-name . #t)
		       (display-name . #t)))
	       (edges .
		      ((from .
			     #("country:gb"))))))
  ("synonym:nir:nir" . ((names .
			       ((en-GB . "nir")))
			(meta  .
			       ((canonical . #f)
				(canonical-mask . 0)
				(stable-name . #f)
				(display-name . #t)))
			(edges .
			       ((from .
				      #("uk:nir"))))))
  ("uk:wls" . ((names .
		      ((en-GB . "Wales")
		       (cy    . "Cymru")))
	       (meta  .
		      ((canonical . #f)
		       (canonical-mask . 0)
		       (stable-name . #t)
		       (display-name . #t)))
	       (edges .
		      ((from .
			     #("country:gb"))))))
  ("synonym:wls:wls" . ((names .
			       ((en-GB . "wls")))
			(meta  .
			       ((canonical . #f)
				(canonical-mask . 0)
				(stable-name . #f)
				(display-name . #t)))
			(edges .
			       ((from .
				      #("uk:wls"))))))
  ("uk:sct" . ((names .
		      ((en-GB . "Scotland")
		       (cy    . "Yr Alban")))
	       (meta  .
		      ((canonical . #f)
		       (canonical-mask . 0)
		       (stable-name . #t)
		       (display-name . #t)))
	       (edges .
		      ((from .
			     #("country:gb"))))))
  ("synonym:sct:sct" . ((names .
			       ((en-GB . "sct")))
			(meta  .
			       ((canonical . #f)
				(canonical-mask . 0)
				(stable-name . #f)
				(display-name . #t)))
			(edges .
			       ((from .
				      #("uk:sct"))))))
  ("uk:eng" . ((names .
		      ((en-GB . "England")
		       (cy    . "Lloegr")))
	       (meta  .
		      ((canonical . #f)
		       (canonical-mask . 0)
		       (stable-name . #t)
		       (display-name . #t)))
	       (edges .
		      ((from .
			     #("country:gb"))))))
  ("synonym:eng:eng" . ((names .
			       ((en-GB . "eng")))
			(meta  .
			       ((canonical . #f)
				(canonical-mask . 0)
				(stable-name . #f)
				(display-name . #t)))
			(edges .
			       ((from .
				      #("uk:eng"))))))
  ("synonym:eng:1" . ((names .
			     ((en-GB . "Enkalnd")))
		      (meta  .
			     ((canonical . #f)
			      (canonical-mask . 0)
			      (stable-name . #f)
			      (display-name . #f)))
		      (edges .
			     ((from .
				    #("uk:eng"))))))
  ("spelling:eng:ingland" . ((names .
				    ((en-GB . "Ingland")))
			     (meta  .
				    ((canonical . #f)
				     (canonical-mask . 0)
				     (stable-name . #t)
				     (display-name . #f)))
			     (edges .
				    ((from .
					   #("uk:eng"))))))
  ("uk:gbn" . ((names .
		      ((en-GB . "Great Britain")
		       (cy    . "Prydain Fawr")))
	       (meta  .
		      ((canonical . #f)
		       (canonical-mask . 0)
		       (stable-name . #t)
		       (display-name . #t)))
	       (edges .
		      ((from .
			     #("country:gb"))))))
  ("synonym:gbn:gbn" . ((names .
			       ((en-GB . "gbn")))
			(meta  .
			       ((canonical . #f)
				(canonical-mask . 0)
				(stable-name . #f)
				(display-name . #f)))
			(edges .
			       ((from .
				      #("uk:gbn"))))))
  ("territory:im" . ((names .
			    ((en-GB . "Isle of Man")
			     (cy    . "Ynys Manaw")))
		     (meta  .
			    ((canonical . #t)
			     (canonical-mask . 1)
			     (stable-name . #t)
			     (display-name . #t)))
		     (edges .
			    ((from .
				   #())))))
  ("territory:hk" . ((names .
			    ((en-GB . "Hong Kong")
			     (cy    . "Hong Kong")))
		     (meta  .
			    ((canonical . #t)
			     (canonical-mask . 1)
			     (stable-name . #t)
			     (display-name . #t)))
		     (edges .
			    ((from .
				   #())))))
  ("territory:sx" . ((names .
			    ((en-GB . "Sint Maarten (Dutch part)")
			     (cy    . "Sint Maarten (rhan Iseldireg)")))
		     (meta  .
			    ((canonical . #t)
			     (canonical-mask . 1)
			     (stable-name . #t)
			     (display-name . #t)))
		     (edges .
			    ((from .
				   #())))))
  ("synonym:sx:sx" . ((names . ; If official-name is different from name then we add a synonym for the official-name.
			     ((en-GB . "Sint Maarten")))
		      (meta  .
			     ((canonical . #f)
			      (canonical-mask . 0)
			      (stable-name . #f)
			      (display-name . #t)))
		      (edges .
			     ((from .
				    #("territory:sx"))))))
  ("territory:bq-se" . ((names .
			       ((en-GB . "Sint Eustatius")
				(cy    . "Sint Eustatius")))
			(meta  .
			       ((canonical . #t)
				(canonical-mask . 1)
				(stable-name . #t)
				(display-name . #t)))
			(edges .
			       ((from .
				      #())))))
  ("territory:bat" . ((names .
			     ((en-GB . "British Antarctic Territory")
			      (cy    . "Tiriogaeth Antarctig Prydain")))
		      (meta  .
			     ((canonical . #f)
			      (canonical-mask . 0)
			      (stable-name . #t)
			      (display-name . #t)))
		      (edges .
			     ((from .
				    #("country:gb"))))))
  ("territory:aq" . ((names .
			    ((en-GB . "Antarctica")
			     (cy    . "Antarctica")))
		     (meta  .
			    ((canonical . #f)
			     (canonical-mask . 0)
			     (stable-name . #t)
			     (display-name . #t)))
		     (edges .
			    ((from .
				   #("country:gb" "country:fr"))))))
  ("country:fr" . ((names .
			  ((en-GB . "France")
			   (cy    . "France")))
		   (meta  .
			  ((canonical . #t)
			   (canonical-mask . 1)
			   (stable-name . #t)
			   (display-name . #t)))
		   (edges .
			  ((from .
				 #())))))
  ("synonym:fr:fr" . ((names . ; If official-name is different from name then we add a synonym for the official-name.
			     ((en-GB . "The French Republic")))
		      (meta  .
			     ((canonical . #f)
			      (canonical-mask . 0)
			      (stable-name . #f)
			      (display-name . #t)))
		      (edges .
			     ((from .
				    #("country:fr"))))))
  ("endonym:fr" . ((names .
			  ((fr-FR . "République Française")))
		   (meta  .
			  ((canonical . #f)
			   (canonical-mask . 0)
			   (stable-name . #f)
			   (display-name . #t)))
		   (edges .
			  ((from .
				 #("country:fr"))))))




  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code to turn the alist into JSON.

(use medea)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Library code from medea
(use vector-lib srfi-14)
(define (for-each/delimiter proc list delimiter)
  (let ((size (if (vector? list)
		(vector-length list)
		(length list)))
	(count 0))
    ((if (vector? list) vector-for-each for-each)
     (lambda (d #!optional (ad d))
       (proc ad)
       (set! count (add1 count))
       (unless (= count size)
	 (display #\,)))
     list)))
(define control-chars
  (ucs-range->char-set #x0 #x20))
(define json-escape-chars
  "\"\\bfnrt/")
(define json-escapes
  (map (lambda (e c)
	 (cons (string e) (string #\\ c)))
       (string->list "\"\\\b\f\n\r\t")
       (string->list json-escape-chars)))
(define (display/escape-control-chars s)
  (let loop ((i 0))
    (let ((j (string-index s control-chars i)))
      (if j
	(begin
	  (display (substring/shared s i j))
	  (display "\\u")
	  (display (string-pad (number->string (char->integer (string-ref s j)) 16) 4 #\0))
	  (loop (+ j 1)))
	(display (substring/shared s i))))))
(define (unparse-string s)
  (display #\")
  (display/escape-control-chars (string-translate* s json-escapes))
  (display #\"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(json-unparsers
  (cons
    ; Serialise strings in alist cars. This results in a non-reversible encoding.
    (cons list?
	  (lambda (object)
	    (display #\{)
	    (for-each/delimiter
	      (lambda (member)
		(unparse-string
		  (cond
		    ((string? (car member))
		     (car member))
		    ((symbol? (car member))
		     (symbol->string (car member)))))
		(display #\:)
		(write-json (cdr member)))
	      object
	      #\,)
	    (display #\})))
    (json-unparsers)))

(display
  (json->string
    (second
      (with-input-from-file "picker-input.scm" read))))


;wombat
;cousin

; index: case insensitive matching
; do we need to know what the user's input language is?
; assume: the field can only ever take one of the canonical values.


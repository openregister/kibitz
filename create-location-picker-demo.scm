; csi -s create-location-picker-demo.scm data/Location\ picker\ data\ -\ Data.csv
; csi -s create-location-picker-demo.scm data/Location\ picker\ data\ -\ Data.csv | csi -s ./visualise-picker-input.scm

(require-extension utf8)

(load "kibitz")
(import kibitz-debug)
(import kibitz-graph)
(import kibitz-register)
(import kibitz-spreadsheet)

(assert (= 1 (length (command-line-arguments))))
(define filename (first (command-line-arguments)))

(define die-on-warnings #f)


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
    ("Combined Synonyms"        ,string-list (,(nyms display-name: #t)))
    ("DWP Synonyms"             ,ignore      ())))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Program logic
;;;

(define state    (make-parameter (make-kibitz-graph)))


; Import the country, territories & uk registers.
(state
  (fold
    (lambda (r s)
      (add-nodes-from-rsf
        s
        (second r)  ; canonical?
        (first r)   ; register-name
        (third r))) ; records
    (state)
    `(("country"   #t "data/country.rsf")
      ("territory" #t "data/territory.rsf")
      ("uk"        #f "data/uk.rsf"))))


(define row-key "Andy's key")

; Annotate and extend the graph with the contents of David D's spreadsheet.

(define next-location
  (make-spreadsheet-reader filename
			   first-row-is-field-names: #t))

(state (add-nodes-from-spreadsheet (state) col-spec #f row-key next-location))


(if (and die-on-warnings (not (= 0 (warnings))))
  (die "~S warnings encountered during processing. Aborting!" (warnings))
  (pp (state)))


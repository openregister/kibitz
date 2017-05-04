(require-extension utf8)
(use posix srfi-1 irregex)

(define (process-status rc)
  (define (wait-signaled? x) (not (= 0 (bitwise-and x 127))))
  (define (wait-signal x) (bitwise-and x 127))
  (define (wait-exit-status x) (arithmetic-shift x -8))
  (if (wait-signaled? rc)
    (values #f (wait-signal rc))
    (values #t (wait-exit-status rc))))

(define (generate-pdf graphviz-input)
  (let-values (((fd temp-path) (file-mkstemp "/tmp/gengraph.XXXXXX")))
              (let ((temp-port (open-output-file* fd)))
                (fprintf temp-port "~A" graphviz-input)
                (close-output-port temp-port))
              (receive (ok ec) (process-status (system (sprintf "ccomps -x ~A | dot | gvpack |neato -Tpdf -n2" temp-path)))
                       (assert (and ok (= 0 ec))))))


;;;
;;; Stolen from create-picker-input.scm
;;;

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

;;;


(define graphviz-input
  (with-output-to-string
    (lambda ()
      (printf "~A\n" (conc "digraph G {
                           colorscheme=svg;
                           labelloc=\"t\";
                           label=\"Picker Graph - " (seconds->string (current-seconds)) "\";
                           splines=true;
                           sep=\"2,2\"
                           nodesep=0.5
                           overlap=scale;
                           graph [fontname=\"Helvetica-Bold\", fontweight=Bold, fontsize=15];
                           node [fontname=Helvetica, fontsize=10];
                           edge [fontname=Helvetica, fontsize=10];"))

                           (for-each
                             (lambda (node)
                               (let ((id   (car node))
                                     (node (cdr node)))
                                 (printf "\"~A\" [label=\"~A\\n~A\\n~A\",style=filled,fillcolor=~A];\n"
                                         id
                                         id (node-ref '(names en-GB) node) (node-ref '(names cy) node)
                                         (cond
                                           ((node-ref '(meta canonical)    node) "mediumspringgreen")
                                           ((node-ref '(meta display-name) node) "grey")
                                           (else "white")))
                                 (for-each
                                   (lambda (remote-id)
                                     (printf "\"~A\" -> \"~A\";\n" id remote-id))
                                   (vector->list (node-ref '(edges from) node)))))
                             (read))

                           (printf "}\n"))))



      (generate-pdf graphviz-input)

      (exit)


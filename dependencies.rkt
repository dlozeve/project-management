#lang racket/base

(provide project-graph)

(require graphviz
	 pict
	 "project.rkt")

(define (rounded-box w h color)
  (filled-rounded-rectangle w h 5
			    #:color color
			    #:border-color "black"))

(define deliverable-pict (rounded-box 150 40 "LightSkyBlue"))
(define task-pict (rounded-box 150 40 "Gainsboro"))

(define (project-graph project)
  (define task-nodes
    (for*/list ([group project]
		[task group])
      `(,(number->string (eq-hash-code task))
	#:label ,(task-name task))))
  (define deliverables-nodes
    (for*/list ([group project]
		[task group]
		[deliverable (task-deliverables task)])
      `(,(number->string (eq-hash-code deliverable))
	#:label ,(deliverable-name deliverable))))
  (define deliverables-edges
    (for*/list ([group project]
		[task group]
		[deliverable (task-deliverables task)])
      `(edge (,(number->string (eq-hash-code task)) ,(number->string (eq-hash-code deliverable))))))
  (define inputs-edges
    (for*/list ([group project]
		[task group]
		[input (task-inputs task)])
      `(edge (,(number->string (eq-hash-code input)) ,(number->string (eq-hash-code task))))))
  (make-digraph
   (append task-nodes deliverables-nodes deliverables-edges inputs-edges)))

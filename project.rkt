#lang racket/base

(require racket/stream
	 racket/list
	 racket/generic)

(provide (struct-out resource-struct)
	 resource
	 (struct-out deliverable-struct)
	 deliverable
	 inputs
	 actions
	 outcomes
	 deliverables
	 work
	 milestone
	 (struct-out task-struct)
	 task
	 (struct-out group-struct)
	 group
	 (struct-out project-struct)
	 project)

;; Resource and deliverable declarations
(struct resource
  (name role)
  #:transparent
  #:name resource-struct
  #:constructor-name make-resource)

(struct deliverable
  (name description)
  #:transparent
  #:name deliverable-struct
  #:constructor-name make-deliverable)

(define-syntax-rule (resource id name role)
  (define id (make-resource name role)))

(define-syntax-rule (deliverable id name desc)
  (define id (make-deliverable name desc)))

;; Convenience macros to visually distinguish between inputs,
;; deliverables, etc.
(define-syntax-rule (inputs a ...)
  (list a ...))
(define-syntax-rule (deliverables a ...)
  (list a ...))
(define-syntax-rule (actions a ...)
  (list a ...))
(define-syntax-rule (outcomes a ...)
  (list a ...))
(define-syntax-rule (milestone m)
  m)

;; Work estomates are represented as a hashmap between a (previously
;; declared) resource and a number of days.
(define-syntax work
  (syntax-rules ()
    [(work) (make-hash)]
    [(work resource estimate other ...)
     (let [(h (work other ...))]
       (hash-set! h resource estimate)
       h)]))

;; Tasks containing all the useful information required. 
(struct task
  (name
   inputs
   actions
   outcomes
   deliverables
   work
   milestone)
  #:name task-struct
  #:constructor-name make-task
  #:transparent
  ;; #:methods gen:custom-write
  ;; [(define (write-proc task port mode)
  ;;    (fprintf port "Task: ~a\n" (task-name task))
  ;;    (unless (empty? (task-inputs task))
  ;;      (fprintf port "Inputs:\n")
  ;;      (for ([input (task-inputs task)])
  ;; 	 (fprintf port "  - ~a\n" input)))
  ;;    (unless (empty? (task-actions task))
  ;;      (fprintf port "Actions:\n")
  ;;      (for ([action (task-actions task)])
  ;; 	 (fprintf port "  - ~a\n" action)))
  ;;    (unless (empty? (task-outcomes task))
  ;;      (fprintf port "Outcomes:\n")
  ;;      (for ([outcome (task-outcomes task)])
  ;; 	 (fprintf port "  - ~a\n" outcome)))
  ;;    (unless (empty? (task-deliverables task))
  ;;      (fprintf port "Deliverables:\n")
  ;;      (for ([deliverable (task-deliverables task)])
  ;; 	 (fprintf port "  - ~a\n" deliverable)))
  ;;    (fprintf port "Work:\n")
  ;;    (for ([(resource work) (task-work task)])
  ;;      (fprintf port "~a: ~a\n" resource work)))]
  )

;; Milestones are optional, so we declare tasks using a function with
;; an optional argument.
(define (task name inputs actions outcomes deliverables work [milestone #f])
  (make-task name inputs actions outcomes deliverables work milestone))

;; Groups implement the generic interface for streams, to allow easy
;; iteration of tasks.
(struct group
  (name tasks)
  #:transparent
  #:name group-struct
  #:constructor-name make-group
  #:methods gen:stream
  [(define (stream-empty? grp)
     (empty? (group-tasks grp)))
   (define (stream-first grp)
     (first (group-tasks grp)))
   (define (stream-rest grp)
     (rest (group-tasks grp)))])

(define-syntax group
  (syntax-rules ()
    [(group name) (make-group name '())]
    [(group name task ...) (make-group name (list task ...))]))

;; Project are also streams, in order to iterate on groups.
(struct project
  (name groups)
  #:transparent
  #:name project-struct
  #:constructor-name make-project
  #:methods gen:stream
  ;; use the generic version of stream-empty?, otherwise it tries to
  ;; reuse the version localised to the project struct
  [(define (stream-empty? prj)
     (empty? (project-groups prj)))
   (define (stream-first prj)
     (first (project-groups prj)))
   (define (stream-rest prj)
     (rest (project-groups prj)))])

(define-syntax project
  (syntax-rules ()
    [(project name) (make-project name '())]
    [(project name group ...) (make-project name (list group ...))]))

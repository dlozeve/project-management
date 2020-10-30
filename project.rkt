#lang racket/base

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
	 project
	 project-tasks-stream)

(require racket/stream
	 racket/list
	 racket/generic)

(module+ test
  (require rackunit))

;; Resource and deliverable declarations
(struct resource
  (name role)
  #:transparent
  #:name resource-struct
  #:constructor-name make-resource)

(define-syntax-rule (resource id name role)
  (define id (make-resource name role)))

(module+ test
  (test-begin
   (resource res1 "John Smith" "Engineer")
   (check-pred resource? res1)
   (check-equal? res1 (make-resource "John Smith" "Engineer"))
   ;; Role is significant, not just the name
   (check-not-equal? res1 (make-resource "John Smith" "Manager"))))

(struct deliverable
  (name description)
  #:transparent
  #:name deliverable-struct
  #:constructor-name make-deliverable)

(define-syntax-rule (deliverable id name desc)
  (define id (make-deliverable name desc)))

(module+ test
  (test-begin
   (deliverable report "Important report" "This report is important")
   (check-pred deliverable? report)
   (check-equal? report (make-deliverable "Important report" "This report is important"))))

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

(module+ test
  (check-equal? (deliverables 'report) '(report))
  (check-equal? (outcomes) '())
  (check-not-equal? (deliverables 'report) 'report)
  (check-equal? (milestone "Milestone") "Milestone"))

;; Work estomates are represented as a hashmap between a (previously
;; declared) resource and a number of days.
(define-syntax work
  (syntax-rules ()
    [(work) (make-hash)]
    [(work resource estimate other ...)
     (let [(h (work other ...))]
       (hash-set! h resource estimate)
       h)]))

(module+ test
  (test-begin
   (define w (work 'alice 3 'bob 2))
   (check-pred hash? w)
   (check-eq? (hash-count w) 2)
   (check-eq? (apply + (hash-values w)) 5)))

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

(module+ test
  (define t1 (task "" (inputs) (actions) (outcomes) (deliverables) (work)))
  (test-begin
   (check-pred task? t1)
   (check-false (task-milestone t1))
   (check-equal? t1 (make-task "" '() '() '() '() (make-hash) #f)))
  (define t2 (task "Task" (inputs) (actions) (outcomes) (deliverables) (work) (milestone 'm)))
  (test-begin
   (check-not-equal? t1 t2)
   (check-equal? (task-milestone t2) 'm)
   (check-equal? (task-name t2) "Task")))

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

(module+ test
  (define grp (group "Group" t1 t2))
  (test-begin
   (check-pred group? grp)
   (check-equal? grp (make-group "Group" (list t1 t2))))
  ;; Stream interface
  (test-begin
   (check-pred stream? grp)
   (check-eq? (stream-first grp) t1)
   (check-eq? (stream-ref grp 1) t2)
   (check-eq? (stream-length grp) 2)
   (check-equal? (stream->list grp) (group-tasks grp))))

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

(module+ test
  (define prj (project "Project" grp))
  (test-begin
   (check-pred project? prj)
   (check-equal? prj (make-project "Project" (list grp))))
  ;; Stream interface
  (test-begin
   (check-pred stream? prj)
   (check-eq? (stream-first prj) grp)
   (check-eq? (stream-length prj) 1)
   (check-equal? (stream->list prj) (project-groups prj))))

(define (project-tasks-stream project)
  (for*/stream ([group project]
		[task group])
	       task))

(module+ test
  (test-begin
   (define prj-tasks-stream (project-tasks-stream prj))
   (check-pred stream? prj-tasks-stream)
   (check-eq? (stream-first prj-tasks-stream) t1)
   (check-eq? (stream-length prj-tasks-stream) 2)
   (check-true (stream-andmap task? prj-tasks-stream))))

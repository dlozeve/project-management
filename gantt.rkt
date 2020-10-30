#lang racket/base

(provide gantt)

(require racket/stream
	 "project.rkt")

(module+ test
  (require rackunit))

(define (task-max-work task)
  (apply max (hash-values (task-work task))))

(module+ test
  (define w (work 'bob 3 'alice 2))
  (define t1 (task "" (inputs) (actions) (outcomes) (deliverables) w))
  (define t2 (task "Task" (inputs) (actions) (outcomes) (deliverables) (work 'bob 4) (milestone 'm)))
  (check-eq? (task-max-work t1) 3))

(define (group-max-work group)
  (stream-fold + 0 (stream-map task-max-work group)))

(module+ test
  (define grp (group "Group" t1 t2))
  (check-eq? (group-max-work grp) (+ 3 4)))

(define (project-max-work project)
  (stream-fold + 0 (stream-map task-max-work (project-tasks-stream project))))

(module+ test
  (define prj (project "Project" grp))
  (check-eq? (project-max-work prj) (group-max-work grp)))

(define (task-start-times group group-start-time)
  (reverse (cdr (stream-fold
		 (lambda (acc t) (cons (+ (car acc) (task-max-work t)) acc))
		 (list group-start-time)
		 group))))

(module+ test
  (check-equal? (task-start-times grp 1) '(1 4)))

(define (group-start-times project)
  (reverse (cdr (stream-fold
		 (lambda (acc g) (cons (+ (car acc) (group-max-work g)) acc))
		 '(1)
		 project))))

(module+ test
  (check-equal? (group-start-times prj) '(1)))

(define (gantt project)
  (printf "\\documentclass{standalone}~n")
  (printf "\\usepackage{pgfgantt}~n")
  (printf "\\usepackage[sfdefault]{noto}~n")
  (printf "\\usepackage[T1]{fontenc}~n~n")

  (printf "\\begin{document}~n~n")

  (printf "\\begin{ganttchart}[hgrid, vgrid]{1}{~a}~n" (project-max-work project))
  (printf "  \\gantttitle{~a}{~a}\\\\~n" (project-name project) (project-max-work project))
  (printf "  \\gantttitlelist{1,...,~a}{1}\\\\~n" (project-max-work project))

  (for ([group project]
	[group-start-time (group-start-times project)]
	[i (in-naturals 1)])
    (printf "~n  \\ganttgroup[name=g~a]{Group ~a: ~a}{~a}{~a}\\\\~n"
	    i
	    i
	    (group-name group)
	    group-start-time
	    (+ group-start-time (group-max-work group) -1))
    (for ([task group]
	  [task-start-time (task-start-times group group-start-time)]
	  [j (in-naturals 1)])
      (printf "  \\ganttbar[name=t~a~a]{Task ~a.~a: ~a}{~a}{~a}\\\\~n"
	      i j
	      i j
	      (task-name task)
	      (+ task-start-time)
	      (+ task-start-time (task-max-work task) -1))))

  (printf "\\end{ganttchart}~n~n")
  (printf "\\end{document}~n"))

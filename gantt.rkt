#lang racket/base

(require racket/stream
	 "project.rkt")

(provide gantt)

(define (task-max-work task)
  (apply max (hash-values (task-work task))))

(define (group-max-work group)
  (stream-fold + 0 (stream-map task-max-work group)))

(define (project-max-work project)
  (stream-fold + 0 (stream-map task-max-work (project-tasks-stream project))))

(define (task-start-times group group-start-time)
  (reverse (cdr (stream-fold
		  (lambda (acc t) (cons (+ (car acc) (task-max-work t)) acc))
		  (list group-start-time)
		  group))))

(define (group-start-times project)
  (reverse (cdr (stream-fold
		 (lambda (acc g) (cons (+ (car acc) (group-max-work g)) acc))
		 '(1)
		 project))))

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

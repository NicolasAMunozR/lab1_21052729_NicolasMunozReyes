#lang racket

(define (system name)(make-system (cons name null) null null null))

(define (make-system name drive user carpetas) (list name drive user carpetas))

(define get-name car)

(define get-drive cadr)

(define get-user caddr)

(define get-carpetas cadddr)
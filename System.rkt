#lang racket

(define (system name)(make-system (cons name null) null null))

(define (make-system name drive user) (list name drive user))
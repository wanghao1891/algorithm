#!/usr/bin/petite --script

;Bubble
(define bubble
  (lambda (ls)
    (let loop ((_ls ls)
	       (_cur '())
	       (_ls-need '())
	       (_ls-tmp '())
	       (_num 0))
      (cond
       ((and (null? _ls) (not (null? _ls-tmp)))
	(display _ls-tmp) (newline) (display _cur) (newline)
	(set! _ls-need (append (cons _cur '()) _ls-need))
	(loop _ls-tmp '() _ls-need '() _num))
       ((and (null? _ls) (null? _ls-tmp))
	(display _num)
 	(append (cons _cur '()) _ls-need))
       (else ;compare list element and get a sublist that need to compare.
	;(display _pre) (newline)
	(let ((_pre _cur)
	      (_cur (car _ls)))
	  ;(display _cur) (newline)
	  (if (and (not (null? _pre)) (> _pre _cur))
	      (begin (set! _cur _pre)
		     (set! _pre (car _ls))))
	  (cond
	   ((null? _pre) (loop (cdr _ls) _cur _ls-need _ls-tmp _num))
	   (else
	    (set! _ls-tmp (append _ls-tmp (cons _pre '())))
	    (loop (cdr _ls) _cur _ls-need _ls-tmp (+ _num 1))))))))))

(display (bubble '(1 10 7 6 20 8 9 3)))

;(append '(1 2 3) '(4 5 6))

;(cons (cons '() (cons 1 '())) '())

;(null? (vector))


;Insertion
(define insertion
  (lambda (ls)
    (let loop ((input ls)
	       (sorted '()))
      (cond
       ((null? input) sorted)
       (else
	(display (car input)) (newline)
	(if (null? sorted)
	    (set! sorted (cons (car input) sorted))
	    (set! sorted (insertion-compare sorted (car input))))
	;(display sorted)
	(loop (cdr input) sorted))))))

(define insertion-compare
  (lambda (ls element)
    (cond
     ((null? ls) (list element))
     ((<= element (car ls))
      (cons element ls))
     (else
      (cons (car ls) (insertion-compare (cdr ls) element))))))

(insertion-compare '(1 2 4 5 6) 3)

(insertion-compare '(1) 10)

(insertion '(1 10 7 6 20 8 9 3))

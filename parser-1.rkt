#lang racket 
(define (derivative f dx)
	(lambda (x)
		(/ (- (f x) (f (+ x dx))) ;;The cool part is that f exists only in the lexical scope of this lambda
		   dx)))


(define-syntax (my-let stx)
	(syntax-case stx (my-let)
		[(my-let ([id expr] ...) bdy)
			#'((lambda (id ...) bdy) expr ...)]))


(define-syntax (my-let* stx)
	(syntax-case stx (my-let*)
		[(my-let* ([id expr]) bdy)
			#'(my-let ([id expr]) bdy)]
		[(my-let* ([id expr] rst ...) bdy)
			#' (my-let ([id expr]) (my-let* (rst ...) bdy))]))


(define (merge-creator le)
	(define (merge L1 L2)
		(cond
			[(empty? L1) L2]
			[(empty? L2) L1]
			[(le (first L1) (first L2)) (cons (first L1) (merge (rest L1) L2))]
			[else (cons (first L2) (merge L1 (rest L2)))]))

	merge)


(define (my-foldl f acc L)
	(cond
		[(empty? L) acc]
		[else (my-foldl (f (first L) acc) (rest L))]))

(define (my-reverse L)
	(foldl cons empty L))


(define (my-naive-reverse L)
	(define (rev-acc acc L)
		(cond
			[(empty? L) acc]
			[else (rev-acc (cons (first L) acc) (rest L))]))

	(rev-acc empty L))

(define (my-length L)
	(define (my-length-acc L acc)
		(cond
			[(empty? L) acc]
			[else (my-length-acc (rest L) (+ acc 1))]))
	(my-length-acc L 0))

(define (split L)
	(define (split-acc L len n acc)
		(cond
			[(>= n (/ len 2)) (cons (my-reverse acc) (cons L empty))]
			[else (split-acc (rest L) len (+ n 1) (cons (first L) acc))]))

	(split-acc L (my-length L) 0 empty))

(define my-merge (merge-creator <=))

(define (my-mergesort L)
	(cond
		[(empty? L) L]
		[(empty? (rest L)) L]
		[else
			(my-let* ([pair (split L)]
				  [L1 (first  pair)]
				  [L2 (second pair)])
				(my-merge (my-mergesort L1) (my-mergesort L2)))]))


(my-mergesort '(1999 1912 2003 2001 2014 2055))

;;my-foldr : (X Y -> Y) X (listof X) -> Y
(define (my-foldr f end L)
	(cond
		[(empty? L) end]
		[else (f (first L) (my-foldr f end (rest L)))]))


;;This sucker is gonna be hard...
(define-syntax (my-for/list stx) 
	(syntax-case stx (my-for/list)
		[(my-for/list [(var lst) ...] bdy)
			(with-syntax ([f (gensym)]
				      [acc (gensym)]
				      [(params ...) (generate-temporaries #'(lst ...))])
				#`(begin (define (f params ... acc)
						(cond 
							[(empty? params) (reverse acc)] ...
							[else 
								(f (rest params) ... (cons (let ([var (first params)] ...) bdy) acc))]))
					(f lst ... empty)))]))


(my-for/list ([i (range 1 10)]
	      [x (range 1 7)])
	(+ (* i i) x))

;;DrBracket

;;gcd : Number Number -> Number
(define (gcd x y)
	(cond
		[(or (= y 0) (= x 0)) (+ x y)]
		[(<= x y) (gcd x (remainder y x))]
		;; x > y
		[else (gcd y (remainder x y))])) 

#;(define-syntax (my-for/list stx)
	(syntax-case stx (my-for/list)
		[(my-for/list ([var lst] ...  body-or-break ... bdy))
			(with-syntax 
				([f (gensym)]
				 [acc (gensym)]
				 [(params1 ... params2 ... params3 ...) (generate-temporaries #'(lst1 ... lst2 ... lst3 ...))])
				#'(begin 
					(define (f params ... acc)
						(cond
							[(empty? params) (reverse acc)] ...
							[else
								(let ([var1 (first params1)] ...)
									(my-for/list rst ... 	
									))]))
					(f lst1 ... lst2 ... lst3 ... empty)))]))

(define (get-first obj)
	(cond
		[(list? obj) first]
		[(string? obj) (lambda (s) (first (string->list s)))]
		[else (error)]))

(define-syntax (my-for stx)
	(syntax-case stx (my-for)
		[(my-for [(var lst) ...] bdy)
			(with-syntax
				([(params ...) (generate-temporaries #'(lst ...))])
			   #'(begin
				(define (f params ...)
					(cond
						[(empty? params) (void)] ...
						[else
							(begin
								(let ([var (first params)] ...) bdy)
								(f (rest params) ...))]))

				(f lst ...)))]))

(my-for
	([i (range 1 10)])
	(printf "~a~n" (* i i)))


(define-syntax (my-for/and stx)
	(syntax-case stx (my-for/and)
		[(my-for/and ([var lst] ...) bdy)
			(with-syntax
				([(params ...) (generate-temporaries #'(var ...))])
			  #'(begin
				(define (f params ...)
					(cond
						[(empty? params) #t] ...
						[else
							(and (let ([var (first params)] ...) bdy) (f (rest params) ...))]))
				(f lst ...)))]))

(define-syntax (my-for/or stx)
	(syntax-case stx (my-for/or)
		[(my-for/or ([var lst] ...) bdy)
			(with-syntax
				([(params ...) (generate-temporaries #'(var ...))])
			  #'(begin
				(define (f params ...)
					(cond 
						[(empty? params) #f] ...
						[else
							(or (let ([var (first params)] ...) bdy) (f (rest params) ...))]))
				(f lst ...)))]))


;;First attempt at a parser. 
;;Almost definitely does not work properly because any symbol that derives empty string is SOL.
(define-syntax (my-parse-1 stx)
	(syntax-case stx (my-parse-1 =>)
		[(my-parse-1 S ([A => symbs ...] ...))
		 (with-syntax

			;;No idea if this works... It's cool if it does, though...
			;;Note to self: Don't use ellipses in comments. It's confusing.

			([((rev-symbs ...) ...) (map reverse (map syntax->list (syntax->list #'((symbs ...) ...))))]
			 [((rsts ...) ...) (map generate-temporaries (syntax->list #'((symbs ...) ...)))]) 
		  #'(lambda (lst)
			(define	(f stk lst)
				(cond
				 	[(empty? lst) (if (equal? (first (first stk)) S) (first lst) (error "Parsing Error" lst stk))]
					[(and (not (empty? stk)) (equal? (first (first stk)) S)) (first stk) ]
					[else
						(match stk
							;;problem: b doesn't represent the "rest" of the list
							;;Probably fixed now
							[(list-rest (list rev-symbs rsts) ... b) (f (cons (list A (list (list rev-symbs rsts) ...)) b) lst)] ...
							[_ (f (cons (first lst) stk) (rest lst))])]))
			(f empty lst)))]))	

(define parser-1 (my-parse-1
			'S
			(['S => 'B 'eof]
                         ['A => 'w 'B 'y]
			 ['B => 'A]
			 ['A => 'x])))    

(parser-1 (list (list 'w empty) (list 'x empty) (list 'y empty) (list 'eof empty) (list empty empty)))

;;This call is successful

;;I think this is a working LR(0) parser generator
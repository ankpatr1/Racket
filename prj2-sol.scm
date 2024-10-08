;; Name:		Ankita Patra
;; B-Number:	B01101280
;; Email:		apatra@binghamton.edu

#!/usr/bin/env racket

;; comment out following line to load and run in repl
;; MAKE SURE YOU UNCOMMENT BEFORE SUBMISSION.
#lang racket

(require rackunit)

;; to trace function fn, add (trace fn) after fn's definition
(require racket/trace)  

;; *RESTRICTIONS*:
;; YOU WILL RECEIVE A ZERO FOR THIS PROJECT IF YOU VIOLATE THE FOLLOWING
;; RESTRICTIONS:
;;
;; You may not define any top-level auxiliary functions unless
;; mentioned otherwise.  Hence all auxiliary functions must be
;; defined within the specified functions.
;;
;; You may not use the Scheme function 'define except at the top-level.
;;
;; You may not use any of Scheme's destructive functions or iteration.
;;
;; You may only use the built-in Scheme functions mentioned in
;; class or in this project, including the following:
;;   The arithmetic operators +, -, *, /.
;;   The relational operators on numbers: <, >, <=, >= and =.
;;   The exponentiation function (expt a n) = a^n; (expt 2 5) = 32.
;;   The equality function 'eqv? to check primitve equality.
;;   The member and memq functions to check for list membership.
;;   The predicates 'even? and 'odd?.
;; 

;; *TESTS*:
;; Initially, calls to all the test functions have been commented out.
;; Activate each call as you implement the corresponding function.
;; When submitting your project, you should activate all tests
;; unless your implementation for a particular test is going into
;; an infinite loop.  Submitting such a test will cause subsequent
;; tests to be skipped.  If you have such a test, please comment out that test
;; when submitting and add a comment to that effect.


;; #1: "5-points"
;; Given some scheme expression e (count-pairs e) should return
;; the # of pairs in e.

;; #1:count-pairs
(define (count-pairs e)
  (cond 
      [(null? e) 0]                           ; Check for empty list first
      [(not (pair? e)) 0]                   ; If not a pair, return 0
    [else (+ 1 
             (count-pairs (car e))       ; Count the current pair
             (count-pairs (cdr e)))]))

(define (test-count-pairs)
  (check-equal? (count-pairs 'a) 0)
  (check-equal? (count-pairs '()) 0)
  (check-equal? (count-pairs '(a . b)) 1)
  (check-equal? (count-pairs '(a b)) 2)
  (check-equal? (count-pairs '((a) b)) 3)
  (check-equal? (count-pairs '((a) . b)) 2)
  (check-equal? (count-pairs '((a) (b))) 4)
  (check-equal? (count-pairs '((a) (b . (c)) (b))) 7))


(test-count-pairs)

;; #2: "5-points"
;; (tetrate a h) should return the tetration a^^h
;; i.e. a tower of powers of a having height h
;; with exponentiation associating to the right.
;; (see <https://en.wikipedia.org/wiki/Tetration>)
(define (tetrate a h)
  (if (= h 0)
      1
      (expt a (tetrate a (- h 1)))))

(define (test-tetrate)
  (check-equal? (tetrate 1 8) 1)
  (check-equal? (tetrate 2 0) 1)
  (check-equal? (tetrate 2 1) 2)
  (check-equal? (tetrate 2 2) 4)
  (check-equal? (tetrate 2 3) 16)
  (check-equal? (tetrate 2 4) 65536)
  (check-equal? (tetrate 3 2) 27)
  (check-equal? (tetrate 3 3) (expt 3 27)))

(test-tetrate)

;; #3: "5-points"
;; (expt-list list): Given list of integers (n1 n2 n3 ...) return
;; (expt n1 (expt n2 (expt n3 ... ) )).  If the list is empty,
;; return 1.
;; *Restriction*: you may not use recursion directly.
(define (expt-list lst)
  (if (null? lst)
      1  ;; Return 1 if the list is empty
      (foldr (lambda (x y) (expt x y)) 1 lst)))  ;; Use foldr to calculate the exponentiation


(define (test-expt-list)
  (check-equal? (expt-list '()) 1)
  (check-equal? (expt-list '(3 2 2)) 81)
  (check-equal? (expt-list '(2 2 2 2)) 65536)
  (check-equal? (expt-list '(3 3 2)) 19683)
  (check-equal? (expt-list '(3 3 3)) (expt 3 27))
)

(test-expt-list)

;; #4: "10-points"
;; (series n) should return the first n-terms of the series
;; 1/(i^i) where ^ denotes exponentiation.
;;
;; *Hints*: Dividing two integers results in a rational number;
;; i.e. (/ 2 13) results in the rational number literal 2/13.
(define (series n)
  (if (= n 0)
      '()
       (build-list n (lambda (i) (/ 1 (expt (add1 i) (add1 i )))))))

(define (test-series)
  (check-equal? (series 0) '())
  (check-equal? (series 1) '(1/1))
  (check-equal? (series 2) '(1/1 1/4))
  (check-equal? (series 5) '(1/1 1/4 1/27 1/256 1/3125))
)

(test-series)


;; #5: 15-points
;;Given list indexes containing 0-based indexes and a list possibly
;;containing lists nested to an abitrary depth, return the element
;;in list indexed successively by indexes. Return 'nil if there is
;;no such element.
(define (list-access indexes list)
 (if (or (null? indexes) (null? list))
    (if (null? indexes) list 'nil)
    (let ([index (car indexes)])
      (if (>= index (length list))
        'nil
        (list-access (cdr indexes) (list-ref list index))))))

(define (test-list-access)
  (check-equal? (list-access '(1) '(a b c)) 'b)
  (check-equal? (list-access '(2) '(a b (c))) '(c))
  (check-equal? (list-access '(2 0) '(a b (c))) 'c)
  (check-equal? (list-access '(3) '(a b (c))) 'nil)
  (check-equal? (list-access '(2 1) '(a b (c))) 'nil)
  (check-equal? (list-access '() '((1 2 3) (4 (5 6 (8)))) )
		'((1 2 3) (4 (5 6 (8)))))
  (check-equal? (list-access '(1) '((1 2 3) (4 (5 6 (8)))) )
		'(4 (5 6 (8))))
  (check-equal? (list-access '( 1 1 2) '((1 2 3) (4 (5 6 (8)))) )
		'(8))
  (check-equal? (list-access '( 1 1 2 0) '((1 2 3) (4 (5 6 (8)))) )
		'8)
  (check-equal? (list-access '(0 1) '((1))) 'nil)
)
(test-list-access)

;; #6: 5-points
;; (hailstones h) should return a list containing hailstone numbers
;; starting with h.  Specifically, if h is a hailstone number,
;; then:
;;    If h is 1, the sequence terminates.
;;    If h is even, then the next hailstone number is h/2.
;;    If h is an odd number > 1, then the next hailstone number is 3*h + 1.
;; See <https://en.wikipedia.org/wiki/Collatz_conjecture> and
;; <http://xkcd.com/710>.
(define (hailstones h)
 (if (= h 1)
  (list 1)
  (cons h (hailstones (if (even? h) (/ h 2) (+ (* 3 h) 1))))))

(define (test-hailstones)
  (check-equal? (hailstones 1) '(1))
  (check-equal? (hailstones 2) '(2 1))
  (check-equal? (hailstones 3) '(3 10 5 16 8 4 2 1))
  (check-equal? (hailstones 4) '(4 2 1))
  (check-equal? (hailstones 7) '(7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))
  (check-equal? (length (hailstones 27)) 112)
)
(test-hailstones)

;; #7 "10-points"
;;
;; (max-hailstones-seed limit) should return a 2-element list such
;; that the first element is a seed which has the longest hailstone
;; sequence starting with seed <= limit and the second element is
;; the length of that sequence.  If there are multiple max-length
;; hailstone sequences having the same length, then the first element
;; should be set to the smallest seed.
;; Your implementation may call the hailstones function implemented
;; in the previous exercise.
(define (max-hailstones limit)
 (define (hailstones-length n)
  (length (hailstones n)))
  (define (find-max current-max current-length i)
    (if (> i limit)
      (list current-max current-length)
      (let ([new-length (hailstones-length i)])
        (if (> new-length current-length)
          (find-max i new-length (+ i 1))
          (find-max current-max current-length (+ i 1))))))
  (find-max 1 1 1))


(define (test-max-hailstones)
  (check-equal? (max-hailstones 1) '(1 1))
  (check-equal? (max-hailstones 4) '(3 8))
  (check-equal? (max-hailstones 10) '(9 20))
  (check-equal? (max-hailstones 100) '(97 119))
  (check-equal? (max-hailstones 1000) '(871 179))
  (check-equal? (max-hailstones 10000) '(6171 262))
  (check-equal? (max-hailstones 100000) '(77031 351))
)
(test-max-hailstones)

;; #8: 20-points
;; given an integer or list of nested lists containing integers,
;; return a string containing its JSON representation without any
;; whitespace
;; *Hints*: use (number->string n) to convert integer n to a string.
;;          use (string-append str1 str2 ...) to append str1 str2 ...
;;          use (string-join str-list sep) to join strings in str-list using sep
;;          see toJson() methods in java-no-deps Parser.java in prj1-sol.
(define (int-list-json int-list)
 (cond
  [(number? int-list) (number->string int-list)]
  [(null? int-list) "[]"]
  [else
    (string-append
    "["
    (string-join
    (map (lambda (item)
      (if (list? item)
        (int-list-json item)
        (number->string item)))
      int-list)
       ",")
      "]")]))


(define (test-int-list-json)
  (check-equal? (int-list-json '(1 2 3)) "[1,2,3]")
  (check-equal? (int-list-json '(1 (2 (4 5) 6))) "[1,[2,[4,5],6]]")
  (check-equal? (int-list-json '()) "[]")
  (check-equal? (int-list-json 42) "42")
)
(test-int-list-json)

;; #9: 25-points
;; A Scheme regex is defined as follows (an atom represents a symbol or number):
;;
;;   + An atom is a regex which matches itself.
;;
;;   + (class atom...) represents a "character-class" which matches
;;     any one of the atom... arguments (similar to [abc...] in
;;     normal regex notation).
;;
;;   + (alt re...) is a regex which matches iff any of the re... match
;;     (similar to re1 | re2 | ... | reN in normal regex notation).
;;
;;   + (* re) is a regex which matches iff zero-or-more occurrences of re match
;;     (similar to re* in normal regex notation).
;;     You may assume that re is not itself a (* _) closure regex.
;;
;;   + (re...) is a regex which matches iff the sequence of
;;     re... match (similar to the concatenation regex re1 re2 ... reN
;;     in regular regex notation).
;; 
;; (re-match re atoms): return #t iff re matches the complete list atoms,
;; #f otherwise.
;;
;; You may define auxiliary functions at the top-level for this exercise.
;;
;; *Hints*: see project assignment.

(define (re-match re atoms)
 (define (match-atom re atoms)
  (and (not (null? atoms)) (eqv? re (car atoms)) (cdr atoms)))
  (define (match-class class atoms)
   (and (not (null? atoms)) (memq (car atoms) class) (cdr atoms)))
  (define (match-alt alts atoms)
    (ormap (lambda (alt) (match-re alt atoms)) alts))
  (define (match-star re atoms)
    (let loop ((remaining atoms))
      (or (match-seq (cdr re) remaining)
        (and (not (null? remaining))
          (let ((next (match-re (car re) remaining)))
            (and next (loop next)))))))
        
  (define (match-seq res atoms)
    (if (null? res)
      atoms
      (let ((next (match-re (car res) atoms)))
        (and next (match-seq (cdr res) next)))))
  (define (match-re re atoms)
    (cond
     [(symbol? re) (match-atom re atoms)]
      [(pair? re)
       (case (car re)
        [(class) (match-class (cdr re) atoms)]
        [(alt) (match-alt (cdr re) atoms)]
        [(*) (match-star re atoms)]
        [else (match-seq re atoms)])]
      [else #f]))
  (and (match-re re atoms) (null? (match-re re atoms))))


	     
(define (test-re-match)
  
  ;; atom tests
  ;; /a/ matches "a"
  (check-equal? (re-match 'a '(a)) #t)
  ;; /b/ does not match "a"
  (check-equal? (re-match 'b '(a)) #f)
  ;; /a/ does not match ""
  (check-equal? (re-match 'a '()) #f)

  ;; concatenation tests
  ;; /bab/ matches "bab"
  (check-equal? (re-match '(b a b) '(b a b)) #t)
  ;; the concatenation regex can be nested
  (check-equal? (re-match '((b a b)) '(b a b)) #t)
  ;; the concatenation regex can be nested
  (check-equal? (re-match '((b a) (b)) '(b a b)) #t)
  ;; /ab/ does not match "bab"
  (check-equal? (re-match '(a b) '(b a b)) #f)

  ;; character class tests
  ;; /[abc]x/ matches "bx"
  (check-equal? (re-match '((class a b c) x) '(b x)) #t)
  ;; /[abc]x/ matches "cx"
  (check-equal? (re-match '((class a b c) x) '(c x)) #t)
  ;; /[abc]x/ does not match "dx"
  (check-equal? (re-match '((class a b c) x) '(d x)) #f)
  ;; /[abc]/ does not match ""
  (check-equal? (re-match '(class a b c) '()) #f)

  ;; closure tests
  ;; /a*/ matches "aa"
  (check-equal? (re-match '(* a) '(a a)) #t)
  ;; /a*/ matches ""
  (check-equal? (re-match '(* a) '()) #t)
  ;; /a*/ does not match "aab"
  (check-equal? (re-match '(* a) '(a a b)) #f)
  ;; /a*b/ matches "aab"
  (check-equal? (re-match '((* a) b) '(a a b)) #t)
  ;; /a*b*/ matches "aab"
  (check-equal? (re-match '((* a) (* b)) '(a a b)) #t)
  ;; /a*b*/ matches "aa"
  (check-equal? (re-match '((* a) (* b)) '(a a)) #t)
  ;; /(ab)*(ba)*/ matches "abab"
  (check-equal? (re-match '((* (a b)) (* (b a))) '(a b a b)) #t)
  ;; /(ab)*(ba)*/ matches "ababbababa"
  (check-equal? (re-match '((* (a b)) (* (b a))) '(a b a b b a b a b a)) #t)
  ;; /(ab)*(ba)*/ matches "bababa"
  (check-equal? (re-match '((* (a b)) (* (b a))) '(b a b a b a)) #t)
  ;; /(ab)*(ba)*/ does not match "ababbababab"
  (check-equal? (re-match '((* (a b)) (* (b a))) '(a b a b b a b a b a b)) #f)

  ;; alternation tests
  ;; /a|b/ matches "a"
  (check-equal? (re-match '(alt a b) '(a)) #t)
  ;; /a|b/ matches "b"
  (check-equal? (re-match '(alt a b) '(b)) #t)
  ;; /aa*|bb*" matches "aaa"
  (check-equal? (re-match '(alt (a (* a)) (b (* b))) '(a a a)) #t)
  ;; /aa*|bb*" matches "bbb"
  (check-equal? (re-match '(alt (a (* a)) (b (* b))) '(b b b)) #t)
  ;; /aa*|bb*" dose not match "baaba"
  (check-equal? (re-match '(alt (a (* a)) (b (* b))) '(b a a b a)) #f)
  ;; /(a|b)*/ matches "baabbba"
  (check-equal? (re-match '(* (alt a b)) '(b a a b b b a)) #t)

  ;; backtracking tests
  ;; /a*aab/ matches "aaaab"
  (check-equal? (re-match '((* a) a a b) '(a a a a b)) #t)
  ;; /a*aab/ matches "aab"
  (check-equal? (re-match '((* a) a a b) '(a a b)) #t)
  ;; /a*aab/ does not match "ab"
  (check-equal? (re-match '((* a) a a b x) '(a b)) #f)
  ;; /(aab|aa)b/ matches "aab"
  (check-equal? (re-match '((alt (a a b) (a a)) b) '(a a b)) #t)
  ;; /(aab|a)b/ does not match "aab"
  (check-equal? (re-match '((alt (a a b) (a)) b) '(a a b)) #f)
  
  ;; combination tests
  ;; /[xyz]a*/ matches "yaaa"
  (check-equal? (re-match '((class x y z) (* a)) '(y a a a)) #t)
  ;; /[xyz]a*/ matches "zaaa"
  (check-equal? (re-match '((class x y z) (* a)) '(z a a a)) #t)
  ;; /[xyz]a*/ does not match "aaa"
  (check-equal? (re-match '((class x y z) (* a)) '(a a a)) #f)
  ;; /[xyz]a*/ does not match ""
  (check-equal? (re-match '((class x y z) (* a)) '()) #f)
  ;; /([abc]|[def])*x*/ matches "aecdfxx"
  (check-equal? (re-match '((* (alt (class a b c) (class d e f))) (* x))
			    '(a e c d f x x)) #t)
  ;; /([abc]|(d|e|f))*x*/ matches "aecdfxx"
  (check-equal? (re-match '((* (alt (class a b c) (alt d e f))) (* x))
			    '(a e c d f x x)) #t)
  ;; /([abc]|[def])*x*/ matches ""
  (check-equal? (re-match '((* (alt (class a b c) (class d e f))) (* x)) 
			    '()) #t)
  ;; /([abc]|[def])*x*/ does not match "aecdfxxa"
  (check-equal? (re-match '((* (alt (class a b c) (class d e f))) (* x))
			    '(a e c d f x x a)) #f)


)
;(test-re-match)

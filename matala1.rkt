#lang pl


#|Q1|#

#| Q1 a 
define a recursive function open-list that
consumes a list of lists and returns a list contains all the elements of the
inner lists concatenated in the same order |#

(: open-list : (Listof(Listof Number)) -> (Listof Number))
(define (open-list list)
  (if (null? list) null
      (append (first list) (open-list (rest list)))))

(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11
90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90))

(test (open-list '()) => '())

(test (open-list '(()())) => '())

(test (open-list '((0)())) => '(0))

(test (open-list '((0)(0))) => '(0 0))

(test (open-list '((1 2 3))) => '(1 2 3))


#| Q1 b
Define a function min&max that consumes a list of lists and returns a list
containing the minimum and the maximum of the values in the inner
lists 
|#

;; this func will give us the min value from a list of numbers using min func
(: list-min : (Listof Number) -> Number)
(define (list-min list)
  (if (null? list) +inf.0 (min (first list) (list-min (rest list)))))

(test (list-min '()) => +inf.0)

(test (list-min '(-1)) => -1.0)

(test (list-min '(-1 3 -99 5)) => -99.0)

;; this func will give us the max value from a list of numbers using max func
(: list-max : (Listof Number) -> Number)
(define (list-max list)
  (if (null? list) -inf.0 (max (first list) (list-max (rest list)))))

(test (list-max '()) => -inf.0)

(test (list-max '(-1)) => -1.0)

(test (list-max '(-1 3 -99 5)) => 5.0)

#| this func will give us a list with 2 value 1.min_num in all the lists 2.max_num in all the lists
 | using the open-list func to create one list from all the lists
 | using list_min func to find the min_num from the list
 | using list_max func to find the max_num from the list
 | and list func to make a list of those too numbers|# 
(: min&max : (Listof(Listof Number)) -> (Listof Number))
(define (min&max lists)
  (if (null? (open-list lists)) null
      (list(list-min (open-list lists)) (list-max (open-list lists)))))

(test (min&max '()) => '())

(test(min&max '(()())) => '())

(test(min&max '((1)())) => '(1.0 1.0))

(test(min&max '((1)(0))) => '(0.0 1.0))

(test(min&max '((0)(1))) => '(0.0 1.0))

(test(min&max '((0 1))) => '(0.0 1.0))

(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11
90))) => '(-1.0 233.0))

#| Q1 c
Write a function min&max_apply that does exactly what you did in
part “b” but using apply function|#

#| this func will give us a list with 2 value 1.min_num in all the lists 2.max_num in all the lists
 | using the open-list func to create one list from all the lists
 | using min func to find the min_num from the list
 | using max func to find the max_num from the list
 | apply func to covort the numbers from double to int (0.0 =>)|
 | list func to make a list of those too numbers|# 
(: min&max_apply : (Listof(Listof Number)) -> (Listof Number))
(define (min&max_apply lists)
  (if (null? (open-list lists)) null
      (list (apply min(open-list lists)) (apply max(open-list lists)))))

(test (min&max_apply '()) => '())

(test(min&max_apply '(()())) => '())

(test(min&max_apply '((1)())) => '(1 1))

(test(min&max_apply '((1)(0))) => '(0 1))

(test(min&max_apply '((0)(1))) => '(0 1))

(test(min&max_apply '((0 1))) => '(0 1))

(test (min&max_apply '((1 2 3) (2 3 3 4) (9 2 -1) (233 11
90))) => '(-1 233))

#|Q2|#

#| Q2.1
Implement the empty table EmptyTbl – this should be a variant of the
data type (constructor)|#

#|

(define-type table
  [EmptyTbl])

|#

#| Q2.2
Implement the add operation Add – this too should be a variant of the
data type.
The add operation should take as input a symbol (key), a string
(value), and an existing table and return an extended table in the natural
way|#

(define-type table
[EmptyTbl]
[Add Symbol String table])


(test (EmptyTbl) => (EmptyTbl)) 

(test (Add 'a "A" (EmptyTbl)) => (Add 'a "A" (EmptyTbl)))

(test (Add 'b "B" (Add 'a "A" (EmptyTbl))) =>
 (Add 'b "B" (Add 'a "A" (EmptyTbl))))

(test (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) =>
 (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))

#| Q2.3
Implement the search operation search-table – the search operation
should take as input a symbol (key) and a table and return the first (LIFO,
last in first out) value that is keyed accordingly – see examples below|#


(: search-table : Symbol table -> (U String #f))
(define (search-table Symbol table)
  (cases table
    [(Add Sbl Str NextTbl)
     (if(eq? Symbol Sbl) Str
        (search-table Symbol NextTbl))]
    [(EmptyTbl) #f]))


(test (search-table 'a (EmptyTbl)) => #f) 

(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'a "A"
(EmptyTbl)))))
=> #f)

(test (search-table 'a (Add 'a "AAA" (Add 'b "B" (Add 'a "A"
(EmptyTbl)))))
=> "AAA")

#| Q2.4
Implement the remove item operation remove-item – the remove item
operation should take as input a table and a symbol (key) and return a
new table contains the items of the original table except of the item
to be deleted without the (first (LIFO) keyed value) |#

(: remove-item : table Symbol -> table)
(define (remove-item table Symbol)
  (cases table
    [(Add Sbl Str NextTbl)
     (cond[(eq? Symbol Sbl) NextTbl]
          [else(Add Sbl Str (remove-item NextTbl Symbol))])]
    [(EmptyTbl) (EmptyTbl)]))


(test (remove-item (EmptyTbl) 'a) => (EmptyTbl))

(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A"
(EmptyTbl)))) 'a)
=> (Add 'b "B" (Add 'a "A" (EmptyTbl))))

(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A"
(EmptyTbl)))) 'b)
=> (Add 'a "AAA" (Add 'a "A" (EmptyTbl))))
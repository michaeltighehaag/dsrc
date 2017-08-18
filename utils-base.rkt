#lang racket

[provide [all-defined-out]]

;*******************************************************************************************
[define cat string-append]
;*******************************************************************************************

;;;
[define [nth-tsv n] [lambda [x] [list-ref [regexp-match* #rx"[^\t\n]*" x] [* 2 n]]]]

[define [n<-f f] [string->number f]]

[define [rcomp p m n]  [lambda [r1 r2] [p [m [[nth-tsv n] r1]]
                                          [m [[nth-tsv n] r2]]]]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;*******************************************************************************************
;making padded numeric strings
[define [pad n d [r 10]]
  [let [[s [number->string n r]]]
    [cat [make-string [+ d [- [string-length s]]] #\0] s ]]]

[define [str-pad s d [c #\0]]
  [cat [make-string [+ d [- [string-length s]]] c] s ]]
    
;returns a counter function that sequentially returns handy padded integers 
[define [make-seq-counter [start 0] [digits 10] [radix 10]]
  [let [[count start]] 
    [lambda [] [set! count [+ count 1]] [pad count digits radix] ]]]
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*******************************************************************************************
;[enum-trees [list "a" "b" "c"]]
[define [enum-trees labels]
  [if [equal? [length labels] 1]
       labels
      [for*/list [[i [in-range 1 [length labels]]]
                  [left [in-list [enum-trees [take labels i]]]]
                  [right [in-list [enum-trees [drop labels i]]]]]
       [list left right]]]]

;[uni-comb 2 [list "a" "b" "c"]] m unique choices from list
[define [uni-comb m lst]
  [if [equal? m 0]
    [list [list]]
    [if [null? lst] '[]
      [append [map [lambda [y] [cons [car lst] y]]
                   [uni-comb [- m 1] [cdr lst]]]
              [uni-comb m [cdr lst]]]]]]

;[all-comb 2 [list "a" "b" "c"]] all perms with duplicates
[define [all-comb d v-list]
  [for/fold [[ret-list [list [list]]]]
            [[n [in-range d]]]
    [apply append 
      [map [lambda [x] 
        [for/list [[f v-list]] 
           [cons f x]]] ret-list]]]]

;[all-perm [list 1 2 3]]
[define [all-perm l]
  [if [> 2 [length l]]
    [list l] 
    [apply append 
      [map [lambda [x] 
             [for/list [[i [in-range 0 [length  l]]]]
               [append [take x i] [list [car l]] [drop x i]]]]
           [all-perm [cdr l]]]]]]

;*******************************************************************************************
;********************************************************************************************


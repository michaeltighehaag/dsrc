#! /usr/bin/racket
#lang racket
[require racket/require
  [path-up "dsrc/utils-base.rkt"]]
[require bitsyntax]
[require racket/performance-hint]

[provide [all-defined-out]]

[define [int->bv v s] [integer->bit-string v s #t]]
[define [char-str->bv s]
  [let [[sl [string-length s]]]
    [int->bv [if [equal? sl 0] 0 [string->number s 2]] sl]]]
[define-inline [bv->char-str bs] 
  [let [[bsl [bit-string-length bs]]]
    [if [equal? bsl 0] "" [pad [bit-string->unsigned-integer bs #t] bsl 2]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[struct fbrt [bits count aux val left right] #:transparent]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [rffd bs1 s1 bs2 s2]
  [if [and [< s1 [bit-string-length bs1]] [< s2 [bit-string-length bs2]]]
    [if [equal? [bit-string-ref bs1 s1] [bit-string-ref bs2 s2]]
      [add1 [rffd bs1 [add1 s1] bs2 [add1 s2]]]
      0 ] 0]]

[define [suvf n o] n ]
[define [snf af hb v] 
  [fbrt hb 1 [af hb v null null] v null null]] ;;add/use uf??

[define [fbrt-insert v k t af uf nf]
  [let [[kl [bit-string-length k]]]
    [if [null? t]
      [fbrt k 1 [af k v null null] [uf v null] null null] ;;v
      [car [fbrt-rec-insert af uf nf v k kl 0 t [list t]]]]]]
[define [fbrt-rec-insert af uf nf v k kl hbi h np]
  [let [[hb [fbrt-bits h]][hl [fbrt-left h]][hr [fbrt-right h]]]                  
  [let [[nki [+ hbi [rffd k hbi hb 0]]][hei [+ hbi [bit-string-length hb]]]] 
    [if [< nki hei]
      [let [[oh [fbrt [sub-bit-string hb [- nki hbi] [- hei hbi]] [fbrt-count h] [fbrt-aux h] [fbrt-val h] hl hr]]
            [nbp [sub-bit-string k hbi nki]]]
        [if [< nki kl]
          [let* [[nhb [sub-bit-string k nki kl]][nh [nf af nhb v]]]
            [if [equal? 0 [bit-string-ref k nki]]
                  [rec-update [fbrt nbp [add1 [fbrt-count h]] [af nbp null nh oh] null nh oh][cdr np]af nh null]  
                  [rec-update [fbrt nbp [add1 [fbrt-count h]] [af nbp null oh nh] null oh nh][cdr np]af nh null]] ] 
          [if [equal? 0 [bit-string-ref hb [- nki hbi]]]
            [rec-update [fbrt nbp [add1 [fbrt-count h]] [af nbp v oh null] [uf v null] oh null][cdr np]af null null] ;;v
            [rec-update [fbrt nbp [add1 [fbrt-count h]] [af nbp v null oh] [uf v null] null oh][cdr np]af null null]]]] ;;v
      [let [[hv [fbrt-val h]]]
        [if [< nki kl]
          [let* [[nhb [sub-bit-string k nki kl]] [nh [nf af nhb v]]]
            [if [equal? [bit-string-ref k hei] 0]
              [if [equal? hl null]
                [rec-update [fbrt hb [+ 1 [fbrt-count h]] [af hb hv nh hr] hv nh hr] [cdr np] af nh null]
                [fbrt-rec-insert af uf nf v k kl hei hl [cons hl np]]]
              [if [equal? hr null]
                [rec-update [fbrt hb [+ 1 [fbrt-count h]] [af hb hv hl nh] hv hl nh] [cdr np] af nh null]
                [fbrt-rec-insert af uf nf v k kl hei hr [cons hr np]]]]]
            [let [[nn [fbrt hb [+ 1 [if [null? hl] 0 [fbrt-count hl]] [if [null? hr] 0 [fbrt-count hr]]] [af hb v hl hr] [uf v hv] hl hr]]];[nf af uf hb v hv hl hr]]]
              [rec-update nn [cdr np] af [cons nn null] hv]]
        ]]]]]]

[define [rec-update up-n np af p ov]
  [if [null? np] [list up-n [cons up-n p] ov]
    [let* [[h [car np]]
           [psum [+ [fbrt-count up-n] [if [null? [fbrt-val h]] 0 1]]]]
      [rec-update 
        [if [equal? [bit-string-ref [fbrt-bits up-n] 0] 0]
          [fbrt [fbrt-bits h]
                [+ psum [if [null? [fbrt-right h]] 0 [fbrt-count [fbrt-right h]]]]
                [af [fbrt-bits h] [fbrt-val h] up-n [fbrt-right h]] [fbrt-val h] up-n [fbrt-right h]] 
          [fbrt [fbrt-bits h]
                [+ psum [if [null? [fbrt-left h]] 0 [fbrt-count [fbrt-left h]]]]
                [af [fbrt-bits h] [fbrt-val h] [fbrt-left h] up-n] [fbrt-val h] [fbrt-left h] up-n]]  
        [cdr np] af [cons up-n p] ov]]]]

[define [fbrt-get-path k t]
  [fbrt-rec-get-path k [bit-string-length k] 0 [fbrt-bits t] [fbrt-left t][fbrt-right t][list t][list 0]]]
[define [fbrt-rec-get-path k kl hbi hb hl hr np inp]
  [let [[nki [+ hbi [rffd k hbi hb 0]]] [hei [+ hbi [bit-string-length hb]]]] 
    [if [< nki hei]
      [if [< nki kl] null [values np inp]]
      [if [< nki kl]
        [if [equal? [bit-string-ref k hei] 0]
          [if [equal? hl null] null [fbrt-rec-get-path k kl hei [fbrt-bits hl][fbrt-left hl][fbrt-right hl][cons hl np][cons [+ hei [bit-string-length [fbrt-bits hl]]] inp]]]
          [if [equal? hr null] null [fbrt-rec-get-path k kl hei [fbrt-bits hr][fbrt-left hr][fbrt-right hr][cons hr np][cons [+ hei [bit-string-length [fbrt-bits hr]]] inp]]]]
        [values np inp]]]]]

[define [fbrt-get-val k t]
  [let-values [[[np inp] [fbrt-get-path k t]]]
    [if [null? np] null
        [if [equal? [bit-string-length k] [car inp]] [fbrt-val [car np]] null]]]] 

[define [fbrt-delete k t af]
  [let-values [[[np inp] [fbrt-get-path k t]]]
    [if [null? np] t
      [let [[h [car np]]]
        [if [and [null? [fbrt-left h]] [null? [fbrt-right h]]]
           [if [null? [cdr np]]
             null
             [let [[n [cadr np]][nl [fbrt-left [cadr np]]][nr [fbrt-right [cadr np]]]]
               [if [null? [fbrt-val n]]
                 [if [equal? 0 [bit-string-ref [fbrt-bits h] 0]]
                   [car [rec-update [fbrt [bit-string-append [fbrt-bits n][fbrt-bits nr]] [fbrt-count nr] [fbrt-aux nr] [fbrt-val nr] [fbrt-left nr][fbrt-right nr]] [cddr np] af null null]]
                   [car [rec-update [fbrt [bit-string-append [fbrt-bits n][fbrt-bits nl]] [fbrt-count nl] [fbrt-aux nl] [fbrt-val nl] [fbrt-left nl][fbrt-right nl]] [cddr np] af null null]]]
                 [if [equal? 0 [bit-string-ref [fbrt-bits h] 0]]
                   [car [rec-update [fbrt [fbrt-bits n] [sub1 [fbrt-count n]] [af [fbrt-bits n] [fbrt-val h] null [fbrt-right h]] [fbrt-val h] null [fbrt-right n]] [cddr np] af null null]]
                   [car [rec-update [fbrt [fbrt-bits n] [sub1 [fbrt-count n]] [af [fbrt-bits n] [fbrt-val h] [fbrt-left h] null] [fbrt-val h] [fbrt-left n] null] [cddr np] af null null]]]
                 ]]]
           [car [rec-update [fbrt  [fbrt-bits h] [sub1 [fbrt-count h]] [af [fbrt-bits h] null [fbrt-left h][fbrt-right h]] null [fbrt-left h][fbrt-right h]][cdr np]af null null]]
        ]]]]];;needs testing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iterate node vals in skew order (ie left, internal, right);;;;;;;;;;;
[define [fbrt-skew-first t]
  [if [null? t] null
    [if [null? [fbrt-left t]] [list t] [fbrt-skew-rec-first [list t]]]]]
[define [fbrt-skew-rec-first np]
  [let [[hl [fbrt-left [car np]]]]
    [if [null? hl] np [fbrt-skew-rec-first [cons hl np]]]]]

[define-inline [fbrt-skew-last t]
  [if [null? t] null
    [if [null? [fbrt-right t]] [list t] [fbrt-skew-rec-last [list t]]]]]
[define [fbrt-skew-rec-last np]
  [let [[hr [fbrt-right [car np]]]]
    [if [null? hr] np [fbrt-skew-rec-last [cons hr np]]]]]

[define-inline [fbrt-skew-next np]
  [let [[hr [fbrt-right [car np]]]]
    [if [null? hr]
      [if [null? [cdr np]] null [fbrt-skew-next-up [cdr np] [bit-string-ref [fbrt-bits [car np]] 0]]]
      [fbrt-skew-rec-first [cons hr np]]]]]
[define [fbrt-skew-next-up np b]
  [if [null? np] null
    [let [[h [car np]]]
      [if [equal? b 0] np
        [if [null? [cdr np]] null [fbrt-skew-next-up [cdr np] [bit-string-ref [fbrt-bits h] 0]]]]]]]

[define-inline [fbrt-skew-prev np]
  [let [[hl [fbrt-left [car np]]]]
    [if [null? hl]
      [if [null? [cdr np]] null [fbrt-skew-prev-up [cdr np] [bit-string-ref [fbrt-bits [car np]] 0]]]
      [fbrt-skew-rec-last [cons hl np]]]]]
[define [fbrt-skew-prev-up np b]
  [if [null? np] null
    [let [[h [car np]]]
      [if [equal? b 1] np
        [if [null? [cdr np]] null [fbrt-skew-prev-up [cdr np] [bit-string-ref [fbrt-bits h] 0]]]]]]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;iterate node vals in lexical string order (ie internal, left, right);;;;;;;;;
[define [fbrt-lex-first t]
  [if [null? t] null [list t]]]

[define [fbrt-lex-last t]
  [if [null? t] null [fbrt-lex-rec-last [list t]]]]
[define [fbrt-lex-rec-last np]
  [let [[h [car np]]]
    [if [null? [fbrt-right h]]
      [if [null? [fbrt-left h]] np
        [fbrt-lex-rec-last [cons [fbrt-left h] np]]]
      [fbrt-lex-rec-last [cons [fbrt-right h] np]]]]]

[define-inline [fbrt-lex-next np]
  [let [[h [car np]]]
    [if [null? [fbrt-left h]]
      [if [null? [fbrt-right h]]
        [if [null? [cdr np]] null [fbrt-lex-next-up [cdr np] [bit-string-ref [fbrt-bits h] 0]]]
        [cons [fbrt-right h] np]]
      [cons [fbrt-left h] np]]]]
[define [fbrt-lex-next-up np b]
  [if [null? np] null
    [let [[h [car np]]]
      [if [equal? b 0]
        [if [null? [fbrt-right h]]
          [if [null? [cdr np]] null [fbrt-lex-next-up [cdr np] [bit-string-ref [fbrt-bits h] 0]]]
          [cons [fbrt-right h] np]]
        [if [null? [cdr np]] null [fbrt-lex-next-up [cdr np] [bit-string-ref [fbrt-bits h] 0]]]]]]]

[define-inline [fbrt-lex-prev np]
  [if [null? [cdr np]] null
    [fbrt-lex-prev-up [cdr np] [bit-string-ref [fbrt-bits [car np]] 0]]]]
[define-inline [fbrt-lex-prev-up np b]
  [let [[h [car np]]]
    [if [equal? b 1]
      [if [null? [fbrt-left h]] np
        [fbrt-lex-rec-last [cons [fbrt-left h] np]]]
        np]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[define [trav iter-f n-path f acc]
  [if [null? n-path] acc
    [trav iter-f [iter-f n-path] f [f n-path acc]]]]

[define [list-rlex t]
  [trav fbrt-lex-next [fbrt-lex-first t] dump-vals [list]]]
[define [list-lex t]
  [trav fbrt-lex-prev [fbrt-lex-last t] dump-vals [list]]]

[define [list-rskew t]
  [trav fbrt-skew-next [fbrt-skew-first t] dump-vals [list]]]
[define [list-skew t]
  [trav fbrt-skew-prev [fbrt-skew-last t] dump-vals [list]]]

[define [flist-skew t]
  [trav fbrt-skew-prev [fbrt-skew-last t] dump-path-vals [list]]]

[define [dump-vals n a] [if [null? [fbrt-val [car n]]] a [cons [fbrt-val [car n]] a]]]
[define-inline [dump-path-vals x a]
  [if [null? [fbrt-val [car x]]] a
    [cons [string-append [read-back get-bits x ""] ":"  [format "~a" [fbrt-val [car x]]] ] a]]]

[define [read-back f np str]
  [if [null? np] str [read-back f [cdr np] [string-append [f [car np]] str]]]]

[define-inline [get-bits n] [bv->char-str [fbrt-bits n]]]
[define [get-brk n] [pad 1 [bit-string-length [fbrt-bits n]]]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;Breadth first search functions;;;;;;;;;;;;;;;;;;;;;;;;;

[define [fbrt-bfs f ns testv a]
  [fbrt-rec-bfs f [list ns] [list] testv a]]

[define [fbrt-rec-bfs f wl rl testv a]
  [if [null? wl]
    [if [null? rl] #f 
      [begin [displayln "end."][fbrt-rec-bfs f [reverse rl] [list] testv a]]]
    [let* [[h [car wl]]
          [r [f h a]]]
      [if [equal? [testv r] #t] r
        [fbrt-rec-bfs f [cdr wl] [cons-c h rl] testv r]]]]]

[define-inline [cons-c h ls]
  [let [[l [fbrt-left h]]
        [r [fbrt-right h]]]
    [if [and [null? l][null? r]] ls
      [if [null? l]
        [cons r ls]
        [if [null? r]
          [cons l ls]
          [cons r [cons l ls]]]]]]]

[define [bftf h a] [begin [displayln [format "~a:~a:~a" [fbrt-count h] [get-bits h] [fbrt-val h] ]] a]]
[define [bftt r] #f]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;node insert (new nf should have wrapper that tests v= n-v)
;;bfs basic set ops / zip 
;;shamb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[define [fbrt=? x y]
  [if [and [null? x] [null? y]] #t
    [if [and [bit-string-equal? [bit-string-pack [fbrt-bits x]] [bit-string-pack [fbrt-bits x]]]
             [equal? [fbrt-count x] [fbrt-count y]]
             [equal? [fbrt-aux x] [fbrt-aux y]]
             [equal? [fbrt-val x] [fbrt-val y]]
             [fbrt=? [fbrt-left x] [fbrt-left y]]
             [fbrt=? [fbrt-right x] [fbrt-right y]]] #t #f]]]

[define [rec-in t l af]
  [if [null? l] t
    [let [[h [car l]][hl [string-length [car l]]]]
      [rec-in [fbrt-insert h [int->bv [if [equal? hl 0] 0 [string->number h 2]] hl] t af suvf snf] [cdr l] af]]]]

[define [atf hb v l r] [+ [if [null? v] 0 1] [if [null? l] 0 [fbrt-count l]] [if [null? r] 0 [fbrt-count r]]]]

[define-inline [unit-test n r]
  [let [[kl [sort
              [remove-duplicates
                [for/list [[j [in-range 0 [expt 2 n]]]]
                  [substring [pad [random [expt 2 31]] 31 2] [* r [random 8]] [- 31 [* r [random 8]]]]]]
              [lambda [x y] [< 0.5 [random]]]]]]
    [displayln "made keylist"]
    [displayln [length kl]]
    [displayln [car kl]]
    [list [time [rec-in [list] kl atf]] kl]]]

[let [[mt [unit-test 6 1]]]
  [equal? [list-lex [car mt]] [sort [cadr mt] string<?]]]

[let [[mt [unit-test 6 0]]]
  [equal? [list-lex [car mt]] [sort [cadr mt] string<?]]]

[let [[mt [unit-test 12 1]]]
  [fbrt-bfs bftf [car mt] bftt null]]

[define [rec-mk-skew-str n s]
  [if [equal? 0 [modulo n 2]]
    [rec-mk-skew-str [/ n 2] [sub1 s]]
    [bv->char-str [int->bv [/ [sub1 n] 2] [sub1 s]]]]] 

[define [make-key-str-skew<? kl]
  [lambda [x y]
    [string<? [string-append x "1" [make-string [- [sub1 kl] [string-length x]] #\0]]
              [string-append y "1" [make-string [- [sub1 kl] [string-length y]] #\0]]]]]

[define [sub-check pkl pks pt l]
  [if [null? l] [void]
    [let [[tt [rec-in [list] [car l] atf]]]
      [if [and [fbrt=? pt tt]
               [equal? pkl [list-lex tt]]
               [equal? pks [list-skew tt]]]
        [void]
        [begin [writeln "**** FAILED ****"]
               [writeln pkl][writeln [list-lex tt]][writeln pks][writeln [list-skew tt]]]]
    [sub-check pkl pks pt [cdr l]]]]]

[define [rec-check kl l]
  [if [null? l] [void]
    [let* [[llv [all-perm [car l]]]
           [llk [map [lambda [y] [map [lambda [x] [rec-mk-skew-str x kl]] y]] llv]]]
      [let* [[pk [car llk]]
             [pt [rec-in [list] pk atf]]]
        [sub-check [sort pk string<?] [sort pk [make-key-str-skew<? kl]] pt [cdr llk]]
        [rec-check kl [cdr l]]]]]]

[define [gen-test n kl]
  [let [[l [uni-comb n [cdr [build-list [expt 2 kl] values]]]]]
    [displayln [format "n=~a kl=~a length=~a" n kl [length l]]]
    [time [rec-check kl l]]]]

[for [[i [in-range 1 2]]] [gen-test i 1]] 
[for [[i [in-range 1 4]]] [gen-test i 2]]
[for [[i [in-range 1 8]]] [gen-test i 3]]
[for [[i [in-range 1 5]]] [gen-test i 4]]

; for each elem build, del, and test == elem \ t

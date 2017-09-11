#lang racket
[require racket/require
  [path-up "dsrc/utils-base.rkt"]]
[require racket/require
  [path-up "dsrc/utils-dsrc.rkt"]]
[require racket/require
  [path-up "dsrc/utils-date.rkt"]]
[require racket/require
  [path-up "dsrc/utils-ndef.rkt"]]



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;********************************************************************************************
;*** Tests ***
;***************
[define [test-strms]
  [let [[ts [stream  2 2 2 2 2 3 4 4 5 6 6 6 7 7 8 9 9 9 10 10]]
        [ts1 [stream 31 0 1 2 2 3 5 7 8 9]]
        [ts2 [stream 1 1 1 2 4 5 5 8 15]]
        [ts3 [stream 31 0 1 2 2 3 05 7 8 9 1 1 0 1 2 4 5 5 8 15 0]]]

    [displayln "num-alike"]
    [define nts [strm-number-alike equal? ts]]
    [strm-for-each displayln nts]

    [displayln "split at"]

    [let-values [[[a b][strm-split-at [lambda [x] [equal? 0 [cdr x]]] [strm-cdr nts]]]]
      [displayln "a"][strm-for-each displayln a]
      [displayln "b"][strm-for-each displayln b]]
    
    [displayln "uncated"]
    [define unts [strm-uncat [lambda [x] [equal? 0 [cdr x]]] nts]]
    [disp-strm unts]

    [displayln "chunk 6"]
    [disp-strm
      [strm-chunk 6 ts]]
 
    [disp-strm [strm-chunk 7 [strm<-cmd "ls -l"]]]
]]
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[test-strms]

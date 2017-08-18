#lang racket
[require racket/require
  [path-up "Projects/source-libs/utils-base.rkt"]]

[require racket/date]
[provide [all-from-out racket/date]]
[require [prefix-in srfi19: srfi/19]]
[provide [all-from-out srfi/19]]

[provide [all-defined-out]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*******************************************************************************************
[define [timestamp [d [current-date]]]
  [pad [string->number [srfi19:date->string d "~s~N"]] 20]]

[define [datestamp [d [current-date]]]
  [srfi19:date->string d "~Y~m~d~H~M~S~N"]]

[define [noon-seconds [cd [current-date]]]
  [find-seconds 0 0 12 [date-day cd] [date-month cd] [date-year cd]]]

[define [days-to-new-month [d [current-date]]]
    [inexact->exact [ceiling
      [* [+ [find-seconds 0 0 12 1
              [if [equal? [add1 [date-month d]] 13] 1 [add1 [date-month d]]] 
              [if [equal? 12 [date-month d]] [add1 [date-year d]] [date-year d]]]  
            [- [noon-seconds d]]]
         [/ [* 24 60 60]] 1.0]]]]

[define [same-day? d1 d2]
  [let [[d1y [date-year d1]][d1yd [date-year-day d1]]
        [d2y [date-year d2]][d2yd [date-year-day d2]]]
  [if [and [equal? d1y d2y] [equal? d1yd d2yd]] #t #f]]]

[define [date-shift-days sh d]
  [seconds->date [+ [* sh 24 60 60] [date->seconds d]]]]

[define [date-filter-m m]
  [lambda [x] [equal? m [date-month x]]]]

[define [date-filter-wd wd]
  [lambda [x] [equal? wd [date-week-day x]]]]

[define [date<? d1 d2] [< [date->seconds d1] [date->seconds d2]]] 

[define [mk-days-of-month-list [x [current-date]]]
  [let [[d [date-day x]]
        [m [date-month x]]
        [fxs [noon-seconds x]]]
    [filter [date-filter-m m]
      [for/list [[i [in-range 1 32]]]
        [let [[s [+ fxs [* [+ i [- d]] 24 60 60]]]]
          [seconds->date s]]]]]]

[define [nth-wd-of-month n wd [d [current-date]]]
  [if [< n 0]
    [list-ref [reverse [filter [date-filter-wd wd] [mk-days-of-month-list d]]] [sub1 [- n]]]
    [list-ref [filter [date-filter-wd wd] [mk-days-of-month-list d]] [sub1 n]]]]

[define [nth-wd-of-month-sh? n wd d sh]
  [let [[sd [date-shift-days sh d]]]
    [same-day? sd [nth-wd-of-month n wd sd]]]]

[define [db-ts->secs ts]
  [date->seconds
    [srfi19:string->date
      [car [regexp-match #rx"[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]" ts]]
      "~Y-~m-~d ~H:~M:~S"]]]
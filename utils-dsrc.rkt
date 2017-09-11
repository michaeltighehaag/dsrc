#lang racket
[require racket/require
  [path-up "dsrc/utils-base.rkt"]]

[provide [all-defined-out]]

[require file/tar]
[provide [all-from-out file/tar]]
[require file/untgz]
[provide [all-from-out file/untgz]]
[require sxml]
[provide [all-from-out sxml]]
[require html-parsing]
[provide [all-from-out html-parsing]]
;[require math/number-theory]
;[provide [all-from-out math/number-theory]]

[require http/request]
[require json-parsing]
;[require http]
;[require net/uri-codec]
;[require file/md5]

[require [rename-in srfi/41
    [list->stream strm<-list]
    [port->stream strm<-port]
    [stream->list list<-strm]
    [stream-null strm-null]
    [stream-cons strm-cons]
    [stream? strm?]
    [stream-null? strm-null?]
    [stream-pair? strm-pair?]
    [stream-car strm-car]
    [stream-cdr strm-cdr]
    [stream-lambda strm-lambda]
    [define-stream define-strm]
    [stream-append strm-append]
    [stream-concat strm-concat]
    [stream-constant strm-constant]
    [stream-drop strm-drop]
    [stream-drop-while strm-drop-while]
    [stream-filter strm-filter]
    [stream-fold strm-fold]
    [stream-for-each strm-for-each]
    [stream-from strm-from]
    [stream-iterate strm-iterate]
    [stream-length strm-length]
    [stream-let strm-let]
    [stream-map strm-map]
    [stream-of strm-of]
    [stream-range strm-range]
    [stream-ref strm-ref]
    [stream-reverse strm-reverse]
    [stream-scan strm-scan]
    [stream-take strm-take]
    [stream-take-while strm-take-while]
    [stream-unfold strm-unfold]
    [stream-unfolds strm-unfolds]
    [stream-zip strm-zip]
]]
[provide [all-from-out srfi/41]]



;*******************************************************************************************
;xml 
[define [get-xml str [nsl [list]]] 
  [call-with-input-file str [lambda [in] [ssax:xml->sxml in nsl]]]]

[define [get-json-sxml str] 
  [call-with-input-file str [lambda [in] [json->sxml in ]]]]

[define [get-json-sjson str] 
  [call-with-input-file str [lambda [in] [json->sjson in ]]]]

[define [str->file str fstr]
  [call-with-output-file fstr
    [lambda [out]
      [display str out]] #:exists 'replace]]

[define [sxml->xml-file ns-list root attr-list body filename]
  [srl:sxml->xml
    [list '*TOP*
      [list '*PI* 'xml "version=\"1.0\" encoding=\"UTF-8\""]
      [list '@ [cons '*NAMESPACES* ns-list]]
      [list root [cons '@ attr-list] body]]
    filename]]

;*******************************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;********************************************************************************************
;*** Streams ***
;***************

[define [disp-strm x] [if [strm? x]
  [begin [displayln "-"] [strm-for-each disp-strm x]]
  [displayln x]]]

;[define nats [strm-cons 0 [strm-map add1 nats]]]
[define nats [strm-from 0]]

[define [strm-number-alike pred? strm]
  [strm-unfold
    [lambda [x] [cons [strm-car [car x]] [cdr x]]]
    [lambda [x] [not [strm-null? [car x]]]]
    [lambda [x] [if [strm-null? [strm-cdr [car x]]]
                  [cons strm-null 0]
                  [if [pred? [strm-car [car x]] [strm-car [strm-cdr [car x]]]]
                    [cons [strm-cdr [car x]] [+ [cdr x] 1]]
                    [cons [strm-cdr [car x]] 0]]]]
    [cons strm 0]]]

[define-strm [strm-uncat pred? strm]
  [if [strm-null? strm]
    strm-null
    [let-values [[[a b][strm-split-at pred? [strm-cdr strm]]]]
      [strm-cons
        [strm-cons [strm-car strm] a]
        [strm-uncat pred? b]]]]]

[define-strm [strm-chunk n strm]
  [if [strm-null? strm]
    strm-null
    [strm-cons
      [strm-take n strm]
      [strm-chunk n [strm-drop n strm]]]]]

[define [strm-collate istrm]
  [strm-unfolds
    [lambda [s]
      [if [strm-null? s]
          [values s '[] '[]]
        [if [strm-null? [strm-cdr s]]
           [values [strm-cdr s] [list [strm-car s]] '[]]
          [let [[a [strm-car s]]
                [b [strm-car [strm-cdr s]]]
                [d [strm-cdr [strm-cdr s]]]]
              [values d [list a] [list b]]]]]]
    istrm]]
;[call-with-values  [lambda [] [strm-extraleave [stream 1 2 3 4 5]]] [lambda [x y] [begin [display-strm x] [display-strm y]]]]

[define [strm-uncollate s1 s2 ] [void]];add this

[define [strm-bi-partition pred? strm]
  [strm-unfolds
    [lambda [seed]
      [if [strm-null? seed]
          [values seed '[] '[]]
          [let [[a [strm-car seed]]
                [d [strm-cdr seed]]]
            [if [pred? a]
                [values d [list a] #f]
                [values d #f [list a]]]]]]
    strm]]

[define [strm-split-at pred? strm]
  [strm-unfolds
    [lambda [sd]
      [if [strm-null? [cdr sd]]
          [values sd '[] '[]]
          [let [[a [strm-car [cdr sd]]]
                [d [strm-cdr [cdr sd]]]]
            [if [or [car sd] [pred? a]]
                [values [cons #t d] #f [list a]]
                [values [cons #f d] [list a] #f]]]]]
    [cons #f strm]]]

[define-strm [strm-smerge lt? . strms]
  [define-strm [merge xx yy]
    [stream-match xx [[] yy] [[x . xs]
      [stream-match yy [[] xx] [[y . ys]
        [if [lt? y x]
            [strm-cons y [merge xx ys]]
            [strm-cons x [merge xs yy]]]]]]]]
  [strm-let loop [[strms strms]]
    [cond [[null? strms] strm-null]
          [[null? [cdr strms]] [car strms]]
          [else [merge [car strms]
                       [apply strm-smerge lt?
                         [cdr strms]]]]]]]

[define [rec-strm-sort lt? s]
  [if [strm-null? [strm-cdr s]] s
    [rec-strm-sort lt?
      [strm-map
        [lambda [x] [if [strm-null? [strm-cdr x]]
                      [strm-car x]
                      [strm-smerge lt? [strm-car x] [strm-car [strm-cdr x]]]]]
        [strm-chunk 2 s]]]]]

[define [strm-sort lt? s] [strm-car [rec-strm-sort lt? [strm-map [lambda [x] [stream x]] s]]]]

[define-strm [strm-unique eql? s]
  [if [strm-null? s]
      strm-null
      [strm-cons [strm-car s]
        [strm-unique eql?
          [strm-drop-while
            [lambda [x]
              [eql? [strm-car s] x]]
            s]]]]]


[define [rec-find-sm lt? x]
  [let [[fstrm [car x] ][rstrm [cdr x]]]
  [if [or [strm-null? fstrm] [strm-null? rstrm]]
    [cons strm-null strm-null]
    [if [lt? [strm-car rstrm] [strm-car fstrm]]
      [rec-find-sm lt? [cons fstrm [strm-drop-while [lambda [x] [lt? x [strm-car fstrm]]] rstrm]]]
      [if [lt? [strm-car fstrm] [strm-car rstrm]]
        [rec-find-sm lt? [cons [strm-drop-while [lambda [x] [lt? x [strm-car rstrm]]] fstrm] rstrm]]
        [cons fstrm rstrm]]]]]]

[define [strm-intersect lt? fstrm rstrm]
  [strm-unfold 
     [lambda [x] [cons [strm-car [cdr x]] [strm-car [car x]]]] 
     [lambda [x] [and [not [strm-null? [car x]]][not [strm-null? [cdr x]]]]]
     [lambda [x] [rec-find-sm lt? [cons [strm-cdr [car x]] [cdr x]]]]
     [rec-find-sm lt? [cons rstrm fstrm]]
   ]]

[define [strm-diff] [void]];add this

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define-strm [strm<-file filename]
  [let [[p [open-input-file filename]]]
    [strm-let loop [[c [read-line p]]]
      [if [eof-object? c]
          [begin [close-input-port p]
                 strm-null]
          [strm-cons c
            [loop [read-line p]]]]]]]

[define [file<-strm file pf strm]
  [let [[o [open-output-file file #:exists 'replace]]]
    [strm-for-each [lambda [x] [pf x o]] strm]
    [close-output-port o]]]

[define [strm<-cmd cmd]
  [let-values [[[in-p out-p pid err-p stat]
                [apply values [process/ports #f #f [current-output-port] cmd ]]]]
    [strm-let loop [[c [read-line in-p]]]
      [if [eof-object? c]
          [begin [close-input-port in-p]
                 strm-null]
          [strm-cons c
            [loop [read-line in-p]]]]]]]

[define-strm [strm-rxm<-file rx filename]
  [let [[p [open-input-file filename]]]
    [strm-let loop [[c [regexp-match rx p]]]
      [if [equal? #f c]
          [begin [close-input-port p] strm-null]
          [strm-cons c [loop [regexp-match rx p]]]]]]]

[define [xml-elem->file x]
  [let* [[xs [bytes->string/utf-8 [car x]]]
         [id [cadr [regexp-match #rx"<recordid>(.*?)</recordid>" xs]]]
         [o [open-output-file [cat "dest/" id] #:exists 'replace]]]
    [displayln "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" o]
    [displayln xs o]
    [close-output-port o]
    ]]



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [strm-do in-p do-re exit-re]
  [strm-let loop [[r [regexp-match do-re in-p 0]]]
    [if [regexp-match exit-re [car r]]
        [begin strm-null]
        [strm-cons [car r]
          [loop [regexp-match do-re in-p 0]]]]]]

[define [strm<-sqlline sys conn-str fmt q-str]
  [let-values [[[in-p out-p pid err-p stat]
                [apply values [process*/ports #f #f [current-output-port]
                  "/usr/bin/java" "-Djava.ext.dirs=./" "-jar" "/usr/share/java/sqlline.jar"
                  ]]]]
    [let [[dore [regexp [cat "([0-9]+: " sys ":[0-9]*>|[^\n]*\n)"]]]
          [exre [regexp [cat "[0-9]+: " sys ":[0-9]*>"]]]
          [con-str [cat "!connect " sys conn-str]] ]
    [file-stream-buffer-mode out-p 'line] 
    [strm-for-each displayln [strm-do in-p #rx"(sqlline>|[^\n]*\n)" #rx"sqlline>"]]
    [displayln con-str out-p]
    [strm-for-each displayln [strm-do in-p dore exre]]
    [displayln "!set incremental true " out-p]
    [strm-for-each displayln [strm-do in-p dore exre]]
    [displayln [cat "!outputformat " fmt] out-p]
    [strm-for-each displayln [strm-do in-p dore exre]]
    [displayln q-str out-p]
    [let [[r-strm [strm-do in-p dore exre]]]
      [displayln "!quit" out-p]
      [flush-output out-p]
      r-strm]]]]

[define [strm<-sqlline<-strm sys conn-str fmt q-str]
  [let-values [[[in-p out-p pid err-p stat]
                [apply values [process*/ports #f #f [current-output-port]
                  "/usr/bin/java" "-Djava.ext.dirs=./" "-jar" "/usr/share/java/sqlline.jar"
                  ]]]]
    [let [[dore [regexp [cat "([0-9]+: " sys ":>|[^\n]*\n)"]]]
          [exre [regexp [cat "[0-9]+: " sys ":>"]]]
          [con-str [cat "!connect " sys conn-str]] ]
    [file-stream-buffer-mode out-p 'line] 
    [strm-for-each displayln [strm-do in-p #rx"(sqlline>|[^\n]*\n)" #rx"sqlline>"]]
    [displayln con-str out-p]
    [strm-for-each displayln [strm-do in-p dore exre]]
    [displayln "!set incremental true " out-p]
    [strm-for-each displayln [strm-do in-p dore exre]]
    [displayln [cat "!outputformat " fmt] out-p]
    [strm-for-each displayln [strm-do in-p dore exre]]
    [let* [[r-strm [map [lambda [x] [begin [displayln x out-p] [let [[r [strm-do in-p dore exre]]] r]]] [list<-strm q-str]] ]
           [rl [length r-strm]]]
      [displayln rl]
      [displayln "!quit" out-p]
      [flush-output out-p]
      r-strm]]]]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [get-resp in out serv path]
  [let-values [[[path rh] [uri&headers->path&header [cat serv path] [hash]]]]
     [when [start-request in out "1.1" "GET" path rh] ;[display "value" out];[flush-output out]
       [void]]
     [let [[h [purify-port/log-debug in]]]
       [values h [read-entity/bytes in h]]
     ]]]

[define [get-resource serv path-list]
  [let-values [[[scheme host port] [uri->scheme&host&port serv]]]
    [call/requests scheme host port
      [lambda [in out]
        [map [lambda [i] [call-with-values [lambda [] [get-resp in out serv i]]
                                           [lambda [x y] [cons x y]]]]
             path-list]] 
    ]]]

[define [http-path-strm->doc-strm sys pstrm]
  [strm-concat [strm<-list [get-resource sys [list<-strm [strm-chunk 1000 pstrm]]]]]] 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define node->file [lambda [x]
  [let* [[xs [bytes->string/utf-8 [car x]]]
         [id [cadr [regexp-match #rx"<recordid>(.*?)</recordid>" xs]]]
         [o [open-output-file [cat "dest/" id] #:exists 'replace]]]
    [displayln "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" o]
    [displayln xs o]
    [close-output-port o]
    ]]]



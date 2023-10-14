;;;; package.lisp

(defpackage #:l-99
  (:use #:cl #:fg)
  (:export #:last. #:last/loop
           #:butlast. #:butlast/loop
           #:kth #:kth/loop
           #:length. #:length/loop
           #:reverse. #:reverse/loop
           #:palindromep
           #:flatten #:flatten/loop
           #:compress #:compress/loop
           #:pack #:pack/loop
           #:encode #:encode/loop
           #:encode-modified #:encode-modified/loop
           #:decode #:decode/map
           #:encode-direct #:encode-direct/loop
           #:dupli #:dupli/loop
           #:repli #:repli/loop
           #:drop #:drop/loop
           #:split-at #:split-at/loop
           #:slice #:slice/loop
           #:rotate
           #:remove-at #:remove-at/loop
           #:insert-at #:insert-at/loop
           #:range #:range/loop
           #:rnd-select
           #:lotto-select
           #:random-permutation
           #:combine
           #:multinomial-coefficient
           #:multicombine
           #:lsort
           #:lfsort))


(defpackage #:l-99/tests
  (:use #:cl #:fg #:parachute #:l-99))

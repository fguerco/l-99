(in-package #:l-99/tests)

(define-test last
  (is equal '(d) (last. '(a b c d)))
  (is equal '(d) (last/loop '(a b c d))))

(define-test butlast
  (is equal '(c d) (butlast. '(a b c d)))
  (is equal '(c d) (butlast/loop '(a b c d))))
  
(define-test kth
  (is equal 'c (kth '(a b c d e) 3))
  (is equal 'c (kth/loop '(a b c d e) 3)))

(define-test length
  (is = 5 (length. '(a b c d e)))
  (is = 5 (length/loop '(a b c d e))))

(define-test reverse
  (is equal '(c b a) (reverse. '(a b c)))
  (is equal '(c b a) (reverse/loop '(a b c))))

(define-test palindromep
  (true (palindromep '(x a m a x))))

(define-test flatten
  (is equal '(a b c d e) (flatten '(a (b (c d) e))))
  (is equal '(a b c d e) (flatten/loop '(a (b (c d) e)))))

(define-test compress
  (is equal '(a b c a d e) (compress '(a a a a b c c a a d e e e e)))
  (is equal '(a b c a d e) (compress/loop '(a a a a b c c a a d e e e e))))

(define-test pack
  (is equal '((a a a a) (b) (c c) (a a) (d) (e e e e))
      (pack '(a a a a b c c a a d e e e e)))
  (is equal '((a a a a) (b) (c c) (a a) (d) (e e e e))
      (pack/loop '(a a a a b c c a a d e e e e))))

(define-test encode
  (is equal '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))
      (encode '(a a a a b c c a a d e e e e)))
  (is equal '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))
      (encode/loop '(a a a a b c c a a d e e e e))))

(define-test encode-modified
  (is equal '((4 a) b (2 c) (2 a) d (4 e))
      (encode-modified '(a a a a b c c a a d e e e e)))
  (is equal '((4 a) b (2 c) (2 a) d (4 e))
      (encode-modified/loop '(a a a a b c c a a d e e e e))))

(define-test decode
  (is equal '(a a a a b c c a a d e e e e)
      (decode '((4 a) b (2 c) (2 a) d (4 e))))
  (is equal '(a a a a b c c a a d e e e e)
      (decode/map '((4 a) b (2 c) (2 a) d (4 e)))))

(define-test encode-direct
  (is equal '((4 a) b (2 c) (2 a) d (4 e))
      (encode-direct '(a a a a b c c a a d e e e e)))
  (is equal '((4 a) b (2 c) (2 a) d (4 e))
      (encode-direct/loop '(a a a a b c c a a d e e e e))))

(define-test dupli
  (is equal '(a a b b c c c c d d)
      (dupli '(a b c c d)))
  (is equal '(a a b b c c c c d d)
      (dupli/loop '(a b c c d))))

(define-test repli
  (is equal '(a a a b b b c c c)
      (repli '(a b c) 3))
  (is equal '(a a a b b b c c c)
      (repli/loop '(a b c) 3)))

(define-test drop
  (is equal '(a b d e g h k)
      (drop '(a b c d e f g h i k) 3))
  (is equal '(a b d e g h k)
      (drop/loop '(a b c d e f g h i k) 3)))

(define-test split-at
  (is equal '((a b c) (d e f g h i k))
      (split-at '(a b c d e f g h i k) 3))
  (is equal '((a b c) (d e f g h i k))
      (split-at/loop '(a b c d e f g h i k) 3)))

(define-test slice
  (is equal '(c d e f g) (slice '(a b c d e f g h i k) 3 7))
  (is equal '(c d e f g) (slice/loop '(a b c d e f g h i k) 3 7)))

(define-test rotate
  (is equal '(d e f g h a b c) (rotate '(a b c d e f g h) 3)))

(define-test remove-at
  (is equal '(a c d) (remove-at '(a b c d) 2))
  (is equal '(a c d) (remove-at/loop '(a b c d) 2)))

(define-test insert-at
  (is equal '(a alfa b c d) (insert-at 'alfa '(a b c d) 2))
  (is equal '(a alfa b c d) (insert-at/loop 'alfa '(a b c d) 2)))

(define-test range
  (is equal '(4 5 6 7 8 9) (range 4 9))
  (is equal '(3 2 1 0 -1 -2) (range 3 -2))
  (is equal '(4 5 6 7 8 9) (range/loop 4 9))
  (is equal '(3 2 1 0 -1 -2) (range/loop 3 -2)))

(define-test rnd-select
  (destructuring-bind (a b) (rnd-select '(a b c d e) 2)
    (true (member a '(a b c d e)))
    (true (member b '(a b c d e)))
    (false (eql a b))))

(define-test lotto-select
  (destructuring-bind (a b c) (lotto-select 3 10)
    (true (member a (range 1 10)))
    (true (member b (range 1 10)))
    (true (member b (range 1 10)))
    (let ((x (list a)))
      (pushnew b x)
      (pushnew c x)
      (is = 3 (length x)))))

(define-test random-permutation
  (let ((test-data '(1 2 3 4)))
    (destructuring-bind (a b c d) (random-permutation test-data)
      (true (member a test-data))
      (true (member b test-data))
      (true (member c test-data))
      (true (member d test-data))
      (let ((x (list a)))
        (pushnew b x)
        (pushnew c x)
        (pushnew d x)
        (is = 4 (length x))))))

(define-test combine
  (is equal '((a b c) (a b d) (a b e) (a b f)
              (a c d) (a c e) (a c f)
              (a d e) (a d f)
              (a e f)
              (b c d) (b c e) (b c f)
              (b d e) (b d f)
              (b e f)
              (c d e) (c d f)
              (c e f)
              (d e f))
      (combine 3 '(a b c d e f))))

(define-test multicombine
  (is equal '(((a b) (c d)) ((a c) (b d)) ((a d) (b c))
              ((b c) (a d)) ((b d) (a c))
              ((c d) (a b)))
      (multicombine '(a b c d) '(2 2))))

(define-test lsort
  (is equal '((o) (d e) (d e) (m n) (a b c) (f g h) (i j k l))
      (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))))

(define-test lfsort
  (is equal '((i j k l) (o) (f g h) (a b c) (m n) (d e) (d e))
      (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))))


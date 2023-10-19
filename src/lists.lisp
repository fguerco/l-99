(in-package #:l-99/lists)
            
;; Working with lists

(defun last. (list)
  "P01 Find the last box of a list"
  (if (cdr list)
      (last. (cdr list))
      list))

(defun last/loop (list)
  "P01 loop version"
  (loop for x on list
    unless (cdr x) return x))

(defun butlast. (list)
  "P02 Find the last but one box of a list"
  (if (cddr list)
      (butlast. (cdr list))
      list))

(defun butlast/loop (list)
  "P02 loop version"
  (loop for x on list
    unless (cddr x) return x))

(defun kth (list k)
  "P03 Find the K'th element of a list"
  (cond ((not (plusp k)) nil)
        ((= k 1) (car list))
        (t (kth (cdr list) (1- k)))))

(defun kth/loop (list k)
  "P03 loop version"
  (loop for n from 1 upto k
    for x in list
    if (= n k) return x))

(defun length. (list)
  "P04 Find the number of elements of a list"
  (if (endp list)
      0
      (1+ (length. (cdr list)))))

(defun length/loop (list)
  "P04 loop version"
  (let ((r 0))
    (dolist (x list r) (incf r))))

(defun reverse. (list)
  "P05 Reverse a list"
  (if (endp list)
      nil
      (append (reverse. (cdr list))
              (list (car list)))))

(defun reverse/loop (list)
  "P05 loop version"
  (let (r)
    (dolist (x list r) (push x r))))
  
(defun palindromep (list)
  "P06 Find out whether a list is a palindrome"
  (equalp list (reverse. list)))

(defun flatten (list)
  "P07 (**) Flatten a nested list structure"
  (cond
    ((atom list) list)
    ((atom (car list)) (cons (car list)
                             (flatten (cdr list))))
    (t (nconc (flatten (car list))
              (flatten (cdr list))))))

(defun flatten/loop (list)
  "P07 loop version"
  (loop for x in list
    if (atom x) nconc (list x)
    else nconc (flatten/loop x)))


(defun compress (list)
  "P08 (**) Eliminate consecutive duplicates of list elements"
  (cond
    ((atom list) list)
    ((eql (car list) (cadr list)) (compress (cdr list)))
    (t (list* (car list) (compress (cdr list))))))

(defun compress/loop (list)
  "P08 loop version"
  (loop for x in list
    and p = x
    unless (eql x p) collect x))
    
(defun pack (list)
  "P09 (**) Pack consecutive duplicates of list elements into sublists"
  (nlet recur ((packed (list (car list))) (list (cdr list)) (acc nil))
    (cond
      ((endp list) (reverse. (cons packed acc)))
      ((eql (car packed) (car list))
       (recur (cons (car list) packed)
              (cdr list)
              acc))
      (t (recur (list (car list))
                (cdr list)
                (cons packed acc))))))

(defun pack/loop (list)
  "P09 loop version"
  (loop
    with packed and acc
    for (x y) on list
    do (push x packed)
    unless (eql x y)
      do (progn (push packed acc)
                (setf packed nil))
    finally (return (reverse. acc))))

(defun encode (list)
  "P10 (*) Run-length encoding of a list"
  (nlet recur ((list (pack list)) (acc nil))
    (if (endp list)
        (reverse. acc)
        (recur (cdr list)
               (cons (list (length. (car list)) (caar list))
                     acc)))))

(defun encode/loop (list)
  "P10 using loop"
  (loop for x in (pack/loop list)
        collect (list (length. x) (car x))))

(defun encode-modified (list)
  "P11 (*) Modified run-length encoding"
  (nlet recur ((list (encode list)) (acc nil))
    (if (endp list)
        (reverse acc)
        (destructuring-bind (len elem) (car list)
          (when (> len 1)
            (setf elem (list len elem)))
          (recur (cdr list)
                 (cons elem acc))))))


(defun encode-modified/loop (list)
  "P11 loop version"
  (loop for (len x) in (encode list)
        collect (if (= 1 len)
                    x
                    (list len x))))

(defmethod unpack ((packed list))
  (loop repeat (car packed) collect (cadr packed)))

(defmethod unpack (packed)
  (list packed))

(defun decode (list)
  "P12 (**) Decode a run-length encoded list"
  (if (endp list)
      nil
      (nconc (unpack (car list)) (decode (cdr list)))))

(defun decode/map (list)
  "P12 using mapcan"
  (mapcan #'unpack list))

  
(defun encode-direct (list)
  "P13 (**) Run-length encoding of a list (direct solution)"
  (symbol-macrolet ((pack (if (= 1 (car packed)) (cadr packed) packed)))
    (nlet encode ((list (cdr list)) (packed (list 1 (car list))))
      (cond ((endp list) (list pack))
            ((eql (car list) (cadr packed))
             (incf (car packed))
             (encode (cdr list) packed))
            (t (cons pack
                     (encode (cdr list) (list 1 (car list)))))))))


(defun encode-direct/loop (list)
  "P13 loop version"
  (symbol-macrolet ((pack
                      (push (if (= 1 (car packed)) (cadr packed) packed) r)))
    (loop with r and packed
          for x in list and prev = x
          if (eql prev x)
            do (incf (car packed))
          else
            do (when packed pack)
               (setf packed (list 1 x))
          finally (return (progn pack (reverse r))))))

(defun dupli (list)
  "P14 (*) Duplicate the elements of a list"
  (if (endp list)
      nil
      (nconc (list #1=(car list) #1#) (dupli (cdr list)))))

(defun dupli/loop (list)
  "P14 loop version"
  (loop for x in list
        nconc (list x x)))

(defun repli (list times)
  "P15 (**) Replicate the elements of a list a given number of times"
  (labels ((many (item times)
             (if (zerop times)
                 nil
                 (cons item (many item (1- times))))))
  (if (endp list)
      nil
      (nconc (many (car list) times) (repli (cdr list) times)))))

(defun repli/loop (list times)
  "P15 loop version"
  (loop for x in list
        nconc (loop repeat times collect x)))

(defun drop (list n)
  "P16 (**) Drop every N'th element from a list"
  (symbol-macrolet ((rec (recur (cdr list) n (1+ i))))
    (nlet recur ((list list) (n n) (i 1))
      (cond ((endp list) nil)
            ((zerop (mod i n)) rec)
            (t (cons (car list) rec))))))

(defun drop/loop (list n)
  "P16 loop version"
  (loop for x in list
        for i from 1
        unless (zerop (mod i n)) collect x))


(defun split-at (list n)
  "P17 (*) Split a list into two parts; the length of the first part is given.
   Do not use any predefined functions"
  (labels ((head (list i)
             (if (or (endp list) (zerop i))
                 nil
                 (cons (car list)
                       (head (cdr list) (1- i)))))
           (tail (list i)
             (cond ((endp list) nil)
                   ((zerop i) list)
                   (t (tail (cdr list) (1- i))))))
    (if (plusp n)
        (list (head list n) (tail list n))
        list)))

(defun split-at/loop (list n)
  "P17 loop version"
  (if (plusp n)
      (loop for (x . tail) on list
            repeat n
            collect x into head
            finally (return (list head (cons x tail))))
      list))

(defun slice (list i k)
  "P18 (**) Extract a slice from a list"
  (symbol-macrolet ((recur (%slice (cdr list) (1+ n))))
    (nlet %slice ((list list) (n 1))
      (cond
        ((or (endp list) (> n k)) nil)
        ((< n i) recur)
        (t (cons (car list) recur))))))

(defun slice/loop (list i k)
  "P18 loop version"
  (loop for x in list
        for n from 1 to k
        if (<= i n k)
          collect x))

(defun rotate (list n)
  "P19 (**) Rotate a list N places to the left"
  (let ((n (mod n (length list))))
    (destructuring-bind (a b) (split-at list n)
      (append b a))))

(defun remove-at (list k)
  "P20 (*) Remove the K'th element from a list"
  (labels ((recur (list k acc)
             (cond
               ((endp list) (values (reverse acc) nil))
               ((= 1 k) (values (nconc (reverse acc) (cdr list)) (car list)))
               (t (recur (cdr list) (1- k) (cons (car list) acc))))))
    (recur list k nil)))

(defun remove-at/loop (list k)
  "P20 loop version"
  (loop for (x . rest) on list
        for i from 1 to k
        if (= i k) return (values (append r rest) x)
        else collect x into r))

(defun insert-at (elem list n)
  "P21 (*) Insert an element at a given position into a list"
  (cond
    ((or (endp list) (= 1 n)) (cons elem list))
    (t (cons (car list) (insert-at elem (cdr list) (1- n))))))

(defun insert-at/loop (elem list n)
  "P21 loop version"
  (loop for (x . rest) on list
        for i from 1 to n
        if (or (= i n) (endp rest)) nconc (list* elem x rest)
        else collect x))

(defun range (from to)
  "P22 (*) Create a list containing all integers within a given range"
  (cond
    ((= from to) (list to))
    ((> from to) (cons from (range (1- from) to)))
    (t (cons from (range (1+ from) to)))))

(defun range/loop (start end)
  "P22 loop version"
  (loop with step = (if (> end start) 1 -1)
        for x = start then (+ x step)
        until (if (> end start) (> x end) (< x end))
        collect x))

(defun rnd-select (list n)
  "P23 (**) Extract a given number of randomly selected elements from a list"
  (if (zerop n)
      nil
      (let ((rand (1+ (random (length list)))))
        (multiple-value-bind (l e) (remove-at list rand)
          (cons e (rnd-select l (1- n)))))))

(defun lotto-select (n m)
  "P24 (*) Lotto: Draw N different random numbers from the set 1..M"
  (rnd-select (range 1 m) n))

(defun random-permutation (list)
  "P25 (*) Generate a random permutation of the elements of a list"
  (rnd-select list (length list)))

(defun combine (n list)
  "P26 (**) Generate the combinations of K distinct objects chosen from the N
 elements of a list"
  (nlet %combine ((n n) (list list) (acc nil))
    (if (zerop n)
        (list (reverse acc))
        (loop for (x . xs) on list
              nconc (%combine (1- n) xs (cons x acc))))))

(defun multinomial-coefficient (total &rest combination)
  (flet ((fact (n) (loop for x from 1 to n
                         for y = 1 then (* x y)
                         finally (return y))))
    (let ((n (fact total))
          (ns (reduce #'* (mapcar #'fact combination))))
      (/ n ns))))

(defun multicombine (list ns)
  "P27 (**) Group the elements of a set into disjoint subsets"
  (nlet recur ((list list) (ns ns) (acc nil))
    (if (endp (cdr ns))
        (loop for x in (combine (car ns) list)
              collect (reverse (cons x acc)))
        (loop for x in (combine (car ns) list)
              for l2 = (remove-if (lambda (y) (member y x)) list)
              nconc (recur l2 (cdr ns) (cons x acc))))))

(defun lsort (list)
  "P28-A (**) Sorting a list of lists according to length of sublists"
  (sort (copy-list list) #'< :key #'length))

(defun lfsort (list)
  "P28-B (**) Sorting a list of lists according to length frequency of sublists"
  (let* ((ht (reduce (lambda (ht x)
                       (push x (gethash (length x) ht nil))
                       ht)
                     list :initial-value (make-hash-table)))
         (frequencies (loop for v being each hash-value of ht
                            collect (cons (length v) v))))
    (mapcan #'cdr (sort frequencies #'< :key #'car))))


; problem 1
; find the last item in a list

(defun my-last (ls) 
  (if (null ls) 
    nil 
    (if (null (cdr ls)) 
      (car ls) 
      (my-last (cdr ls)))))


; problem 2
; find the second last of a list
(defun my-but-last (ls)
  (if (or (null ls) (null (cdr ls))) nil
    (if (null (cddr ls)) (car  ls)
      (my-but-last (cdr ls)))))

; prblem 3
; find kth element of a list (1 indexed)
(defun element-at (ls i)
  (car (nthcdr (- i 1) ls)))

; problem 4
; find the number of elements in a list
(defun my-length (ls)
  (if (null ls) 0
    (+ 1 (my-length (cdr ls)))))


; problem 5
; reverse a list
(defun my-reverse (ls)
  (reduce #'(lambda (x y) (cons y x)) ls :initial-value nil))

; problem 6
; find out whether a list is a palindrome
(defun palindrome-p (ls)
  (cond
    ((null ls) t)
    ((null (cdr ls)) t)
    ((null (cddr ls)) (equal (first ls) (second ls)))
    (t (and (equal (first ls) (my-last ls)) 
            (palindrome-p (butlast (rest ls)))))))

; problem 7
; flatten nested list structure
(defun my-flatten (ls)
  (cond
    ((null ls) nil)
    ((listp (car ls)) 
     (concatenate 'list (my-flatten (car ls)) (my-flatten (cdr ls))))
    (t (cons (car ls) (my-flatten (cdr ls))))))

; problem 8
; Eliminate consecutive duplicates of a list
(defun compress (ls)
  (cond
    ((null ls) nil)
    ((equal (first ls) (second ls)) (compress (cdr ls)))
    (t (cons (car ls) (compress (cdr ls))))))

; problem 9
; Pack consecutive duplicates of list into sublist
(defun pack (ls)
  (if (null ls) nil 
    (let ((r (pack (cdr ls))) (h (car ls)))
      (if (equal (first ls) (second ls)) 
        (cons 
          (cons h (car r))
          (cdr r))
        (cons (list h) r)))))

; problem 10
; run-length encode a list (use problem 9)
(defun encode (ls)
  (mapcar #'(lambda (x) (list (length x) (first x))) (pack ls)))

; problem 11
; modified run-length encode - if length is 1, put the element in the list directly
(defun encode-modified (ls)
  (mapcar #'(lambda (x) (if (equal (first x) 1) (second x) x)) (encode ls)))


; problem 12
; run-length decode - use the run-length encoding from problem 11
(defun decode-modified (ls)
  (if (null ls) nil (let ((elem (car ls)))
    (if (listp elem) (append (make-list (first elem) :initial-element (second elem))
                             (decode-modified (cdr ls)))
      (cons elem (decode-modified (cdr ls)))))))

; problem 13
; modified run-length direct version -- make problem 11 directly without first making sublists
(defun direct-encode (ls)
  (labels ((make-runl-elem (elem count)
                           (if (equal count 1) elem (list count elem)))
           (de-sub (ls precar count)
                   (cond ((null ls) (list (make-runl-elem precar count)))
                         ((equal precar (car ls)) (de-sub (cdr ls) precar (+ count 1)))
                         (t (append (list (make-runl-elem precar count))
                                    (de-sub (cdr ls) (car ls) 1))))))
    (if (null ls) nil
      (de-sub (cdr ls) (car ls) 1))))

; problem 14
; duplicate elements of a list
(defun duplicate (ls)
  (if (null ls) nil
    (append (list (car ls) (car ls)) (duplicate (cdr ls)))))

; problem 15
; replicate elements of a list a given number of times (ie (repli '(1 2 3) 3) -> (1 1 1 2 2 2 3 3 3))
(defun repli (ls num)
  (if (null ls) nil
    (append (make-list num :initial-element (car ls)) (repli (cdr ls) num))))

; problem 16
; drop every nth element from a list
(defun drop-nth (ls n)
  (labels ((dn-sub (ls n count)
                   (if (null ls) nil
                     (if (equal count n) (dn-sub (cdr ls) n 1)
                       (cons (car ls) (dn-sub (cdr ls) n (+ count 1)))))))
    (dn-sub ls n 1)))

; problem 17
; split a list into two - length of the first part given
; Do not use any pre-defined predicates.
(defun split (ls len)
  (labels ((split-sub (ls len count)
                      (if (null ls) (list nil nil)
                        (if (equal count len) (list nil ls)
                          (let ((sret (split-sub (cdr ls) len (+ count 1))))
                            (list (cons (car ls) (first sret)) (second sret)))))))
    (split-sub ls len 0)))

; problem 18
; slice a list on indices (inclusive, one-indexed)
(defun slice (xs start end)
  (cond
   ((null xs) nil)
   ((> start 1) (slice (cdr xs) (- start 1) (- end 1)))
   ((> end 0) (cons (car xs) (slice (cdr xs) 1 (- end 1))))
   (t nil)))

; problem 19
; rotate a list n places to the left (negative values rotate right)
(defun rotate (xs n)
  (if (equal n 0) xs
    (let ((l (length xs)))
      (if (> n 0) (let ((splits (split xs (mod n l))))
                    (append (cadr splits) (car splits))))
      (let ((splits (split xs (+ l (mod n (- l))))))
        (append (cadr splits) (car splits))))))

; problem 20
; remove a 1-indexed element from a list
(defun remove-at (xs n)
  (cond
   ((or (null xs) (< n 1)) xs)
   ((equal n 1) (cdr xs))
   (t (cons (car xs) (remove-at (cdr xs) (- n 1))))))
  
; problem 21
; insert an element into a list
(defun insert-at (elem xs n)
  (cond
   ((null xs) (list elem))
   ((<= n 1) (cons elem xs))
   (t (cons (car xs) (insert-at elem (cdr xs) (- n 1))))))

; problem 22
; create a list of an integer range (inclusive)
; (if end is less than start, make them decrease)
(defun range (start end)
  (cond
   ((equal start end) (list end))
   ((< start end) (cons start (range (1+ start) end)))
   (t (cons start (range (1- start) end)))))


;; problem 23
;; select a number of elements from a list at random
(defun rnd-select (xs n)
  (if (<= n 0) nil
    (let ((l (length xs)))
      (cond
       ((>= n l) xs)
       (t (rnd-select (remove-at xs (1+ (random l))) n))))))

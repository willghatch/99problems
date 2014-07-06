
;;;; LIST SECTION


;; problem 1
;; find the last item in a list

(defun my-last (ls) 
  (if (null ls) 
    nil 
    (if (null (cdr ls)) 
      (car ls) 
      (my-last (cdr ls)))))


;; problem 2
;; find the second last of a list
(defun my-but-last (ls)
  (if (or (null ls) (null (cdr ls))) nil
    (if (null (cddr ls)) (car  ls)
      (my-but-last (cdr ls)))))

;; prblem 3
;; find kth element of a list (1 indexed)
(defun element-at (ls i)
  (car (nthcdr (- i 1) ls)))

;; problem 4
;; find the number of elements in a list
(defun my-length (ls)
  (if (null ls) 0
    (+ 1 (my-length (cdr ls)))))


;; problem 5
;; reverse a list
(defun my-reverse (ls)
  (reduce #'(lambda (x y) (cons y x)) ls :initial-value nil))

;; problem 6
;; find out whether a list is a palindrome
(defun palindrome-p (ls)
  (cond
    ((null ls) t)
    ((null (cdr ls)) t)
    ((null (cddr ls)) (equal (first ls) (second ls)))
    (t (and (equal (first ls) (my-last ls)) 
            (palindrome-p (butlast (rest ls)))))))

;; problem 7
;; flatten nested list structure
(defun my-flatten (ls)
  (cond
    ((null ls) nil)
    ((listp (car ls)) 
     (concatenate 'list (my-flatten (car ls)) (my-flatten (cdr ls))))
    (t (cons (car ls) (my-flatten (cdr ls))))))

;; problem 8
;; Eliminate consecutive duplicates of a list
(defun compress (ls)
  (cond
    ((null ls) nil)
    ((equal (first ls) (second ls)) (compress (cdr ls)))
    (t (cons (car ls) (compress (cdr ls))))))

;; problem 9
;; Pack consecutive duplicates of list into sublist
(defun pack (ls)
  (if (null ls) nil 
    (let ((r (pack (cdr ls))) (h (car ls)))
      (if (equal (first ls) (second ls)) 
        (cons 
          (cons h (car r))
          (cdr r))
        (cons (list h) r)))))

;; problem 10
;; run-length encode a list (use problem 9)
(defun encode (ls)
  (mapcar #'(lambda (x) (list (length x) (first x))) (pack ls)))

;; problem 11
;; modified run-length encode - if length is 1, put the element in the list directly
(defun encode-modified (ls)
  (mapcar #'(lambda (x) (if (equal (first x) 1) (second x) x)) (encode ls)))


;; problem 12
;; run-length decode - use the run-length encoding from problem 11
(defun decode-modified (ls)
  (if (null ls) nil (let ((elem (car ls)))
    (if (listp elem) (append (make-list (first elem) :initial-element (second elem))
                             (decode-modified (cdr ls)))
      (cons elem (decode-modified (cdr ls)))))))

;; problem 13
;; modified run-length direct version -- make problem 11 directly without first making sublists
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

;; problem 14
;; duplicate elements of a list
(defun duplicate (ls)
  (if (null ls) nil
    (append (list (car ls) (car ls)) (duplicate (cdr ls)))))

;; problem 15
;; replicate elements of a list a given number of times (ie (repli '(1 2 3) 3) -> (1 1 1 2 2 2 3 3 3))
(defun repli (ls num)
  (if (null ls) nil
    (append (make-list num :initial-element (car ls)) (repli (cdr ls) num))))

;; problem 16
;; drop every nth element from a list
(defun drop-nth (ls n)
  (labels ((dn-sub (ls n count)
                   (if (null ls) nil
                     (if (equal count n) (dn-sub (cdr ls) n 1)
                       (cons (car ls) (dn-sub (cdr ls) n (+ count 1)))))))
    (dn-sub ls n 1)))

;; problem 17
;; split a list into two - length of the first part given
;; Do not use any pre-defined predicates.
(defun split (ls len)
  (labels ((split-sub (ls len count)
                      (if (null ls) (list nil nil)
                        (if (equal count len) (list nil ls)
                          (let ((sret (split-sub (cdr ls) len (+ count 1))))
                            (list (cons (car ls) (first sret)) (second sret)))))))
    (split-sub ls len 0)))

;; problem 18
;; slice a list on indices (inclusive, one-indexed)
(defun slice (xs start end)
  (cond
   ((null xs) nil)
   ((> start 1) (slice (cdr xs) (- start 1) (- end 1)))
   ((> end 0) (cons (car xs) (slice (cdr xs) 1 (- end 1))))
   (t nil)))

;; problem 19
;; rotate a list n places to the left (negative values rotate right)
(defun rotate (xs n)
  (if (equal n 0) xs
    (let ((l (length xs)))
      (if (> n 0) (let ((splits (split xs (mod n l))))
                    (append (cadr splits) (car splits))))
      (let ((splits (split xs (+ l (mod n (- l))))))
        (append (cadr splits) (car splits))))))

;; problem 20
;; remove a 1-indexed element from a list
(defun remove-at (xs n)
  (cond
   ((or (null xs) (< n 1)) xs)
   ((equal n 1) (cdr xs))
   (t (cons (car xs) (remove-at (cdr xs) (- n 1))))))
  
;; problem 21
;; insert an element into a list
(defun insert-at (elem xs n)
  (cond
   ((null xs) (list elem))
   ((<= n 1) (cons elem xs))
   (t (cons (car xs) (insert-at elem (cdr xs) (- n 1))))))

;; problem 22
;; create a list of an integer range (inclusive)
;; (if end is less than start, make them decrease)
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

;; problem 24
;; Draw n random numbers from set 1..M (in list)
(defun lotto-select (n m)
  (if (<= n 0) nil
    (cons (1+ (random m)) (lotto-select (1- n) m))))

;; problem 25
;; generate a random permutation of a list
(defun rnd-permu (xs)
  (labels
   ((subf (xs len)
          (if (<= len 0) nil
          (let ((index (1+ (random len))))
                (cons (element-at xs index)
                      (subf (remove-at xs index) (1- len)))))))
   (subf xs (length xs))))
  
;; problem 26
;; Generate combinations of k distinct objects chosen from a list
(defun combination (n xs)
  (labels
   ((comb-sub (taken t-len rest r-len n)
              (cond
               ((equal t-len n) (list taken))
               ((< (+ t-len r-len) n) (list nil))
               (t (append (comb-sub (cons (car rest) taken)
                                    (1+ t-len) (cdr rest) (1- r-len) n)
                          (comb-sub taken t-len (cdr rest) (1- r-len) n))))))
   (remove-if #'null (comb-sub nil 0 xs (length xs) n))))

;; problem 27
;; group the elements of a set into disjoint subsets
;; ie take a list of elements and a list of group sizes, and list the ways the elements
;; can be combined into those groups.  Within one group (a b) is equal to (b a),
;; but if two groups have size two. then ((a b) (c d)) is not equal to ((c d) (a b))
(defun group (elems gspecs)
  (cond
   ((null gspecs) nil)
   ((null (cdr gspecs)) (mapcar #'(lambda (x) (list x)) (combination (car gspecs) elems)))
   (t (let* ((combos (combination (car gspecs) elems))
             (set-of-groups (mapcar #'(lambda (comb) (group (set-difference elems comb) (cdr gspecs))) combos))
             (set-of-gr-with-combos (mapcar #'(lambda (c gr)
                                                (mapcar #'(lambda (cs)
                                                            (cons c cs))
                                                        gr))
                                            combos set-of-groups)))
        (reduce #'append set-of-gr-with-combos)))))


;; I guess that was part B, and part A was the less generic one that I thought was just
;; explanatory to help explain.  So here's A by way of B
(defun group3 (elems)
  (group elems '(2 3 4)))

               
;; problem 28
;; lists with sublists

;; part a
;; sort sublists by length (shortest first)
(defun lsort (xss)
  (sort xss #'< :key #'length))

;; part b
;; sort sublists by length frequency (least frequent first)
(defun lfsort (xss)
  (let ((with-l (mapcar #'(lambda (x) (cons (length x) x)) xss))
        (freq-map (make-hash-table)))
    (loop for elem in with-l do
          (let ((curfreq (gethash (car elem) freq-map)))
            (setf (gethash (car elem) freq-map)
                  (if (null curfreq) 1 (1+ curfreq)))))
    (mapcar #'cdr (sort with-l #'< :key #'(lambda (x) (gethash (car x) freq-map))))))


;; Problems 29 and 30 seem not to exist... in any version of the problem list I can find
;; (lisp, haskell, or the original prolog versions).
;; Maybe since the last two had two parts each those counted?
;; At any rate...


;;;; ARITHMETIC SECTION

;; problem 31
;; Determine whether a given integer is prime

(defun is-prime (x)
  (cond
   ((< x 2) nil) ; Negative primes?  I don't know if that counts.
   ((= x 2) t)
   (t (labels ((subf (x n)
                     (cond
                      ((= 0 (mod x n)) nil)
                      ((> (* n n) x) t)
                      (t (subf x (1+ n))))))
              (subf x 2)))))

;; problem 32
;; determine the GCD of two positive integers with euclid's algorithm
(defun my-gcd (x y) ; I can't define gcd, it's reserved apparently...
  (let* ((big (max x y))
         (small (min x y))
         (r (mod big small)))
    (if (= 0 r) small
      (gcd small r))))

;; problem 33
;; determine if two positive integers are coprime (gcd = 0)
(defun coprime (x y)
  (if (= (gcd x y) 1) t nil))

;; problem 34
;; Calculate Euler's totient function phi(m)
;; That is, determine the number of smaller positive integers that are coprime,
;; with a special case of phi(1) = 1.
(defun totient-phi (m)
  (if (= 1 m) 1
    (count t (mapcar #'(lambda (x) (coprime x m)) (range 1 (1- m))))))

;; problem 35
;; determine the prame factors of a given positive integer
;; * in a list in ascending order
(defun prime-factors (x)
  (if (= 1 x) '(1)
   (labels
       ((find-div (x n)
                  (cond
                   ((= 1 x) nil)
                   ((= 0 (mod x n)) (cons n (find-div (/ x n) n)))
                   ((> (* n n) x) (list x))
                   (t (find-div x (1+ n))))))
       (find-div x 2))))
   
;: problem 36
;; determine prime factors as above, but make list of factors and their frequency
(defun prime-factors-mult (x)
  ;; I have to swap elements of the run-length encoding, because they want them in
  ;; the other order...
  (mapcar #'(lambda (x) (list (cadr x) (car x))) (encode (prime-factors x))))

;; problem 37
;; totient func improved
;; Use phi(m) = (p1-1) * p1**(m1-1) + (p2-1) * p2**(m2-1) + ... where
;;   pn = nth prime factor, mn = multiplicity of pn
;; CORRECTION!!!!  The lisp version of the problems has that formula wrong.  It
;; should be a product, not a sum.
(defun totient-phi-improved (x)
  (let* ((factors (prime-factors-mult x)))
    (reduce #'* (mapcar
                 #'(lambda (fac) (let ((p (car fac)) (m (cadr fac)))
                                   (* (1- p) (expt p (1- m)))))
                 factors))))

;; problem 38
;; compare totient solutions...
;; Well, the second one is obviously much better.
;; Run them on 10090
;; The second one seemed pretty fast.  The first one got a stack overflow, because it's
;;   trying to load these ginormous lists.
;; Let's try something a little more modest, to give #1 a chance.  Using 1000:
;; #1 took 20 times as long and took 13 times as much memory.


;; problem 39
;; given a range with a lower and upper limit, make a list of the primes in that range



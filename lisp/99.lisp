
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



;;;; Test cases for Project 3.
;;;; To run (from the Project3 folder):  sbcl --script tests.lisp

;; Load our functions so we can test them.
(load "fighting_mongooses_project3.lisp")


;;; ------------------------------------------------------------
;;; check: prints one test result.
;;;   function-name  the function being tested (as text)
;;;   input          the input we gave it (as text)
;;;   expected       what the answer should be
;;;   actual         what the function really returned
;;; ------------------------------------------------------------


;; this function is for a cleaner output for tests
;; Used Claude for this function due to not knowing how to properly create
;; a clean formatted test result
(defun check (function-name input expected actual)
  (format t "Function: ~a~%" function-name)
  (format t "Input:    ~a~%" input)
  (format t "Expected: ~a~%" expected)
  (format t "Actual:   ~a~%" actual)
  (if (equal expected actual)
      (format t "Result:   PASS~%~%")
      (format t "Result:   FAIL~%~%")))


;; Sample data for the Student Course Analyzer.
(defvar *students*
  '((101 (1 85) (2 90) (3 78))
    (102 (1 92) (3 88))
    (103 (2 75) (3 95))
    (104 (1 80) (2 85))))


;;; ============================================================
;;; SET OPERATIONS (Ramin)
;;; ============================================================

(check "set-member" "'(1 2 3) 2" t   (set-member '(1 2 3) 2))
(check "set-member" "'(1 2 3) 5" nil (set-member '(1 2 3) 5))
(check "set-member" "'() 1"      nil (set-member '() 1))

(check "set-intersection" "'(1 2 3) '(2 4 3)" '(2 3) (set-intersection '(1 2 3) '(2 4 3)))
(check "set-intersection" "'(1 2) '(3 4)" nil (set-intersection '(1 2) '(3 4)))
(check "set-intersection" "'() '(1 2)" nil (set-intersection '() '(1 2)))

(check "set-diff" "'(1 2 3) '(2 4)" '(1 3) (set-diff '(1 2 3) '(2 4)))
(check "set-diff" "'(1 2) '(1 2)" nil (set-diff '(1 2) '(1 2)))
(check "set-diff" "'() '(1 2)" nil (set-diff '() '(1 2)))

(check "set-union" "'(1 2) '(2 4)" '(1 2 4) (set-union '(1 2) '(2 4)))
(check "set-union" "'(1 3) '(2 4)" '(1 3 2 4) (set-union '(1 3) '(2 4)))
(check "set-union" "'() '(1 2)" '(1 2) (set-union '() '(1 2)))

(check "set-symmetric-diff" "'(1 2 3) '(2 4)" '(1 3 4) (set-symmetric-diff '(1 2 3) '(2 4)))
(check "set-symmetric-diff" "'(1 2) '(1 2)" nil (set-symmetric-diff '(1 2) '(1 2)))
(check "set-symmetric-diff" "'() '(1 2)" '(1 2) (set-symmetric-diff '() '(1 2)))


;;; ============================================================
;;; BOOLEAN LOGIC (Keith)
;;; ============================================================

;; --- boolean-xor: full truth table (all 4 combinations) ---
(check "boolean-xor" "t nil"   t   (boolean-xor t nil))
(check "boolean-xor" "nil t"   t   (boolean-xor nil t))
(check "boolean-xor" "t t"     nil (boolean-xor t t))
(check "boolean-xor" "nil nil" nil (boolean-xor nil nil))

;; --- boolean-implies: full truth table ---
(check "boolean-implies" "t nil"   nil (boolean-implies t nil))   ; the only false case
(check "boolean-implies" "t t"     t   (boolean-implies t t))
(check "boolean-implies" "nil t"   t   (boolean-implies nil t))
(check "boolean-implies" "nil nil" t   (boolean-implies nil nil))

;; --- boolean-iff: full truth table ---
(check "boolean-iff" "t t"     t   (boolean-iff t t))
(check "boolean-iff" "nil nil" t   (boolean-iff nil nil))
(check "boolean-iff" "t nil"   nil (boolean-iff t nil))
(check "boolean-iff" "nil t"   nil (boolean-iff nil t))

;; --- boolean-eval: spec examples ---
(check "boolean-eval" "'(and t nil)"         nil (boolean-eval '(and t nil)))
(check "boolean-eval" "'(and t (or nil t))"  t   (boolean-eval '(and t (or nil t))))
(check "boolean-eval" "'(xor (and t t) (or nil nil))" t
       (boolean-eval '(xor (and t t) (or nil nil))))
(check "boolean-eval" "'(implies (and t nil) (or nil t))" t
       (boolean-eval '(implies (and t nil) (or nil t))))

;; --- boolean-eval: our own cases (constants, NOT, IFF, deep nesting) ---
(check "boolean-eval" "t"                    t   (boolean-eval t))
(check "boolean-eval" "nil"                  nil (boolean-eval nil))
(check "boolean-eval" "'(not t)"             nil (boolean-eval '(not t)))
(check "boolean-eval" "'(not (not t))"       t   (boolean-eval '(not (not t))))
(check "boolean-eval" "'(iff (or t nil) (and t t))" t
       (boolean-eval '(iff (or t nil) (and t t))))
(check "boolean-eval" "'(implies t (xor t t))" nil
       (boolean-eval '(implies t (xor t t))))
(check "boolean-eval" "'(or nil (and t (not (xor nil (iff t nil)))))" t
       (boolean-eval '(or nil (and t (not (xor nil (iff t nil)))))))

;; --- boolean-simplify: spec examples ---
(check "boolean-simplify" "'(and t (or nil t))"        t   (boolean-simplify '(and t (or nil t))))
(check "boolean-simplify" "'(not (not (and t nil)))"   nil (boolean-simplify '(not (not (and t nil)))))
(check "boolean-simplify" "'(and (or a nil) t)"        'a  (boolean-simplify '(and (or a nil) t)))
(check "boolean-simplify" "'(or (and a t) nil)"        'a  (boolean-simplify '(or (and a t) nil)))

;; --- boolean-simplify: one test per rule in the spec table ---
(check "boolean-simplify" "'(not (not a))" 'a  (boolean-simplify '(not (not a))))
(check "boolean-simplify" "'(and a t)"     'a  (boolean-simplify '(and a t)))
(check "boolean-simplify" "'(and t a)"     'a  (boolean-simplify '(and t a)))
(check "boolean-simplify" "'(and a nil)"   nil (boolean-simplify '(and a nil)))
(check "boolean-simplify" "'(and nil a)"   nil (boolean-simplify '(and nil a)))
(check "boolean-simplify" "'(or a nil)"    'a  (boolean-simplify '(or a nil)))
(check "boolean-simplify" "'(or nil a)"    'a  (boolean-simplify '(or nil a)))
(check "boolean-simplify" "'(or a t)"      t   (boolean-simplify '(or a t)))
(check "boolean-simplify" "'(or t a)"      t   (boolean-simplify '(or t a)))
(check "boolean-simplify" "'(and a a)"     'a  (boolean-simplify '(and a a)))
(check "boolean-simplify" "'(or a a)"      'a  (boolean-simplify '(or a a)))

;; --- boolean-simplify: structure preserved / nested / other operators ---
(check "boolean-simplify" "'a"             'a  (boolean-simplify 'a))
(check "boolean-simplify" "'(and a b)"     '(and a b) (boolean-simplify '(and a b)))
(check "boolean-simplify" "'(not a)"       '(not a)   (boolean-simplify '(not a)))
(check "boolean-simplify" "'(and (or a a) (and b t))" '(and a b)
       (boolean-simplify '(and (or a a) (and b t))))
(check "boolean-simplify" "'(xor (and a t) (not (not b)))" '(xor a b)
       (boolean-simplify '(xor (and a t) (not (not b)))))
(check "boolean-simplify" "'(and (or a b) (or b a))" '(and (or a b) (or b a))
       (boolean-simplify '(and (or a b) (or b a))))          ; not EQUAL, so no rule applies
(check "boolean-simplify" "'(not (not (not (not a))))" 'a
       (boolean-simplify '(not (not (not (not a))))))

;; --- boolean-simplify must not modify its input ---
(let ((original (list 'and (list 'or 'a nil) t)))
  (boolean-simplify original)
  (check "boolean-simplify (input unchanged)" "(and (or a nil) t)"
         '(and (or a nil) t) original))


;;; ============================================================
;;; LIST PROCESSING (Keith)
;;; ============================================================

;; --- list-reverse ---
(check "list-reverse" "'(1 2 3 4)"   '(4 3 2 1)   (list-reverse '(1 2 3 4)))
(check "list-reverse" "'(1)"         '(1)         (list-reverse '(1)))
(check "list-reverse" "'()"          nil          (list-reverse '()))
(check "list-reverse" "'(a (b c) d)" '(d (b c) a) (list-reverse '(a (b c) d)))  ; sublists stay intact

(let ((original (list 1 2 3)))
  (list-reverse original)
  (check "list-reverse (input unchanged)" "(1 2 3)" '(1 2 3) original))

;; --- list-flatten ---
(check "list-flatten" "'(1 (2 (3 4)) 5)"   '(1 2 3 4 5) (list-flatten '(1 (2 (3 4)) 5)))
(check "list-flatten" "'((1 2) (3 4))"     '(1 2 3 4)   (list-flatten '((1 2) (3 4))))
(check "list-flatten" "'()"                nil          (list-flatten '()))
(check "list-flatten" "'(1 () (2 ()) 3)"   '(1 2 3)     (list-flatten '(1 () (2 ()) 3)))  ; empty sublists vanish
(check "list-flatten" "'((((1))) 2)"       '(1 2)       (list-flatten '((((1))) 2)))       ; deep nesting
(check "list-flatten" "'(1 2 3)"           '(1 2 3)     (list-flatten '(1 2 3)))           ; already flat

;; --- list-remove-duplicates ---
(check "list-remove-duplicates" "'(1 2 2 3 1 4)" '(1 2 3 4) (list-remove-duplicates '(1 2 2 3 1 4)))
(check "list-remove-duplicates" "'(5 5 5)"       '(5)       (list-remove-duplicates '(5 5 5)))
(check "list-remove-duplicates" "'()"            nil        (list-remove-duplicates '()))
(check "list-remove-duplicates" "'(1 2 3)"       '(1 2 3)   (list-remove-duplicates '(1 2 3)))
(check "list-remove-duplicates" "'(a (1 2) b (1 2) a)" '(a (1 2) b)
       (list-remove-duplicates '(a (1 2) b (1 2) a)))       ; EQUAL also matches sublists

;; --- list-palindrome ---
(check "list-palindrome" "'(1 2 3 2 1)" t   (list-palindrome '(1 2 3 2 1)))
(check "list-palindrome" "'(1 2 3)"     nil (list-palindrome '(1 2 3)))
(check "list-palindrome" "'(1)"         t   (list-palindrome '(1)))
(check "list-palindrome" "'()"          t   (list-palindrome '()))
(check "list-palindrome" "'(1 2 2 1)"   t   (list-palindrome '(1 2 2 1)))   ; even length
(check "list-palindrome" "'(a b)"       nil (list-palindrome '(a b)))


;;; ============================================================
;;; MERGE SORT ()
;;; ============================================================

(check "merge-sort" "'(2 1 5 0) #'<" '(0 1 2 5) (merge-sort '(2 1 5 0) #'<))


;;; ============================================================
;;; STUDENT COURSE ANALYZER (Ramin)
;;; ============================================================

; This is just a test to see if find-students works
(check "find-student" "students 102" '(102 (1 92) (3 88)) (find-student *students* 102))
(check "find-student" "students 999" nil (find-student *students* 999))

(check "get-student-courses" "students 101" '(1 2 3) (get-student-courses *students* 101))
(check "get-student-courses" "students 102" '(1 3) (get-student-courses *students* 102))
(check "get-student-courses" "students 999" nil (get-student-courses *students* 999))

(check "common-courses" "students 101 104" '(1 2) (common-courses *students* 101 104))
(check "common-courses" "students 102 103" '(3) (common-courses *students* 102 103))
(check "common-courses" "students 102 104" '(1) (common-courses *students* 102 104))

(check "list-sum" "'(85 90 78)" 253 (list-sum '(85 90 78)))
(check "student-average" "students 101" 253/3 (student-average *students* 101))
(check "student-average" "students 102" 90 (student-average *students* 102))
(check "student-average" "students 103" 85 (student-average *students* 103))
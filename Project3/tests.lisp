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


;;; ============================================================
;;; BOOLEAN LOGIC ()
;;; ============================================================

(check "boolean-xor" "t nil" t (boolean-xor t nil))


;;; ============================================================
;;; LIST PROCESSING ()
;;; ============================================================

(check "list-reverse" "'(1 2 3 4)" '(4 3 2 1) (list-reverse '(1 2 3 4)))


;;; ============================================================
;;; MERGE SORT ()
;;; ============================================================

(check "merge-sort" "'(2 1 5 0) #'<" '(0 1 2 5) (merge-sort '(2 1 5 0) #'<))


;;; ============================================================
;;; STUDENT COURSE ANALYZER (Ramin)
;;; ============================================================

(check "get-student-courses" "students 101" '(1 2 3) (get-student-courses *students* 101))
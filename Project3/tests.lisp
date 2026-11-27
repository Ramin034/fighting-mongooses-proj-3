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

(check "boolean-xor" "t nil" t (boolean-xor t nil))


;;; ============================================================
;;; LIST PROCESSING (Keith)
;;; ============================================================

(check "list-reverse" "'(1 2 3 4)" '(4 3 2 1) (list-reverse '(1 2 3 4)))


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
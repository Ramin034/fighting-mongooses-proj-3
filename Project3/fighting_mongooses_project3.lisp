;; fighting_mongooses_project3.lisp
;; Project 3 - Functional Programming with Common Lisp
;; Team: Fighting Mongooses
;;
;;Unfinished functions return NIL for now so the file still loads.


;;; ============================================================
;;; 3. SET OPERATIONS  (Ramin)
;;; ============================================================

(defun set-member (set item)
  (cond ((not set)nil)                  ; checks if list is empty
    ((equal (car set) item) t)          ; checks if the first element matches
    (t (set-member (cdr set) item))))   ; else check the rest

;; helper function help from claude
(defun set-join (list-1 list-2)
    (cond ((not list-1) list-2)
            (t (cons (car list-1) (set-join (cdr list-1) list-2)))))

(defun set-union (set-1 set-2)
  (set-join set-1 (set-diff set-2 set-1)))

(defun set-intersection (set-1 set-2)
  (cond ((not set-1) nil)                                           ; empty nothing in common
        ((set-member set-2 (car set-1))                             ; first element is in set-2
        (cons (car set-1) (set-intersection (cdr set-1) set-2)))   ;   keep it + check the rest
        (t (set-intersection (cdr set-1) set-2))))                  ; not in set-2 -> skip it

(defun set-diff (set-1 set-2)
  (cond ((not set-1) nil)
        ((not (set-member set-2(car set-1)))                ; first element not in set-2
        (cons (car set-1) (set-diff (cdr set-1) set-2)))    ; keep it
        (t (set-diff (cdr set-1) set-2))))                  ; else skip it

(defun set-symmetric-diff (set-1 set-2)
  (set-join (set-diff set-1 set-2) (set-diff set-2 set-1)))

;;; ============================================================
;;; 4. BOOLEAN LOGIC  ()
;;; ============================================================

(defun boolean-xor (a b)
  nil)

(defun boolean-implies (a b)
  nil)

(defun boolean-iff (a b)
  nil)

(defun boolean-eval (exp)
  nil)

(defun boolean-simplify (exp)
  nil)

;;; ============================================================
;;; 5. RECURSIVE LIST PROCESSING  ()
;;; ============================================================

(defun list-reverse (lst)
  nil)

(defun list-flatten (lst)
  nil)

(defun list-remove-duplicates (lst)
  nil)

(defun list-palindrome (lst)
  nil)


;;; ============================================================
;;; 6. MERGE SORT  ()
;;; ============================================================

(defun merge-sort (lst predicate)
  nil)


;;; ============================================================
;;; 7. STUDENT COURSE ANALYZER  (Ramin)
;;; ============================================================

(defun find-student (students student-id)
    (cond ((not students) nil)                                          ; no students found return nil
            ((equal (car (car students)) student-id) (car students))    ; Id match return student
            (t (find-student (cdr students) student-id))))              ; else check the rest

(defun course-ids (pairs)
(cond ((not pairs) nil)                                                 ; no pairs
        (t (cons (car (car pairs))(course-ids (cdr pairs))))))          ; first course id then rest

(defun get-student-courses (students student-id)
  (course-ids (cdr (find-student students student-id))))

(defun student-average (students student-id)
  nil)

(defun common-courses (students student-1 student-2)
  (set-intersection (get-student-courses students student-1)
                    (get-student-courses students student-2)))

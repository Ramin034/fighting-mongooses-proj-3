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
;;; 4. BOOLEAN LOGIC  (Keith)
;;; ============================================================

(defun boolean-xor (a b)
  (and (or a b)                 ; at least on operand is true
       (not (and a b))))        ; but not both. NOT always returns T or NIL, so the whole AND returns T or NIL.
    (not (and a b))))

(defun boolean-implies (a b)
  (not (and a (not b))))        ; NIL only for a=T, b=NIL; T otherwise.

(defun boolean-iff (a b)
  (not (boolean-xor a b)))        ; T only for a=b, NIL otherwise.

(defun boolean-eval (exp)
  (defun boolean-eval (exp)
  (cond ((equal exp t) t)                      ; base case: constant true
        ((not exp) nil)                        ; base case: constant false (NIL)
        (t
         ;; It's a list like (op x) or (op x y). Pull out the pieces once.
         (let ((op (first exp)))               ; operator symbol, e.g. AND
           (cond
             ;; NOT takes one operand: evaluate it, then flip it.
             ((equal op 'not)
              (not (boolean-eval (second exp))))

             ;; AND: both evaluated operands must be true.
             ((equal op 'and)
              (and (boolean-eval (second exp))
                   (boolean-eval (third exp))))

             ;; OR: at least one evaluated operand must be true.
             ((equal op 'or)
              (or (boolean-eval (second exp))
                  (boolean-eval (third exp))))

             ;; XOR / IMPLIES / IFF: evaluate both operands, then reuse
             ;; the helper functions written above.
             ((equal op 'xor)
              (boolean-xor (boolean-eval (second exp))
                           (boolean-eval (third exp))))
             ((equal op 'implies)
              (boolean-implies (boolean-eval (second exp))
                               (boolean-eval (third exp))))
             ((equal op 'iff)
              (boolean-iff (boolean-eval (second exp))
                           (boolean-eval (third exp))))
            ;; *Added by Claude.*
            ;; Unknown operator. Spec says inputs are well-formed, so
            ;; this should never happen; return NIL to be safe.
            (t nil)))))))

;; *Helper method added by Claude*
(defun bool-compound-p (exp)
  (cond ((equal exp t) nil)            ; constant T is not compound
        ((not exp) nil)                ; constant NIL is not compound
        (t (not (equal (list-flatten (list exp))
                       (list exp))))))

;; *Helper method added by Claude*
(defun bool-simplify-node (exp)
  (if (not (bool-compound-p exp))
      exp                                           ; T / NIL / variable
      (let ((op (first exp)))
        (if (equal op 'not)
            ;; ---------- NOT (one operand) ----------
            (let ((a (bool-simplify-node (second exp))))
              (if (and (bool-compound-p a)          ; operand is itself
                       (equal (first a) 'not))      ; a NOT expression?
                  (second a)                        ; (not (not A)) => A
                  (list 'not a)))                   ; otherwise keep it
            ;; ---------- two-operand operators ----------
            (let ((a (bool-simplify-node (second exp)))   ; simplified left
                  (b (bool-simplify-node (third exp))))   ; simplified right
              (cond
                ;; AND rules
                ((equal op 'and)
                 (cond ((equal a t) b)              ; (and t A)   => A
                       ((equal b t) a)              ; (and A t)   => A
                       ((not a) nil)                ; (and nil A) => NIL
                       ((not b) nil)                ; (and A nil) => NIL
                       ((equal a b) a)              ; (and A A)   => A
                       (t (list 'and a b))))        ; no rule: rebuild
                ;; OR rules
                ((equal op 'or)
                 (cond ((not a) b)                  ; (or nil A) => A
                       ((not b) a)                  ; (or A nil) => A
                       ((equal a t) t)              ; (or t A)   => T
                       ((equal b t) t)              ; (or A t)   => T
                       ((equal a b) a)              ; (or A A)   => A
                       (t (list 'or a b))))         ; no rule: rebuild
                ;; XOR / IMPLIES / IFF have no required rules, so we
                ;; only simplify inside their operands and keep the
                ;; operator as-is (preserves structure).
                (t (list op a b))))))))

(defun boolean-simplify (exp)
  (let ((simplified (bool-simplify-node exp)))
    (if (equal simplified exp)
        simplified                         ; nothing changed: finished
        (boolean-simplify simplified))))   ; something changed: go again

;;; ============================================================
;;; 5. RECURSIVE LIST PROCESSING  (Keith)
;;; ============================================================

(defun list-reverse (lst)
  (labels ((rev-acc (remaining acc)
             (if (not remaining)
                 acc                                   ; base case: done
                 (rev-acc (cdr remaining)              ; recurse on rest
                          (cons (car remaining) acc))))) ; push first onto acc
    (rev-acc lst nil)))                                ; start with empty acc

(defun list-flatten (lst)
  (cond ((not lst) nil)                                ; base case: empty
        ((not (car lst))                               ; first is () / NIL:
         (list-flatten (cdr lst)))                     ;   skip it
        ((atom (car lst))                              ; first is a plain item:
         (cons (car lst) (list-flatten (cdr lst))))    ;   keep it + rest
        (t                                             ; first is a sublist:
         (set-join (list-flatten (car lst))            ;   flattened sublist
                   (list-flatten (cdr lst))))))        ;   + flattened rest

(defun list-remove-duplicates (lst)
  (labels ((walk (remaining seen)
             (cond ((not remaining) nil)                    ; base case
                   ((set-member seen (car remaining))       ; already kept?
                    (walk (cdr remaining) seen))            ;   skip it
                   (t (cons (car remaining)                 ; new: keep it
                            (walk (cdr remaining)
                                  (cons (car remaining) seen))))))) ; remember it
    (walk lst nil)))

(defun list-palindrome (lst)
  (equal lst (list-reverse lst)))


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

(defun course-grades (pairs)
    (cond ((not pairs) nil)
        (t (cons (second (car pairs)) (course-grades (cdr pairs))))))

(defun list-sum (numbers)
    (cond ((not numbers) 0)
    (t (+ (car numbers) (list-sum (cdr numbers))))))

(defun student-average (students student-id)
    (let ((pairs (cdr (find-student students student-id))))
    (/ (list-sum (course-grades pairs))
    (length pairs))))

(defun common-courses (students student-1 student-2)
  (set-intersection (get-student-courses students student-1)
                    (get-student-courses students student-2)))

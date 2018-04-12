(ql:quickload :iterate)
(ql:quickload :alexandria)

(defpackage :sudoku (:use :common-lisp :iterate :alexandria))
(in-package :sudoku)

(defparameter *board* (make-array '(9 9) :element-type 'char))
(defparameter +all-sq-positions+ (map-product 'list
                                              (iota 3 :start 0 :step 3)
                                              (iota 3 :start 0 :step 3)))

(defparameter +all-positions+ (map-product 'list
                                           (iota 9 :start 0)
                                           (iota 9 :start 0)))

(defun x (pos) (first pos))
(defun y (pos) (second pos))

(defun getval (pos)
  (aref *board* (y pos) (x pos)))

(defun setval (pos val)
  (setf (aref *board* (y pos) (x pos)) val))

(defun sq-positions (start)
  (map-product 'list
               (iota 3 :start (x start))
               (iota 3 :start (y start))))

(defun row-positions (pos)
  (map-product 'list
               (iota 1 :start (x pos))
               (iota 9 :start 0)))

(defun col-positions (pos)
  (map-product 'list
               (iota 9 :start 0)
               (iota 1 :start (y pos))))

(defun getvalues (positions)
  (map 'list (lambda (pos) (getval pos)) positions))

(defun complete? (positions)
  (set-equal (iota 9 :start 1) (getvalues positions)))

(defun print-board ()
  (format t "~%")
  (iter (for i from 0 to 8)
        (iter (for j from 0 to 8)
              (format t "~a " (getval (list i j))))
        (format t "~%")))


(defun get-missing-positions (positions)
  (remove-if-not (lambda (pos) (zerop (getval pos))) positions))

(defun get-missing-numbers (positions)
  (set-difference (iota 9 :start 1) (getvalues positions)))

(defun get-missing (pos)
  (let* ((start (bounding-sq-pos pos))
         (positions (union (union (sq-positions start)
                          (row-positions pos))
                       (col-positions pos))))
    (get-missing-numbers positions)))

(defun bounding-sq-pos (pos)
  (list (* 3 (floor (x pos) 3))
        (* 3 (floor (y pos) 3))))


(defun missing-in-sq (pos)
  (let* ((start (bounding-sq-pos pos))
         (missing-sq-positions (get-missing-positions (sq-positions start))))
    (mapcar #'list
            missing-sq-positions
            (map 'list #'get-missing missing-sq-positions))))

(defun appearance (num lst)
  (count num lst))

(defun find-confidant-positions (pos-num-pair)
  (let ((all-numbers (flatten (map 'list #'second pos-num-pair))))
    (iter (for pair in pos-num-pair)
          (appending (get-single-solution pair all-numbers)))))

(defun get-single-solution (pair nums)
  (iter (for n in (second pair))
        (when (or (equal (appearance n nums) 1) (equal (length (second pair)) 1))
          (collect (list (first pair) n)))))

(defun single-occurnce? (pair possible-numbers)
  (iter (for num in (second pair))
        (when (equal (appearance num possible-numbers) 1)
          (list (first pair) num))))

(defun all-filled ()
  (equal 0 (length (remove-if-not #'zerop (getvalues +all-positions+)))))

(defun solved? ()
  (and (not (fail?))
     (all-filled)))

(defun fill-confidant-positions ()
  (let ((changed nil))
    (dolist (position-pair (find-confidant-positions (find-all-missing)))
      (setf changed t)
      (dolist (val (cdr position-pair))
        (setval (first position-pair) val)))
    changed))

(defun fail-position? (positions)
  (iter (for i in (iota 9 :start 1))
        (if (> (appearance i positions)  1)
            (return t)))
  nil)


(defun find-invalid-positions (pos-num-pair)
  ;; (format t "~% ~a" pos-num-pair)
  (cond ((null pos-num-pair) nil)
        ((null (second (first pos-num-pair))) t)
        (t (find-invalid-positions (cdr pos-num-pair)))))

(defun check-fail (positions)
  (cond ((null positions) nil)
        ((fail-position? (row-positions (first positions))) t)
        ((fail-position? (col-positions (first positions))) t)
        ((fail-position? (sq-positions (first positions))) t)
        ((find-invalid-positions (missing-in-sq (first positions)))t)
        (t (check-fail (cdr positions)))))

(defun fail? ()
  (check-fail +all-positions+))

(defun sort-pair (position-pairs)
  (sort position-pairs
        (lambda (a b) (< (length a) (length b)))
        :key #'second)
  position-pairs)

(defun find-all-missing ()
  (iter (for pos in +all-sq-positions+)
        (appending (missing-in-sq pos))))

(defun fill-confidant-position-recursively ()
  (if (fill-confidant-positions)
      (fill-confidant-position-recursively)
      (try-position)))

(defun try-position ()
  (if (not (or (fail?) (all-filled)))
      (dolist (position-pair (sort-pair (find-all-missing)))
        (dolist (val (second position-pair))
          (let ((back-board (copy-array *board*)))
            (setval (first position-pair) val)
            (when (fill-confidant-position-recursively)
              (return-from try-position t))
            (defparameter *board* back-board))))
      (solved?)))

(defun transpose (board)
  (let ((new-board (make-array '(9 9))))
    (iter (for i from 0 to 8)
          (iter (for j from 0 to 8)
                (setf (aref new-board j i)
                      (aref board i j))))
    new-board))

(defun solve-board (board)
  (defparameter *board* board)
  (if (fill-confidant-position-recursively)
      (format t "~%SOLVED:")
      (format t "~%FAILED:"))
  (print-board))


(solve-board (transpose #2A((1 0 5 0 0 0 0 0 4)
                            (0 0 0 7 0 0 2 9 0)
                            (7 2 9 6 0 0 0 0 0)
                            (2 0 0 0 3 0 0 0 6)
                            (4 0 0 0 0 0 0 0 5)
                            (9 0 0 0 8 0 0 0 1)
                            (0 0 0 0 0 2 8 4 7)
                            (0 4 7 0 0 5 0 0 0)
                            (6 0 0 0 0 0 3 0 9))))

;; ============== BOARDS ==============

;; #2A((5 0 0 0 8 0 0 4 9)
;;     (0 0 0 5 0 0 0 3 0)
;;     (0 6 7 3 0 0 0 0 1)
;;     (1 5 0 0 0 0 0 0 0)
;;     (0 0 0 2 0 8 0 0 0)
;;     (0 0 0 0 0 0 0 1 8)
;;     (7 0 0 0 0 4 1 5 0)
;;     (0 3 0 0 0 2 0 0 0)
;;     (4 9 0 0 5 0 0 0 3))

;; #2A((0 0 4 7 2 0 9 0 0)
;;     (0 3 9 0 0 8 0 0 5)
;;     (0 0 1 5 0 6 0 0 4)
;;     (0 4 0 0 1 0 5 2 0)
;;     (0 2 8 0 5 0 1 7 0)
;;     (0 1 6 0 3 0 0 9 0)
;;     (4 0 0 9 0 1 3 0 0)
;;     (1 0 0 3 0 0 8 4 0)
;;     (0 0 7 0 8 5 6 0 0))

;; #2A((0 0 7 0 8 9 5 0 4)
;;     (0 0 0 0 0 0 0 0 0)
;;     (2 0 4 0 0 5 0 3 1)
;;     (0 3 0 0 6 0 0 9 0)
;;     (8 1 0 0 0 0 0 4 7)
;;     (0 9 0 0 2 0 0 5 0)
;;     (6 2 0 7 0 0 8 0 5)
;;     (0 0 0 0 0 0 0 0 0)
;;     (1 0 8 5 3 0 2 0 0))

;; #2A((0 2 0 0 5 0 7 0 0)
;;     (0 0 6 0 0 0 0 0 9)
;;     (0 0 1 7 0 0 0 0 3)
;;     (0 8 0 0 9 0 0 1 0)
;;     (0 0 0 4 6 3 0 0 0)
;;     (0 7 0 0 8 0 0 2 0)
;;     (4 0 0 0 0 8 5 0 0)
;;     (9 0 0 0 0 0 6 0 0)
;;     (0 0 5 0 7 0 0 3 0))

;; #2A((6 0 0 0 0 1 7 2 0)
;;     (1 2 0 0 7 0 4 3 0)
;;     (0 9 0 3 0 0 0 0 0)
;;     (4 0 0 7 0 0 0 0 2)
;;     (7 8 3 0 0 0 9 1 6)
;;     (2 0 0 0 0 3 0 0 7)
;;     (0 0 0 0 0 4 0 6 0)
;;     (0 1 2 0 6 0 0 9 4)
;;     (0 4 6 1 0 0 0 0 5))

;; #2A((0 2 0 6 0 8 0 0 0)
;;     (5 8 0 0 0 9 7 0 0)
;;     (0 0 0 0 4 0 0 0 0)
;;     (3 7 0 0 0 0 5 0 0)
;;     (6 0 0 0 0 0 0 0 4)
;;     (0 0 8 0 0 0 0 1 3)
;;     (0 0 0 0 2 0 0 0 0)
;;     (0 0 9 8 0 0 0 3 6)
;;     (0 0 0 3 0 6 0 9 0))

;; #2A((1 2 3 4 5 6 7 0 0)
;;     (4 5 6 7 0 0 1 2 3)
;;     (7 0 0 1 2 3 4 5 6)
;;     (2 3 4 5 6 7 0 0 1)
;;     (5 6 7 0 0 1 2 3 4)
;;     (0 0 1 2 3 4 5 6 7)
;;     (3 4 5 6 7 0 0 1 2)
;;     (6 7 0 0 1 2 3 4 5)
;;     (0 1 2 3 4 5 6 7 0))

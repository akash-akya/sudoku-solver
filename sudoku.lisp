(ql:quickload :iterate)
(ql:quickload :alexandria)

(defpackage :sudoku (:use :common-lisp :iterate :alexandria))
(in-package :sudoku)

(defparameter *board* (make-array '(9 9) :element-type 'char))
(defparameter +all-positions+ (map-product 'list
                                           (iota 3 :start 0 :step 3)
                                           (iota 3 :start 0 :step 3)))


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

(iter (for i from 0 to 8)
      (iter (for j from 0 to 8)
            (setf (aref *board* i j) (1+ i))))


(defun get-missing-positions (positions)
  (remove-if-not (lambda (pos) (zerop (getval pos))) positions))

(defun get-missing-numbers (positions)
  (set-difference (iota 9 :start 1) (getvalues positions)))

(defun get-missing (pos)
  (let* ((start (bounding-sq-pos pos))
         (positions (union (union (sq-positions start)
                          (row-positions pos))
                       (col-positions pos))))
    ;; (format t "~a~%" positions)
    (get-missing-numbers positions)))
    ;; (values (get-missing-numbers positions) (get-missing-positions positions))))

(defun bounding-sq-pos (pos)
  (list (* 3 (floor (x pos) 3))
        (* 3 (floor (y pos) 3))))

;; (map 'list #'get-missing (bounding-sq-pos '(7 5)))

(defun missing-in-sq (pos)
  (let* ((start (bounding-sq-pos pos))
         (missing-sq-positions (get-missing-positions (sq-positions start))))
    ;; (format t "~% missing-sq-positions: ~a~% numbers: ~a"
    ;;         missing-sq-positions
    ;;         (map 'list #'get-missing missing-sq-positions))
    (mapcar #'list
            missing-sq-positions
            (map 'list #'get-missing missing-sq-positions))))

(defun appearance (num lst)
  (count num lst ))

(defun find-confidant-positions (pos-num-pair)
  (let ((all-numbers (flatten (map 'list #'second pos-num-pair))))
    (iter (for pair in pos-num-pair)
          (appending (get-single-solution pair all-numbers)))))

(defun get-single-solution (pair nums)
  (iter (for n in (second pair))
        (when (equal (appearance n nums) 1)
          (collect (list (first pair) n)))))

(defun single-occurnce? (pair possible-numbers)
  (iter (for num in (second pair))
        (when (equal (appearance num possible-numbers) 1)
          (list (first pair) num))))


(defun fill-confidant-positions ()
  (iter (for pos in +all-positions+)
        ;; (format t "~%~a" (find-confidant-positions (missing-in-sq pos)))
        (let ((position-pair-list (find-confidant-positions (missing-in-sq pos))))
          (dolist (position-pair position-pair-list) 
            (dolist (val (cdr position-pair))
              (setval (first position-pair) val)))
          (appending position-pair-list))))

(defun fill-confidant-position-recursively ()
  (when (consp (fill-confidant-positions))
    (fill-all-confidant-position)))


(defun reset-board ()
  (defparameter *board* (make-array '(9 9)))
  (setf *board* #2A((6 0 0 0 0 1 7 2 0)
                    (1 2 0 0 7 0 4 3 0)
                    (0 9 0 3 0 0 0 0 0)
                    (4 0 0 7 0 0 0 0 2)
                    (7 8 3 0 0 0 9 1 6)
                    (2 0 0 0 0 3 0 0 7)
                    (0 0 0 0 0 4 0 6 0)
                    (0 1 2 0 6 0 0 9 4)
                    (0 4 6 1 0 0 0 0 5))))

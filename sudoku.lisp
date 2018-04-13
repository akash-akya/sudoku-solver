(ql:quickload :iterate)
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defpackage :sudoku (:use :common-lisp :iterate :alexandria :cl-ppcre))
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


(defparameter *print-board* "╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗
║ 100 │ 101 │ 102 ║ 103 │ 104 │ 105 ║ 106 │ 107 │ 108 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 109 │ 110 │ 111 ║ 112 │ 113 │ 114 ║ 115 │ 116 │ 117 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 118 │ 119 │ 120 ║ 121 │ 122 │ 123 ║ 124 │ 125 │ 126 ║
╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
║ 127 │ 128 │ 129 ║ 130 │ 131 │ 132 ║ 133 │ 134 │ 135 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 136 │ 137 │ 138 ║ 139 │ 140 │ 141 ║ 142 │ 143 │ 144 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 145 │ 146 │ 147 ║ 148 │ 149 │ 150 ║ 151 │ 152 │ 153 ║
╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
║ 154 │ 155 │ 156 ║ 157 │ 158 │ 159 ║ 160 │ 161 │ 162 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 163 │ 164 │ 165 ║ 166 │ 167 │ 168 ║ 169 │ 170 │ 171 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 172 │ 173 │ 174 ║ 175 │ 176 │ 177 ║ 178 │ 179 │ 180 ║
╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝")

(defun get-row (row-num)
  (iter (for i from 0 to 8)
        (collect (getval (list row-num i)))))

(defun get-all ()
   (iter (for i from 0 to 8)
         (appending (get-row i))))

(defun pretty-print-board ()
  (let ((pb (copy-array *print-board*))
        (indices (iota 81 :start 100)))
    (labels ((to-string (x)
               (format nil "~a" (if (zerop x) " " x)))
             (set-value (i v)
               (setf pb (regex-replace (to-string i) pb (to-string v)))))
      (mapc #'set-value  indices (get-all)))
    (format t "~%~a" pb)))

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

(defun all-filled? ()
  (equal 0 (length (remove-if-not #'zerop (getvalues +all-positions+)))))

(defun solved? ()
  (and (not (fail?))
     (all-filled)))

(defun get-status ()
  (cond ((fail?) 'fail)
        ((all-filled?) 'solved)
        (t 'incomplete)))

(defun fill-confidant-positions ()
  (let ((changed nil))
    (dolist (position-pair (find-confidant-positions (find-all-missing)))
      (setf changed t)
      (dolist (val (cdr position-pair))
        (setval (first position-pair) val)))
    changed))

(defun fail-position? (positions)
  (iter (for i in (iota 9 :start 1))
        (if (> (appearance i (getvalues positions))  1)
            (return-from fail-position? t)))
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
        ((fail-position? (sq-positions (bounding-sq-pos (first positions)))) t)
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
  (let ((status (get-status)))
    (cond ((eq status 'fail) nil)
          ((eq status 'solved) t)
          ((fill-confidant-positions)
           (fill-confidant-position-recursively))
          (t (try-position)))))

(defun try-position ()
  (let ((position-pair (first (sort-pair (find-all-missing)))))
    (dolist (val (second position-pair))
      (setval (first position-pair) val)
      (progn
        (setval (first position-pair) 0)
        (let ((back-board (copy-array *board*)))
          (setval (first position-pair) val)
          (cond ((fill-confidant-position-recursively)
                 (return-from try-position t)))
          (defparameter *board* back-board)))
      (setval (first position-pair) 0)))
  nil)

(defun transpose (board)
  (let ((new-board (make-array '(9 9))))
    (iter (for i from 0 to 8)
          (iter (for j from 0 to 8)
                (setf (aref new-board j i)
                      (aref board i j))))
    new-board))

(defun solve-board (board)
  (defparameter *board* board)
  (format t "~%INPUT: ")
  (pretty-print-board)
  (if (fill-confidant-position-recursively)
      (format t "~%~%SOLVED:")
      (format t "~%~%FAILED:"))
  (pretty-print-board))


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


;; == HARD ==

;; #2A((8 0 0 0 0 0 0 0 0)
;; (0 0 3 6 0 0 0 0 0)
;; (0 7 0 0 9 0 2 0 0)
;; (0 5 0 0 0 7 0 0 0)
;; (0 0 0 0 4 5 7 0 0)
;; (0 0 0 1 0 0 0 3 0)
;; (0 0 1 0 0 0 0 6 8)
;; (0 0 8 5 0 0 0 1 0)
;; (0 9 0 0 0 0 4 0 0))

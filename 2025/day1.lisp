;;; Advent of Code: Day1
;;; Copyright 2025 Ethan Smith, all rights reserved.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 1

(defun safe-dial (instruction-stream)
  "every read-line on instruction stream should yield a line in the following
format: XN, where X is L or R and N is a number."
  (declare (optimize (debug 3)))
  (do ((dial-pos 50) 
       (zeroes 0)
       (instruction (read-line instruction-stream) (read-line instruction-stream nil)))
      ((or (not instruction) (equal instruction "")) zeroes)
    (let ((number (parse-integer instruction :start 1)))
      (if (eql #\L (elt instruction 0)) 
          (setf number (* number -1)))

      (setf dial-pos (mod (+ dial-pos number)
                          100))
      (if (= dial-pos 0)
        (incf zeroes)))))

;;; TEST INPUT
(assert (= 3 (safe-dial
              (make-string-input-stream "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"))))

;;; PART1 SOLUTION
(with-open-file (cmds #P"./day1-input.txt")
  (safe-dial cmds)) ;; => 1052 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PART 2

(defun safe-dial2 (instruction-stream)
  "every read-line on instruction stream should yield a line in the following
format: XN, where X is L or R and N is a number."
  (declare (optimize (debug 3)))
  (do ((dial-pos 50) 
       (zeroes 0)
       (instruction (read-line instruction-stream) (read-line instruction-stream nil)))
      ((or (not instruction) (equal instruction "")) zeroes)
    (let ((number (parse-integer instruction :start 1)))
      (setf zeroes (+ zeroes (floor (/ number 100))))

      (setf number (mod number 100))
      (setf dial-pos (+ dial-pos (if (eql #\L (elt instruction 0)) 
                                     (- number)
                                     number)))

      (if (or (and (>= 0 dial-pos) (> (+ dial-pos number) 0))
              (< 99 dial-pos))
          (incf zeroes))

      (setf dial-pos (mod dial-pos 100))
      ;(format t "~&instruction: ~A | Dial position: ~A | Zeroes: ~A~%" instruction dial-pos zeroes)
      )))

;;; TEST INPUT
(assert (= 6 (safe-dial2
              (make-string-input-stream "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"))))

;;; PART2 SOLUTION
(with-open-file (cmds #P"./day1-input.txt")
  (safe-dial2 cmds)) ;; => 6295

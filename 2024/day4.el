;;; day4.el --- Advent of Code Solutions -*- lexical-binding: t; -*-
;;; Copyright (C) 2024 Ethan Smith <ethansmith.dev@gmail.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMON

(defun get-at (x y)
  "assume buffer contains a square grid.  return character at x,y"
  (char-after (+ x (* y (1+ (side-length))) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1

(defun side-length ()
  "assume current buffer contains square grid, returns grid size."
  (count-lines 1 (buffer-end 1)))

(defun diagonal-string (diag &optional rightp)
  "assumes current buffer contains square grid.  returns a string of
the text along a diagonal. (0,0) is the longest diagonal,
negative diag moves down, positive diag moves laterally.  if
rightp is true, the diagonal will start in the top right instead
of the top left."
  (apply 'string (cl-loop for x from (max 0 diag) below (side-length)
                          for y from (min 0 diag) above (- (side-length))
                          collect (if rightp
                                      (get-at (- (side-length) 1 x) (- y))
                                    (get-at x (- y))))))

(defun row-string (row)
  "assume current buffer contains square grid.  return a string of the row."
  (let ((row-pos (+ 1 (* row (1+ (side-length))))))
    (buffer-substring row-pos (+ row-pos (side-length)))))

(defun column-string (column)
  "assume current buffer contains square grid.  return a string of the column"
  (apply 'string
         (cl-loop for n from 0 to (1- (side-length))
                  collect (char-after (+ (* n (1+ (side-length))) column 1)))))

(defun string-search-all (needle haystack)
  "returns the number of times needle occures in haystack"
  (let ((start-pos 0)
        (accum 0)
        result)
    (while (setf result (string-search needle haystack start-pos))
      (setf accum (+ 1 accum)
            start-pos (+ result 1)))
    accum))

(defun part1 (file)
  (with-temp-buffer
    (insert-file file)
    (cl-loop for n from (- (1- (side-length))) to (1- (side-length))
             sum (+ (string-search-all "XMAS" (diagonal-string n))
                    (string-search-all "SAMX" (diagonal-string n))
                    (string-search-all "XMAS" (diagonal-string n t))
                    (string-search-all "SAMX" (diagonal-string n t)))
             if (>= n 0) sum (+ (string-search-all "XMAS" (row-string n))
                                (string-search-all "SAMX" (row-string n))
                                (string-search-all "XMAS" (column-string n))
                                (string-search-all "SAMX" (column-string n))))))

(insert (format "\n;; %s" (part1 "day4-input.txt")))
;; 2524

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2

(defun is-xmas (x y)
  "check if the elements catty-corner to x and y will spell
MAS (forwared or backwared)"
  (cl-loop for n from -1 to 1
           collect (get-at (+ x n) (+ y n)) into l
           collect (get-at (- x n) (+ y n)) into r
           finally return (let ((left (apply 'string l))
                                (right (apply 'string r)))
                            (and (or (equal "MAS" left)
                                     (equal "SAM" left))
                                 (or (equal "MAS" right)
                                     (equal "SAM" right))))))

(defun part2 (file)
  (with-temp-buffer
    (insert-file file)
    (let ((accum 0))
      (dotimes (x (- (side-length) 2))
        (dotimes (y (- (side-length) 2))
          (when (is-xmas (1+ x) (1+ y))
            (setf accum (1+ accum)))))
      accum)))

(with-temp-buffer
  (insert-file "test-input.txt")
  (is-xmas 2 1))

(insert (format "\n;; %d" (part2 "./day4-input.txt")))
;; 1873

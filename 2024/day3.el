;;; day3.el --- Advent of Code Solutions -*- lexical-binding: t; -*-
;;; Copyright (C) 2024 Ethan Smith <ethansmith.dev@gmail.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1

(defvar +instruction-rx-sexp+ '(seq "mul("
                                    (group-n 1 (** 1 3 digit))
                                    ","
                                    (group-n 2 (** 1 3 digit))
                                    ")"))

(defvar +instruction-rx+ (rx-to-string +instruction-rx-sexp+)
  "regex for part1 digit")

(defun part1 (file)
  "find all mul(X,Y) instructions, where X and Y are 1-3 digits long"
  (with-temp-buffer
    (insert-file file)
    (let (x y (accum 0))
      (while (re-search-forward +instruction-rx+ nil t)
        (setf x (read (match-string 1))
              y (read (match-string 2))) 

        (setf accum (+ accum (* x y))))

      accum)))

(insert (format "\n;; %s" (part1 "./day3-input.txt")))
;; 168539636

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2

(defvar +conditional-rx-sexp+ '(seq (group-n 1 (or "do" "don't")) "()"))
(defvar +conditional-rx+ (rx-to-string +conditional-rx-sexp+))

(defvar +part2-rx+ (rx-to-string `(or ,+conditional-rx-sexp+
                                      ,+instruction-rx-sexp+))
  "combination of both conditional and instruction rx")

(defun part2 (file)
  "do() enables instructions, don't() disables instructions."
  (with-temp-buffer
    (insert-file file)
    (let (x y (accum 0) (enabledp t))
      (while (re-search-forward +part2-rx+ nil t)
        (cond ((equal (match-string 1) "do") (setf enabledp t))
              ((equal (match-string 1) "don't") (setf enabledp nil))
              (enabledp
               (setf x (read (match-string 1))
                     y (read (match-string 2)))
               (setf accum (+ accum (* x y))))))
      accum)))

(insert (format "\n;; %s" (part2 "./day3-input.txt")))
;; 97529391

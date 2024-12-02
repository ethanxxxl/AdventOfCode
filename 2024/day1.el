;;; day1.el --- Advent of Code Solutions -*- lexical-binding: t; -*-
;;; Copyright (C) 2024 Ethan Smith <ethansmith.dev@gmail.com>

;; pair smallest numbers, then find out how far appart they are.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMON

(defvar *input-file* "./day1-input.txt")
(defvar *list1* nil)
(defvar *list2* nil)

(defun set-lists ()
  "reads the day1 input file.  sets `*list1*' and `list2*' to a sexp
containing all elements of column 1 and column 2 respectively."
  (let ((columns (with-temp-buffer
                   ;; create an s-expression out of both columns
                   (insert "(")
                   (insert-file-contents *input-file*)
                   (goto-char (buffer-end 1))
                   (insert ")")

                   (goto-char 0)
                   (read (get-buffer (buffer-name)))))
        (list1)
        (list2))

    (while columns
      ;; every other element goes to either list1 or list2
      (setf list1 (cons (car columns) list1)
            list2 (cons (cadr columns) list2))
      (setf columns (cddr columns)))
    
    (setf *list1* list1
          *list2* list2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 1

(defun main1 ()
  (set-lists)
  (apply '+ (cl-map 'list (lambda (x y) (abs (- x y)))
                      (sort *list1* '<)
                      (sort *list2* '<))))

(insert (format "\n;; %d" (main1)))
;; 1506483

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defun similarity-score (num right-list)
  "multiplies num by the number of times it appears in the right list."
  (* num
     (length (seq-filter (lambda (n) (= num n)) right-list))))

(setf *list1* (list 3 4 2 1 3 3)
      *list2* (list 4 3 5 3 9 3))

(defun main2 ()
  (set-lists)
  (apply '+ (mapcar (lambda (n) (similarity-score n *list2*)) *list1*)))

(insert (format "\n;; %d" (main2)))
;; 23126924

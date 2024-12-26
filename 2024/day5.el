;;; day5.el --- Advent of Code Solutions -*- lexical-binding: t; -*-
;;; Copyright (C) 2024 Ethan Smith <ethansmith.dev@gmail.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMON

(defun get-rules (file)
  "return a list of rules, where each rules is a cons cell: X|Y -> '(X . Y)"
  (with-temp-buffer
    (insert-file file)
    (cl-loop while (re-search-forward "\\(.+\\)|\\(.+\\)" nil t) 
             collect (cons (read (match-string 1)) (read (match-string 2))))))


(defun read-update ()
  (cl-loop while (re-search-forward (rx (group (+ digit))) (line-end-position) t)
           collect (read (match-string 1))))

(defun get-updates (file)
  "returns a list of updates, where each update is a list of pages
A,B,C.. -> (A B C ...)"
  (with-temp-buffer
    (insert-file file)
    (goto-char (search-forward ","))
    (beginning-of-line)

    (cl-loop repeat (count-lines (point) (buffer-end 1))
             collect (read-update)
             do (beginning-of-line 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1

(defun rule-before (rule)
  "returns the value that must come first, defined by the rule"
  (car rule))

(defun rule-after (rule)
  "returns the value that must come after, defined by the rule"
  (cdr rule))

(defun breaks-rulep (updates rule)
  "given updates (A B C D ...), return the broken rule if the update is contrary to
A->B->C->D..."
  (when updates
    (cl-loop for subsequent-updates in (cdr updates)
             ;; subsequent-updates|(car updates) == B|A, updates break rule
             if (and (= (rule-before rule) subsequent-updates)
                     (= (rule-after rule) (car updates)))
             return rule

             ;; recursively check rules
             finally return (breaks-rulep (cdr updates) rule))))

(defun breaks-rulesp (updates rules)
  (let (broken-rule)
    (cl-loop for rule in rules
             if (setf broken-rule (breaks-rulep updates rule))
             return broken-rule)))

(defun part1 (file)
  (cl-loop for update in (get-updates file)
           unless (breaks-rulesp update (get-rules file))
           sum (nth (/ (length update) 2) update)))

(insert (format "\n;; %d" (part1 "day5-input.txt")))  ;; oof that was a bit slow
;; 4637

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2

(defun should-flipp (update rules)
  "returns true if first two elements of update violate some rule"
  (cl-loop for rule in rules
           if (and (= (rule-before rule) (nth 1 update))
                   (= (rule-after rule) (nth 0 update)))
           return t))

(defun fix-update-round (update rules)
  "flips first two elements of rule if necessary.  `update' is modified."
  (when (cdr update)
    (if (should-flipp update rules)
        ;; swap cars
        (let ((tmp (car update)))
          (setcar update (cadr update))
          (setcar (cdr update) tmp)))
    (fix-update (cdr update) rules))
  update)

(defun fix-update (update rules)
  "when a rule is moved earlier in the chain by flipping, it may
need to continue moving back.  This function is a lazy way to
ensure this is true.  It checks the entire until no changes are
made.  This is the result."
  (let ((last-update (copy-sequence update)))
    (fix-update-round update rules)
    (while (not (equal last-update update))
      (setf last-update (copy-sequence update))
      (fix-update-round update rules))
    update))

(defun part2 (file)
  (let ((rules (get-rules file)))
    (cl-loop for update in (get-updates file)
             when (breaks-rulesp update rules)
             sum (nth (/ (length update) 2) (fix-update update rules)))))

(insert (format "\n;; %d" (part2 "day5-input.txt"))) ;; oof, even slower
;; 6370

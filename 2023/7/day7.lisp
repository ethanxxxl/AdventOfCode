(defparameter *part1-cards* "AKQJT98765432")
(defparameter *part2-cards* "AKQT98765432J")
(defparameter *part1?* nil)

(defun hand-type (hand)
  "returns the type of hand represented as an integer. higher value hands
are higher value numbers.

if the `*part1?*' global variable is nil, then the part two hand ranking rules are applied"
  (let ((cards (map 'list (lambda (c)
			    (count c hand))
		    (if *part1?*
			*part1-cards*
			*part2-cards*))))

    ;; part two rules. add wilds to card type with most number of
    ;; instances in hand (except the Joker)
    (unless *part1?*
      (incf (nth (position (apply #'max (butlast cards)) cards) cards)
	    (car (last cards)))
      (setf (car (last cards)) 0))
    
    (cond ((find 5 cards)        6)     ; 'five-of-a-kind
	  ((find 4 cards)        5)     ; 'four-of-a-kind
	  ((and (find 3 cards)	        ;
		(find 2 cards))  4)     ; 'full-house
	  ((find 3 cards)        3)     ; 'three-of-a-kind
	  ((= 2 (count 2 cards)) 2)     ; 'two-pair
	  ((find 2 cards)        1)     ; 'one-pair
	  (t                     0))))  ; 'high-card

(defun card-< (c1 c2)
  (let ((cards (if *part1?* *part1-cards* *part2-cards*)))
    (> (position c1 cards) (position c2 cards))))

(defun hand-< (hand1 hand2)
  (declare (type string hand1 hand2))
  (let ((h1 (hand-type hand1))
	(h2 (hand-type hand2)))
    (if (eql h1 h2)
	(loop for c1 across hand1
	      for c2 across hand2
	      when (not (eql c1 c2))
		return (card-< c1 c2))
	(< h1 h2))))

(defun get-cards-from-file (&optional (file "input.txt"))
  (with-open-file (f file)
    (loop as line = (read-line f nil)
	  while line
	  collect (list (subseq line 0 (position #\space line))
			(parse-integer line :start
				       (position #\space line))))))
;; PART ONE AND TWO SOLUTION
(defun solution (&optional (file "input.txt"))
  "when `*part1?*' is t, part one is run. if it is nil, part2 is run."
  (loop for h in (sort (get-cards-from-file file) #'hand-< :key #'first)
	for y from 1
	summing (* y (second h))))

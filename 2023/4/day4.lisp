(defun card-numbers (card &optional winners?)
  "returns a list of numbers that you have on the card"
  (let ((colon (1+ (position #\: card)))
	(pipe (1+ (position #\| card))))
    (loop with next = (if winners? pipe colon)
	  with num
	  do (setf (values num next)
		   (parse-integer card :start next
				       :end (if winners? nil pipe)
				       :junk-allowed t))
	  while num
	  collect num)))

(defun num-winners (nums winning-nums)
  "returns the number of nums that are in winning-nums"
  (reduce #'+ (mapcar (lambda (n) (if (find n winning-nums) 1 0))
		      nums)))

(defun calc-score (n)
  "given n wins, score will be 1 for a single win, and doubled for
 every additional win."
  (if (> n 0)
      (expt 2 (1- n))
      0))

(defun card-score (card)
  "returns the score of the card"
  (calc-score (num-winners (card-numbers card)
			   (card-numbers card t))))


(defun cards-from-file (&optional (file "input.txt"))
  "returns a list in the format '((card0 n) (card1 n) ...) where n is the
number of the associated card (1 by default), and card is the amount
of winning numbers on that card."
  (with-open-file (f file)
    (loop as current-card = (read-line f nil)
	  while current-card
	  collect (list (num-winners (card-numbers current-card)
				     (card-numbers current-card t))
			1))))

;; PART 1 SOLUTION
(defun sum-file (&optional (file "input.txt"))
  "sums `file' depending on the selected part."
  (with-open-file (f file)
    (loop as current-card = (read-line f nil)
	  while current-card
	  summing (card-score current-card))))

;; PART 2 SOLUTION
(defun win-new-cards (card-list &optional (accum 0))
  "`card-list' should be in the format of `cards-from-file'.
recursively increments subsequent cards by the amount of copies of the
first card. The number of copies of the first card is accumulated."

  (if card-list
    (let ((this-wins (caar card-list))
	  (this-copies (cadar card-list))
	  (next-cards (cdr card-list)))
      
      (win-new-cards (append
		      (mapcar (lambda (card)
				(list (first card)
				      (+ this-copies (second card))))
			      (subseq next-cards 0 this-wins))
		      (nthcdr this-wins next-cards))
		     (+ accum this-copies)))
    accum))

(defun next-number (line idx)
  "Returns three values:
    1. next number
    2. start index of that number in line
    3. end index of that number in line

if idx is NIL, nothing is done (equivalent to idx = (length line))"
  (and idx 
       (let ((num-idx (position-if #'digit-char-p line :start idx)))
	 (and num-idx
	      (values (parse-integer line :start num-idx :junk-allowed t)
		      num-idx
		      (position-if-not #'digit-char-p line :start num-idx))))))

(defun symbol-in-region? (beg end line)
  "returns true if there is a symbol in the region spanning beg end"
  (find-if-not (lambda (c)
		 (or (digit-char-p c)
		     (eql c #\.)))
	       line :start beg :end end))

(defun symbol-adjacent? (beg end line prev next)
  "returns true if there is a symbol adjacent to the region beg end.
line, prev, and next are assumed to be the same length."
  (let ((~beg (max 0 (1- beg)))
	(~end (min (length line) (1+ (or end (length line))))))
    (some (lambda (l) (symbol-in-region? ~beg ~end l))
	  (remove nil (list prev line next)))))

(defun sum-line (line prev next)
  "returns the sum of valid parts on line"
  (loop with num = nil
	with beg = 0
	with end = nil
	do (setf (values num beg end) (next-number line beg))
	while beg
	summing (if (symbol-adjacent? beg end line prev next)
		    num 0)
	do (setf beg end)))

(defun get-number-at (line idx)
  "returns the entire number which is at index. idx need not point to the beginning
of the number."
  (when (digit-char-p (char line idx))
    (parse-integer line
		   :start (1+ (or (position-if-not #'digit-char-p line
						   :end idx :from-end t)
				  -1))
		   :end (position-if-not #'digit-char-p line :start idx))))

(defun adjacent-numbers (line idx)
  "returns a list of numbers adjacent to idx. could be a list of one or two
numbers. This function can be applied to all three lines for a gear."
  (remove nil (loop for i from (max (1- idx) 0) to (1+ idx)
		    collect (multiple-value-bind (num end)
				(get-number-at line i)
			      (when end (setf i end))
			      num))))

(defun gear-ratio (idx curr prev next)
  "if the gear at idx has exactly two numbers, their product is
returned. otherwise, zero is returned."
  (let ((nums (append (adjacent-numbers prev idx)
		      (adjacent-numbers curr idx)
		      (adjacent-numbers next idx))))
    (if (= 2 (length nums))
	(reduce #'* nums)
	0)))

(defun sum-line-gears (curr prev next)
  "sum all the valid gears on the current (curr) line."
  (declare (optimize (debug 3)))
  (loop as ~g = 0 then (1+ g)
	as g = (position #\* curr :start ~g)
	while g
	summing (gear-ratio g curr prev next)))

(defun sum-file (&key (file "day3input.txt") part2)
  "sums contents of `file'. will run the algorithm for part 2 if `part2' is t."
  (with-open-file (f file)
    (loop as previous-line = nil then current-line
	  as current-line  = (read-line f nil) then next-line
	  as next-line     = (read-line f nil)
	  while current-line
	  summing (funcall (if part2 #'sum-line-gears #'sum-line)
			   current-line previous-line next-line))))

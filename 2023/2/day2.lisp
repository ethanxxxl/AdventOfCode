(defun validate-game (game)
  "returns the game id if game is valid, nil otherwise"
  (let ((game-id (parse-integer game :start 5 :junk-allowed t))
	(first-thing (1+ (position #\: game))))
    (do* ((beg first-thing (if end (1+ end) (length game)))
	  (end (next-thing game first-thing) (next-thing game beg))
	  (valid game-id))
	 ((<= (length game) beg) valid)
      (setq valid (and (validate-thing (subseq game beg end))
		       valid)))))

(defun next-thing (game current-thing)
  (let ((d1 (position #\, game :start current-thing))
	(d2 (position #\; game :start current-thing)))
    (if (and d1 d2)
	(min d1 d2)
	(or d1 d2))))

(defun validate-thing (thing)
  (let ((num (parse-integer thing :junk-allowed t))
	(color (remove-if-not (lambda (c) (search c thing))
			      '("red" "green" "blue"))))
    (case (car color)
      ("red"   (>= 12 num))
      ("green" (>= 13 num))
      ("blue"  (>= 14 num)))))

(defun parse-thing (thing)
  (let ((num (parse-integer thing :junk-allowed t))
	(color (remove-if-not (lambda (c) (search c thing))
			      '("red" "green" "blue"))))
    (list num (car color))))

(defun parse-game (game)
  (let ((len (length game))
	(start (1+ (position #\: game))))

    (loop as beg = start then (if end (1+ end) len)
	  as end = (next-thing game start) then (next-thing game beg)
	  while (> len beg)
	  collect (parse-thing (subseq game beg end)))))

(defun min-cubes (cubes)
  (mapcar
   (lambda (rgb)
     (apply #'max
	    (mapcar #'car
		    (remove-if-not (lambda (c) (equal c rgb))
				   cubes :key #'second))))
   '("red" "green" "blue")))

(defun game-power (cubes)
  (reduce #'* (min-cubes cubes)))

;; PART 1 SOLUTION
(defun add-up-valid (&optional (filename "./day2input.txt"))
  (with-open-file (f filename)
    (loop as game = (read-line f nil)
	  while game
	    summing (or (validate-game game) 0) into c do
	  (format t "~&~%~S ~S:  ~S"  c (validate-game game) game))))

;; PART 2 SOLUTION
(defun add-up-powers (&optional (filename "./day2input.txt"))
  (with-open-file (f filename)
    (loop as game = (read-line f nil)
	  while game
	  summing (game-power (parse-game game)))))

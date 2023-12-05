(defun get-number (line)
  (declare (type string line))
  (let* ((cleaned (remove-if-not (lambda (s)
				   (parse-integer s :junk-allowed t))
				 line :key 'string))
	 (last-index (1- (array-dimension cleaned 0))))
    (parse-integer (coerce (list (elt cleaned 0)
				 (elt cleaned last-index))
			   'string))))

(defun get-number2 (line)
  (declare (type string line))
  (let ((digits (get-digits line)))
    (+ (* 10 (first digits))
       (car (last digits)))))

(defun get-digits (line)
  (mapcar #'first
	  (sort
	   (remove nil
		   (apply #'concatenate 'list 
			  (mapcar (lambda (substr value)
				    (list (list value (search substr line))
					  (list value (search substr line
							      :from-end t))))
				  '("one" "two" "three" "four" "five"
				    "six" "seven" "eight" "nine"
				    "1" "2" "3" "4" "5" "6" "7" "8" "9")
				  '(1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9)))
		   :key #'second)
	   #'< :key #'second)))

;; PART 2 SOLUTION
(defun read-numbers (filename)
  (with-open-file (s filename)
    (do ((line (read-line s) (read-line s nil))
	 (b 0)
	 (accum 0 (+ accum (get-number2 line))))
	((not line) accum)
      (format t "~&~S => ~A | ~A" line (get-number2 line) accum))))

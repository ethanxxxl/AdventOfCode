
(defun list<-nums (str)
  "returns a list of numbers from a string containing space separated
numbers."
  (loop with num = nil
	with start = 0
	do (setf (values num start)
		 (parse-integer str :start start
				    :junk-allowed t))
	while num
	collect num))

(defun file-reader (&optional (file "input.txt"))
  "reads the file and returns a list of maps in the following format:
"

  (with-open-file (f file)
    (loop as line = (read-line f nil)
	  while line
	  collect line)))

(defun seeds (lines)
  "returns list of seeds (first line)"
  (list<-nums (subseq (first lines)
		      (1+ (position #\: (first lines))))))

(defun get-maps-reversed (lines &optional maps)
  "recursively loops through file and builds a list of maps in the
following format:

(((n1 n2 n3)  ; map 1
  (n1 n2 n3)
  (n1 n2 n3)
  ...)
 ((k1 k2 k3)  ; map 2
  (k1 k2 k3)
  (k1 k2 k3)
  ...)
 ...)

THE MAPS ARE IN REVERSED ORDER FROM THE FILE"

  (cond
    ((not lines) maps) ; exit condition
    ((find #\: (first lines))
     (let ((map (get-map (cdr lines))))
       (get-maps-reversed (nthcdr (length map) lines)
			  (cons map maps))))
    (t
     (get-maps-reversed (cdr lines) maps))))

(defun get-maps (lines)
  "gets the maps, but in the correct order"
  (reverse (get-maps-reversed lines)))

(defun get-map (lines)
  (loop for line in lines
	as map-part = (list<-nums line)
	while map-part
	collect map-part))

(defun next<-seed (this map)
  "transforms this to that using the map algorithm"
  (or
   (loop for m in map
	 as dst = (first m)
	 as src = (second m)
	 as rng = (1- (third m))
	 when (progn ;(format t "~&<= ~S ~S ~S" src this (+ src rng))
		     (<= src this (+ src rng)))
           
	   return (+ dst (- this src)))
      this))

(defun location<-seed (seed maps)
  (declare (optimize (speed 3)))
  "recursivly applies map to `that' and returns the result"
  (if maps
      (location<-seed (next<-seed seed (first maps)) (cdr maps))
      seed))

;; PART 1 SOLUTION
(defun part-one-solution (&optional (file "input.txt"))
  (let* ((f     (file-reader file))
	 (seeds (seeds f))
	 (maps  (get-maps (cdr f))))
    (apply #'min (mapcar (lambda (s)
			 (location<-seed s maps))
		       seeds))))

(defun part-two-solution (&optional (file "input.txt"))
  (declare (optimize (speed 3)))
  (let* ((f     (file-reader file))
	 (seeds (seeds f))
	 (maps  (get-maps (cdr f))))
    (apply #'min 
	   (loop for (start len) on seeds by #'cddr
		 collect 
		 (loop with min = (location<-seed start maps)
		       for n from (1+ start) to (+ start len)
		       do (setf min (min min 
					 (location<-seed n maps))))))))

(defun quick-min (start len map)
  "clever algorithm to find the minimum ")

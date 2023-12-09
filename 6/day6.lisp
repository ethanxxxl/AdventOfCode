;; Time:        51     92     68     90
;; Distance:   222   2031   1126   1225

(defparameter *times* (list 51 92 68 90))
(defparameter *distances* (list 222 2031 1126 1225))

(defun boat-distance (button-time total-time)
  (* (- total-time button-time)
     button-time))

(defun num-win-conditions (time distance)
  (loop for ts to time
	when (< distance (boat-distance ts time))
	  count 1))

(defun part-one-solution ()
  (reduce #'* (mapcar #'num-win-conditions *times* *distances*)))


(defparameter *time* 51926890.0)
(defparameter *distance* 222203111261225.0)

(defun min-hold-time (time distance)
  (loop with l = 0
	with r = (1- time)
	))

(defun part-two-solution ()
  "it doesn't seem that the floating point math is incredibly accurate
for this task, so these are just starting values. I used the
boat-distance function in the REPL to find the actual zeros and then
calculated the amount of win conditions."
  (let ((a -1)
	(b *time*)
	(c (- *distance*)))
    (values
     (/ (- (- b) (sqrt (- (* b b) (* 4 a c))))
	(* 2 a))
     (/ (+ (- b) (sqrt (- (* b b) (* 4 a c))))
	(* 2 a)))))

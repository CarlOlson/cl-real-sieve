;;; cl-real-sieve.lisp --- primality tests

;;; Commentary:
;; Here I implement the original sieve as described in The Genuine
;; Sieve of Eratosthenes.  The wheel optimization was added, but it
;; had little to no effect on speed when using bit-vectors.  I imagine
;; the optimization is more significant with different data-strutures
;; and laziness, I hope to explore this further.
;;
;; https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf

;;; Helpers:

(defun range (a b)
  (loop for n from a to b collecting n))

(defun multiples (num max)
  (loop for multiple from 1 to max
     when (zerop (rem multiple num))
     collect multiple))

(defun simple-sieve (primes max)
  (let ((sieve (range 1 max)))
    (dolist (prime primes)
      (setq sieve
	    (set-difference sieve (multiples prime max))))
    (sort sieve '<)))

(defun create-wheel (primes)
  "CREATE-WHEEL assumes you start at one.  Adding the first wheel
number to one gets the first prime after PRIMES."
  (let* ((max-prime (apply 'max primes))
	 (product (apply '* primes))
	 (max (+ max-prime product))
	 (sieve (simple-sieve primes max)))
    (loop
       for (a b) on sieve
       when b collect (- b a) into wheel
       finally (return (apply 'vector wheel)))))

(defun make-bit-vector (size)
  (make-sequence '(vector bit) size :initial-element 1))

(defun delete-multiples (bit-vector n &optional (from (* n n)))
  (do ((index from (+ index n))
       (size (length bit-vector)))
      ((>= index size) bit-vector)
    (setf (aref bit-vector index) 0)))

;;; Primality Tests:

(defun trial-division-prime-p (n)
  "Checks if N is prime by simple trial division.  Returns t if N is
prime, otherwise nil."
  (loop
     for div from 2 to (truncate (sqrt n))
     when (zerop (mod n div)) do (return)
     finally (return t)))

(defun original-sieve (size)
  "Returns a bit vector with indexes equal to numbers.  If index is 1,
then the number is prime (one and zero are set as if prime)."
  (let ((sieve (make-bit-vector (1+ size)))
	(max (truncate (sqrt size))))
    (declare (type (vector bit) sieve))
    (delete-multiples sieve 2)
    (do ((finger 3 (+ finger 2)))
	((> finger max) sieve)
      (unless (zerop (elt sieve finger))
	(delete-multiples sieve finger)))))

(defun wheel-sieve (size &optional (primes '(2 3 5 7)))
  "Similar to ORIGINAL-SIEVE, but uses a wheel optimization."
  (let ((sieve (make-bit-vector (1+ size)))
	(max (truncate (sqrt size))))
    (dolist (prime primes)
      (delete-multiples sieve prime))
    (do* ((wheel      (create-wheel primes))
	  (wheel-size (length wheel))
	  (wheel-idx  0 (mod (1+ wheel-idx) wheel-size))
	  (finger     (+ 1 (aref wheel 0))
		      (+ finger (aref wheel wheel-idx))))
	((> finger max) sieve)
      (unless (zerop (aref sieve finger))
	(delete-multiples sieve finger)))))

;;; Tests & Benchmarks:

(defun test ()
  (let ((size 10000))
    (assert
     (equal (wheel-sieve size)
	    (original-sieve size)))

    (loop
       for n from 0 to size
       for p across (wheel-sieve size)
       do (assert
	   (if (trial-division-prime-p n)
	       (= p 1) (= p 0))))))

(defun benchmark ()
  (princ "-- Trial Division to 1,000,000 --")
  (sb-sys:without-gcing
    (time (loop for i from 2 to 1000000
	     do (trial-division-prime-p i)))
    (princ "-- Original Sieve to 10,000,000 --")
    (time (original-sieve 10000000))
    (princ "-- Wheel Sieve to 10,000,000 --")
    (time (wheel-sieve 10000000)))
  nil)

;;; cl-real-sieve.lisp ends here

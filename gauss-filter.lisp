(in-package :com.xx.canny)

(defun test-canny (img in-path )
  (ch-image:write-image-file "~/lisp-project/test-canny.jpg" (supporter-array-to-img (canny img))))

(defun canny (img)
;;this function get edge of a img
;;fist, do gauss filter to the image
;;second, get the derivation of image
;;third, using nms to get max value of derivation
;;fouth,  
  (let* ((filted-img (gauss-filter-img img))
	 (der-col (derivate-img filted-img 2 2 #'der-method-col-canny-paper))
	 (der-row (derivate-img filted-img 2 2 #'der-method-row-canny-paper))
	 (der-strength (derivate-strength-img  der-col der-row))
	 (nms-img (nms der-col der-row der-strength)))
;;	 (T1 (get-t1-t2 nms-img)))
	 (multiple-value-bind (T1 T2) (get-t1-t2 nms-img)
	   (format t "~a" (equal T1 T2))
	   (format t "~a" (max-array-value nms-img))
	   (ch-image:write-image-file "~/lisp-project/T1.jpg" (supporter-array-to-img T1))
	   (ch-image:write-image-file "~/lisp-project/T2.jpg" (supporter-array-to-img T2))
	   (merge-dual-threshold T1 T2))))



(defun test-merge-dual-threshold ()
;; 0 0 0 0     0 1 1 1   0 1 1 1
;; 0 1 0 0     0 1 0 0   0 1 0 0
;; 0 1 0 0  +  1 1 0 0 = 1 1 0 0
;; 0 0 0 0     0 0 0 1   0 0 0 0
  (let ((arr2  #2A((0 0 0 0) (0 1 0 0) (0 1 0 0) (0 0 0 0)))
	(arr1 #2A((0 1 1 1) (0 1  0 0) (1 1 0 0) (0 0 0 1))))
    (merge-dual-threshold arr1 arr2)))

(defun test-merge-dual-threshold2 ()
  (multiple-value-bind (arr1 arr2) (get-t1-t2 #2A((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15)))
    (merge-dual-threshold arr1 arr2)))

(defun merge-dual-threshold (T1 T2)
;;T1 is low threshold edge, T2 is high threshold edge.
  (let ((extend-before (copy-array T2))
	(extend-after (copy-array T2)))
    (loop do (setf extend-before (copy-array extend-after))
;;	 (format t "~a" extend-before)
	 (setf extend-after (edge-extend T1 extend-before))
;;	 (format t "~a" extend-after)
;;	 (format t "~a" extend-before)
	 until (equalp extend-before extend-after))
    extend-after))


(defun test-edge-extend ()
;; 0 0 0 0    0 0 0 0   0 0 0 0  
;; 0 1 0 0    0 1 1 1   0 1 1 1
;; 0 1 0 0  + 1 1 0 0 = 1 1 0 0
;; 0 0 0 0    0 0 0 1   0 0 0 0
  (let ((arr2  #2A((0 0 0 0) (0 1 0 0) (0 1 0 0) (0 0 0 0)))
	(arr1 #2A((0 0 0 0) (0 1 1 1) (1 1 0 0) (0 0 0 1))))
    (edge-extend arr1 arr2)))

(defun test-edge-extend2 ()
  (multiple-value-bind (arr1 arr2) (get-t1-t2 #2A((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15)) )
    (edge-extend arr1 arr2)))

(defun edge-extend (T1 T2)
;;T1 is low threshold edge, T2 is hight threshold edge.
;;if pixel of T1 is in 8-neighbour of pixel of T2, then extend T2 to this neighbourhood 
  (let* ((height (array-dimension T1 0))
	 (width (array-dimension T1 1))
	 (res (copy-array T2)))
    (dotimes (row height)
      (dotimes (col width)
	(when (/= 0 (aref res row col))
	  (loop for neighbour in (8-neighbour T1 col row) do 
	       (if (and (/= 0 (aref T1 (car neighbour) (last1 neighbour))) 
			(= 0 (aref T2 (car neighbour) (last1 neighbour)))) 
		   (setf (aref res (car neighbour) (last1 neighbour)) 
			 (aref T1 (car neighbour) (last1 neighbour))))))))
    res))

(defun copy-array (arr)
  (let*	((res (make-array (array-dimensions arr))))
    (dotimes (i (array-total-size arr))
      (setf (row-major-aref res i)
	    (row-major-aref arr i)))
    res))

(defun array-same (arr1 arr2)
  (let ((height (array-dimension arr1 0))
	(width (array-dimension arr2 0)))
    (dotimes (row height)
      (dotimes (col width)
	(if (/= (aref arr1 row col) (aref arr2 row col)) (return-from array-same nil))))
    T))

(defun last1 (lst)
  (car (last lst)))

(defun 8-neighbour (arr col row)
  (let ((lst '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))
	(height (array-dimension arr 0))
	(width (array-dimension arr 1)))
    (if (= col 0) (setf lst (remove-if-not (lambda (x) (/= (last1 x) -1)) lst)))
    (if (= col (- width 1)) (setf lst (remove-if-not (lambda (x) (/= (last1 x) 1)) lst)))
    (if (= row 0) (setf lst (remove-if-not (lambda (x) (/= (car x) -1)) lst)))
    (if (= row (- height 1)) (setf lst (remove-if-not (lambda (x) (/= (car x) 1)) lst)))
    (loop for neighbour in lst collect `(,(+ row (car neighbour)) ,(+ col (last1 neighbour))))))

(defun test-threshold (img theta)
  ())
(defun threshold (nms-img theta)
  (let* ((height (array-dimension nms-img 0))
	 (width (array-dimension nms-img 1))
	 (res (make-array `(,height ,width))))
    (dotimes (row height)
      (dotimes (col width)
	(setf (aref res row col) (if (> (aref nms-img row col) theta) 1 0))))
    res))


(defun test-get-T1-T2 ()
  ;;
  (get-T1-T2 #2A((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15))))

(defun get-T1-T2 (nms)
;;this function get T1 and T2, T1 is low threshold edge nms img, T2 is high threshold edge nms img
  (multiple-value-bind (low-threshold high-threshold) (get-dual-threshold nms)
    (format t "hight threshold is ~a, low threshold is ~a" high-threshold low-threshold)
    (values (threshold nms low-threshold) (threshold nms high-threshold) )))

(defun test-get-dual-threshold ()
;; 0 1 2 3
;; 4 5 6 7
;; 8 9 10 11
;; 12 13 14 15
;;
  (get-dual-threshold #2A((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15))))

(defun get-dual-threshold (nms-img &optional (high-threshold-ratio 0.8) ( low-high-ratio 0.5) (bin-num 30))
  ;;this function get two threshold,
  ;;ratio = high threshold/low threshold
  (let* ((hist (histgram nms-img bin-num))
	 (b (format t "~a" hist))
	 (max-val (max-array-value nms-img))
	 (dif (/ (+ max-val 1) bin-num)) ;;10 bins by default, it is a flaw here. Goner fix it later.
	 (a (format t "~a" dif))
	 (T2 (* dif (- (find-ratio-hist high-threshold-ratio hist) 1)))
	 (T1 (* T2 low-high-ratio)))
    (values T1 T2 )))

(defun find-ratio-hist (ratio hist)
;;from front to end, find out summary of bins is ratio of all the bin
;;attention, res is the nature idx, which means when this function is called, you should sub 1 from res before you use it.
  (let ((total (loop for i across hist sum i))
	(res 0)
	(front-sum 0))
    (loop 
       for i across hist
       until (> front-sum (* ratio total) )
       do (progn 
	    (setf front-sum (+ front-sum i))
	  ;;  (format t "~a" front-sum)
	    (setf res (+ res 1))))
	    ;;(format t "~a" res)))
    res))

(defun histgram (arr &optional (bin-number 10 ))
;;this function get histgram of arr, default bin number is 10
;;this is a special hist, since there is a lot of zero in a nms image, we must exclue zero .
  (let* ((max (max-array-value arr))
	 (dif (/ (+ max 1) bin-number))
	 (hist (make-array bin-number :initial-element 0)))
    (dotimes (i (array-total-size arr))
      (if (/= 0 (row-major-aref arr i))
	  (setf (aref hist (floor (/ (row-major-aref arr i) dif))) 
		(+ 1 (aref hist (floor (/ (row-major-aref arr i) dif)))))))
    hist))

(defun test-nms2 (img)
  (let* ((filted-img (gauss-filter-img img))
	 (der-col (derivate-img filted-img 2 2 #'der-method-col-canny-paper))
	 (der-row (derivate-img filted-img 2 2 #'der-method-row-canny-paper))
	 (der-strength (derivate-strength-img  der-col der-row)))
    (ch-image:write-image-file "~/lisp-project/nms.jpg" (supporter-array-to-img (nms der-col der-row der-strength)))))

(defun test-nms (img)
;;this function test function nms
  (let* ((der-col (derivate-img img 2 2 #'der-method-col-canny-paper))
	 (der-row (derivate-img img 2 2 #'der-method-row-canny-paper))
	 (der-strength (derivate-strength-img  der-col der-row)))
    (nms der-col der-row der-strength)))

(defun nms (der-col der-row strength-der)
;;this function get nms edge
  (let* ((height (array-dimension der-col 0))
	 (width (array-dimension der-col 1))
	 (nms-img (make-array `(,(- height 2) ,(- width 2)))))
    (dotimes (row (array-dimension nms-img 0))
      (dotimes (col (array-dimension nms-img 1))
	(setf (aref nms-img row col) 
	      (nms-9-neighbour (get-9-neighbour strength-der (+ 1 row) (+ 1 col))
			       (get-direction (aref der-col (+ 1 row) (+ 1 col))
					      (aref der-row (+ 1 row) (+ 1 col)))))))
    nms-img))

(defun get-direction (der-col der-row)
  (cond ((and (= der-col 0) (= der-row 0))
	 1)
	((and (= der-col 0) (/= der-row 0))
	 0)
	(t
	 (let ((ratio (/ der-row der-col))
	       (tan225 (tan (* 1/8 pi)))
	       (tan675 (tan (* 3/8 pi)))
	       (tan1125 (tan (* 5/8 pi)))
	       (tan1575 (tan (* 7/8 pi))))
	   (cond ((and (>= ratio (* -1 tan225)) (< ratio tan225))
		  0)
		 ((and (>= ratio tan225) (< ratio tan675))
		  1)
		 ((and (>= ratio tan1125) (< ratio tan1575))
		  3)
		 (t
		  2))))))

(defun nms-9-neighbour (9-neighbour direction)
  (let ((central (aref 9-neighbour 1 1))
	(other1)
	(other2))
    (cond ((= direction 0) 
	   (setf other1 (aref 9-neighbour 1 0)) 
	   (setf other2 (aref 9-neighbour 1 2)))
	  ((= direction 1)
	   (setf other1 (aref 9-neighbour 0 2))
	   (setf other2 (aref 9-neighbour 2 0)))
	  ((=  direction 2)
	   (setf other1 (aref 9-neighbour 0 1))
	   (setf other2 (aref 9-neighbour 2 1)))
	  (t
	   (setf other1 (aref 9-neighbour 0 0))
	   (setf other2 (aref 9-neighbour 2 2))))
;;    (if (> central other1 other2) ;;this is wrong, this expression means central > other1 >other2
    (if (and (> central other1) (> central other2))
    ;;I see a lot paper refer to the method above, but in wangzhi' paper, he prefer to the method below. I checked both in experiments, I think wangzhi's method is better, another method is good to find out edge, but wang's method can find more edge. still, I don't know which is better. The effect will be examed in further experiment.
    ;;(if (> central (/ (+ other1 other2) 2))
	central
	0)))

(defun get-9-neighbour (strength-der row col)
  (let* ((9-neighbour (make-array '(3 3))))
    (dotimes (row1 3)
      (dotimes (col1 3)
	(setf (aref 9-neighbour row1 col1) 
	      (aref strength-der (+ row row1 -1) (+ col col1 -1)))))
    9-neighbour))

(defun test-derivate-strength-img (img)
  (derivate-strength-img (derivate-img img 2 2 #'der-method-col-canny-paper)
			 (derivate-img img 2 2 #'der-method-row-canny-paper)))

(defun derivate-strength-img (der-col der-row)
  (let* ((height (array-dimension der-col 0))
	(width (array-dimension der-col 1))
	(strength-der (make-array `(,height ,width))))
    (dotimes (row height)
      (dotimes (col width)
	(setf (aref strength-der row col)
	      ((lambda (a b) (sqrt (+ (* a a) (* b b)))) 
	       (aref der-col row col)
	       (aref der-row row col)))))
    strength-der))

(defun derivate-img-col (img der-method-row-size der-method-col-size)
;;this function is obsoleted and replaced by function derivate-img
;;this function cal image derivation
;;Since derivation method vary from one another, this function use der-method to calc derivation
;;because different der-method have different size, the result is different, so der-method-size must provided. for example,if image size is width*height, der-method-size is 2*2, then derivation size is (width-1)*(height-1)
 (let* ((width (ch-image:image-width img))
       (height (ch-image:image-height img))
       (der-col (make-array `(,(1+ (- height der-method-row-size)) ,(1+ (- width der-method-col-size))))))
   (dotimes (j (array-dimension der-col 0))
     (dotimes (i (array-dimension der-col 1)) 
       (setf (aref der-col j i) (der-method-col-canny-paper img i j))))
   der-col))

(defun derivate-img-row (img der-method-row-size der-method-col-size)
;;this function is obsoleted and replaced by function derivate-img
;;this function cal image derivation
;;Since derivation method vary from one another, this function use der-method to calc derivation
;;because different der-method have different size, the result is different, so der-method-size must provided. for example,if image size is width*height, der-method-size is 2*2, then derivation size is (width-1)*(height-1)
 (let* ((width (ch-image:image-width img))
       (height (ch-image:image-height img))
       (der-row (make-array `(,(1+ (- height der-method-row-size)),(1+ (- width der-method-col-size))))))
   (dotimes (j (array-dimension der-row 0))
     (dotimes (i (array-dimension der-row 1))
       (setf (aref der-row j i) (der-method-row-canny-paper img i j))))
   der-row))

(defun derivate-img-test (img)
  (derivate-img img  2 2 #'der-method-row-canny-paper)
  (derivate-img img  2 2 #'der-method-col-canny-paper))

(defun derivate-img (img der-method-row-size der-method-col-size der-method)
 (let* ((width (ch-image:image-width img))
       (height (ch-image:image-height img))
       (der (make-array `(,(1+ (- height der-method-row-size)) ,(1+ (- width der-method-col-size))))))
   (dotimes (row (array-dimension der 0))
     (dotimes (col (array-dimension der 1)) 
       (setf (aref der row col) (funcall der-method img col row))))
   der))


(defun der-method-row-canny-paper (img col row)
;;this derivation method is introduced in canny's paper, size is 2*2, 
;; A B
;; C D
;; col-derivate = 0.5* (A + B - C -D)
  (* 0.5 (- (+ (ch-image:get-pixel img row col) (ch-image:get-pixel img row (+ 1 col)))
	    (ch-image:get-pixel img (+ row 1) (+ col 1)) (ch-image:get-pixel img (+ row 1) col))))

(defun der-method-col-canny-paper (img col row)
;;this deviration method cal derivation in col direction
;; A B
;; C D
;; row-derivate = 0.5*(B + D - A - C)
  (* 0.5 (- (+ (ch-image:get-pixel img (+ row 1) (+ col 1)) (ch-image:get-pixel img row (+ col 1))) 
	    (ch-image:get-pixel img row col) (ch-image:get-pixel img (+ row 1) col))))


(defun gauss-filter-img (img &key (delta 1.45 delta-supplied-p) (size 5 size-supplied-p))
;;gauss filter make image smooth
;;this function can only be applied to ch-image gray pic, there is no need to smooth a RGB pic according to my requirement of detecting cubic pattern 
;;this function apply fast algorithm, because gauss filter can be seperated into 2 dimention, calculation complexity can deduce from o(size*size*col*row) to o(2*size*col*row)
;;gauss-filter only apply to inner part of image, because when col=0 and row = 0, filter not work well.
  (if (= 1 (length (ch-image:get-channels img)))
    (gauss-filter-img-row (gauss-filter-img-col img :delta delta :size size))
    (format t "Please use gray image!")))


(defun gauss-filter-img-row (img &key (delta 1.45 delta-supplied-p) (size 5 size-supplied-p))
;;gauss filter filte image row
  (let* ((dst-img (ch-image:copy-image img))
	 (filter1 (gauss-filter-1dimention :delta delta :size size))
	 (half-size (floor size 2))
	 (width (ch-image:image-width img))
	 (height (ch-image:image-height img))
	 (max-i (- width half-size)))
    (do ((i half-size (+ 1 i)))
	((= i max-i) dst-img)
      (do ((j 0 (+ 1 j)))
	  ((= j height))
	(let ((filted-img (img-get-n-pixel img  "row" size i j)))
	  (ch-image:set-pixel dst-img j i (gauss-convolute filter1 filted-img)))))))


(defun gauss-filter-img-col (img &key (delta 1.5 delta-supplied-p) (size 5 size-supplied-p))
;;gauss filter filte image col
  (let* ((dst-img (ch-image:copy-image img))
	(filter1 (gauss-filter-1dimention :delta delta :size size))
	(half-size (floor size 2))
	(width (ch-image:image-width img))
	(height (ch-image:image-height img))
	(max-i (- width half-size))
	(max-j (- height half-size)))
    (do ((j half-size (+ 1 j)))
	((= j max-j) dst-img)
      (do ((i 0 (+ 1 i)))
	  ((= i width))
	(let ((filted-img (img-get-n-pixel img  "col" size i j)))
	  (ch-image:set-pixel dst-img j i (gauss-convolute filter1 filted-img)))))))



(defun gauss-convolute (arr1 arr2)
;;this function bound the result to 0~255, otherwise, it can not put in a 8-bit ch-image,
;; maybe it is wrong.
  (let ((arr1-sum (reduce #'+ arr1))
	(result (reduce #'+ (map 'vector #'* arr1 arr2))))
    (max 0 (min 255 (floor (/ result arr1-sum))))))

(defun gauss-filter-1dimention (&key (delta 1.45 delta-supplied-p) (size 5 size-supplied-p))
;;this function generate 1 dimention gauss filter
  (let ((filter (make-array size :initial-element nil))
	(half-size (floor size 2))
	(delta-square (* delta delta)))
    (dotimes (i size)
      (let ((dif-square (* (- i half-size) (- i half-size))))
	(setf (aref filter i) (exp (* -1 (/ dif-square delta-square 2))))))
    filter))

(defun gauss-filter-2dimention (&key (delta 1.45 delta-supplied-p) (size 5 size-supplied-p))
;;this function generate 2 dimention gauss filter
;;this function rely on gauss-filter-1dimention
  (let ((filter2 (make-array `(,size ,size) :initial-element nil))
	(filter1 (gauss-filter-1dimention :delta delta :size size)))
    (dotimes (i size)
      (dotimes (j size)
	(setf (aref filter2 i j) (* (aref filter1 i) (aref filter1 j)))))
    (let ((filter00 (aref filter2 0 0)))
      (dotimes (i size)
	(dotimes (j size)
	  (setf (aref filter2 i j) (/ (aref filter2 i j) filter00)))))
    filter2))

(defun img-get-n-pixel (img direction n i j)
;;to get n pixels in one direction, return a array with n pixel
;;this function apply to gray img
;;direction is 2 bi-value parameter,if is "row", get n pixel in a row, if is "col", get n pixels in a col
  (let ((arr (make-array n :fill-pointer 0)))
    (if (string= direction "row") 
	(dotimes (k n)
	  (vector-push (ch-image:get-pixel img j (+ i (- k (floor n 2)))) arr)))
    (if (string= direction "col")
	(dotimes (k n)
	  (vector-push (ch-image:get-pixel img (+ j (- k (floor n 2))) i) arr)))
    arr))

(defun generate-gauss-filter (&key (delta 1.45 delta-supplied-p) (size 5 size-supplied-p))
;;this function is used to generate a 2 dimention gauss filter
  (if (and (oddp size) (/= delta 0))
      (let ((filter (make-array `(,size ,size) :initial-element nil))
	    (square-delta (* delta delta))
	    (filter-i (make-array size :initial-element nil))
	    (half-size (floor size 2)))	
	(dotimes (i size)
	  (let ((dif-square (* (- i half-size) (- i half-size))))
	    (setf (aref filter-i i) 
		  (exp (* -1 (/ dif-square square-delta 2))))))
	(dotimes (i size)
	  (dotimes (j size)
	    (setf (aref filter i j) (* (aref filter-i i) (aref filter-i j)))))
	(let ((filter0 (aref filter 0 0)))
	  (dotimes (i size)
	     (dotimes (j size)
	       (setf (aref filter i j) (/ (aref filter i j) filter0)))))
	filter)))




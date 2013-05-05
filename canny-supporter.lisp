(defun test-supporter-array-to-img (arr &optional (path "~/lisp-project/test-support.jgp" path-supplied-p))
  (ch-image:write-image-file (if path-supplied-p
				 (concatenate 'string "~/lisp-project/" path)
				 path)
			     (supporter-array-to-img arr)))

(defun supporter-array-to-img (arr )
;; this function transfer an array to ch-image.
  (multiple-value-bind (max min) (max-array-value arr)
    (let* ((width (array-dimension arr 1))
	  (height (array-dimension arr 0))
	  (img (make-instance 'ch-image:ub8-matrix-image :width width :height height)))
      (let ((k (/ 255 (- max min))))
	(dotimes (row height)
	  (dotimes (col width)
	    (ch-image:set-pixel img row col (round (* k (- (aref arr row col) min)))))))
      img)))

(defun supporter-img-to-array (img)
;; this function transfer a ch-image to an array
  (let* ((width (ch-image:image-width img))
	(height (ch-image:image-height img))
	(arr (make-array `(,height ,width))))
    (dotimes (row height)
      (dotimes (col width)
	(setf (aref arr row col) (ch-image:get-pixel img row col))))
    arr))

(defun extract-arr (arr row col row-len col-len)
  (let ((height (array-dimension arr 0))
	(width (array-dimension arr 1))
	(res-arr (make-array `(,row-len ,col-len))))
    (when (and (> width (+ col col-len)) (> height (+ row row-len)))
      (dotimes (row1 row-len)
	(dotimes (col1 col-len)
	  (setf (aref res-arr row1 col1) (aref arr (+ row row1) (+ col col1))))))
    res-arr))

(defun max-array-value (nms)
  (let ((max-value -100000)
	(min-value 1000000))
    (dotimes (row (array-dimension nms 0))
      (dotimes (col (array-dimension nms 1))
	(setf max-value (max max-value (aref nms row col)))
	(setf min-value (min min-value (aref nms row col) ))))
    (values max-value min-value)))

(defun test-extract-arr (img)
  (test-supporter-array-to-img (extract-arr (supporter-img-to-array img) 10 10 100 100) "a.jpg"))

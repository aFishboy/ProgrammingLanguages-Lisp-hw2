(defun minFunc (x y)
    (if (< x y) x 
        y))

(defun maxFunc (x y)
    (if (> x y) x
        y))

(defun lengthFunc (l)
    (if (null l) 0
        (+ 1 (lengthFunc (cdr l)))))

(defun min-mean-max (lst)
    (let* ((n (lengthFunc lst))
        (minVal (reduce #'minFunc lst))
        (maxVal (reduce #'maxFunc lst))
        (meanVal (/ (reduce #'+ lst) n)))
    (list minVal meanVal maxVal)))

(defun makeLists (listL listR lst num)
  (cond ((null lst) (list listL listR))
        ((< (car lst) num) (makeLists (append listL (list (car lst))) listR (cdr lst) num))
        ((> (car lst) num) (makeLists listL (append listR (list (car lst))) (cdr lst) num))
        (t (makeLists listL (append listR (list (car lst))) (cdr lst) num))
  )
)

(defun pivot (num lst)
  (let ((listL nil) (listR nil))
    (makeLists listL listR lst num)
  )
)

(defun lengthFunc (l)
    (if (null l) 0
        (+ 1 (lengthFunc (cdr l)))))

(defun quicksort (lst)
  (if (or (<= (lengthFunc lst) 1) (null lst))
      lst 
    (let* ((pivotVal (car lst)) 
          (pivotLst (pivot pivotVal lst))
          (listL (car pivotLst))
          (listR (car (cdr pivotLst)))
          )
      (append (quicksort listL) (list pivotVal) (quicksort (cdr listR)))
    )
  )
) 
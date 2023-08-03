(defun matrix-add (mat1 mat2)
  (mapcar #'(lambda (row1 row2)
      (mapcar #'+ row1 row2)) 
      mat1 mat2
  )
)

(defun matrix-transpose (mat)
  (cond ((null mat) 
          nil
        ) 
        ((null (car mat)) 
          nil
        ) 
        (t 
          (cons (mapcar #'car mat) ; take the first element of each row
          (matrix-transpose (mapcar #'cdr mat)))
        )
  )
)

(defun dotRowCol (row col)
  (cond ((null row) 
          0
        )
        ((null col) 
          0
        )
        (t 
          (+ (* (car row) (car col))
          (dotRowCol (cdr row) (cdr col)))
        )
  )
)

(defun matrix-multiply (mat1 mat2)
  (cond ((null mat1) 
          nil
        )
        ((null mat2) 
          nil
        )
        (t (let* ((cols1 (matrix-transpose mat1))
                  (cols2 (matrix-transpose mat2)))
             (mapcar #'(lambda (row1) (mapcar #'(lambda (col2) 
             (dotRowCol row1 col2))cols2))mat1))
        )
  )
)

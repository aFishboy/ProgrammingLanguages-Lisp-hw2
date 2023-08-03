(defun checkPattern (lst1 lst2)
  (cond ((null lst2)
          (checkAssert (cdr lst1) lst2)
        )
        ((equal (car (cdr lst1)) '?)
          (checkAssert (cdr (cdr lst1)) (cdr lst2))
        )
        ((equal (car (cdr lst1)) (car lst2))
          (checkAssert (cdr (cdr lst1)) (cdr lst2))
        )
        (t
          (checkPattern lst1 (cdr lst2))
        )
  )
)

(defun checkAssert (lst1 lst2)
  (cond ((and (null lst1) (null lst2))
          t
        )
        ((and (null lst1) (not (null lst2)))
          nil
        )
        ((and (null lst2) (not (null lst1)))
          nil
        )
        ((equal (car lst1) '!)
          (checkPattern lst1 (cdr lst2))
        )
        ((equal (car lst1) (car lst2))
          (checkAssert (cdr lst1) (cdr lst2))
        )
        ((equal (car lst1) '?)
          (checkAssert (cdr lst1) (cdr lst2))
        )
        (t
          nil
        )
  )
)

(defun match (lst1 lst2)
  (checkAssert lst1 lst2)
)

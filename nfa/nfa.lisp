(defun lstOfReach (lst)
    (cond   ((null lst) 
                nil
            )
            ((null (car lst)) 
                (lstOfReach (cdr lst)) 
            )
            (t 
                t
            )
    )
)

(defun reachable (transition start final input)
    (cond   ((and (null input) (eql start final)) 
                t
            )
            ((null input) 
                nil
            )
            (t
                (let ((options (funcall transition start (car input))))
                        (lstOfReach (mapcar (lambda (curr) 
                        (reachable transition curr final (cdr input))) options))
                )   
            )
      )

)
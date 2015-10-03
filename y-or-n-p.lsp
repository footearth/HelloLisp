; y-or-n-p.lisp
(context 'y-or-n-p)

(define (y-or-n-p:y-or-n-p)
  (set
    'input nil
    'info (args 0)
  )
  (define (get-input)
    (print info)
    (trim
      (read-line)
    )
  )
  (define (check)
    (true?
      (case
        (get-string input)
        ("y" true)
        ("Y" true)
        ("n" true)
        ("N" true)
        (true nil)
      )
    )
  )
  (define (check-again)
    (true?
      (case
        (get-string input)
        ("y" true)
        ("Y" true)
        (true nil)
      )
    )
  )
  (do-while
    (not (check)
    )
    (set 'input (get-input)
    )
  )
  (check-again)
)

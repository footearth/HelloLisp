#!/usr/bin/env newlisp
(load "./y-or-n-p.lsp")

(set 'db nil)

(define (add-record cd)
  (push cd db)
)

(define (make-cd
    title
    artist
    rating
    ripped
  )
  (list
    'title title
    'artist artist
    'rating rating
    'ripped ripped
  )
)

(define (dump-db)
  (print "(\n")
  (dolist (cd db)
    (print "  ")
    (print cd)
    (print "\n")
  )
  (print ")")
)

(define (print-db)
  (dolist (cd db)
    (dolist (field cd)
      (if
        (= 0 (% $idx 2)
        )
        (print
          (format
            "%-8s"
            (append
              (upper-case (string field)
              )
              ":"
            )
          )
        )
        (println
          (format "%-15s" (string field)
          )
        )
      )
    )
    (println)
  )
)

(define (prompt-read prompt)
  (print
    (format "%-8s"
      (append
        prompt
        ":"
      )
    )
  )
  (read-line)
)

(define (prompt-for-cd)
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or
      (int
        (trim (prompt-read "Rating")
        )
      ) 0
    )
    (true?
      (y-or-n-p "Ripped [y/n]")
    )
  )
)

(define (add-cds)
  (while
    (true?
      (y-or-n-p "Another? [y/n]: ")
    )
    (add-record
      (prompt-for-cd)
    )
  )
)

(add-record
  (make-cd
    "Roses" "Kathy Mattea" 7 true
  )
)
(add-record
  (make-cd
    "Fly" "Dixie Chicks" 8 true
  )
)
(add-record
  (make-cd
    "Home" "Dixie Chicks" 9 true
  )
)

(add-cds)
(println)

;(dump-db)
(print-db)

(exit)

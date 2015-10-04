;;;; -*- Lisp -*-

(defpackage :hello-system
  (:use #:asdf)
)
(in-package :hello-system)

(defsystem hello
  :name Hello World
  :version
  :author Chun Tian (binghe)
  :depends-on ()
  :components (
    (:file package)
    (:file config :depends-on (package)
    )
    (:file hello :depends-on (config)
    )
  )
)

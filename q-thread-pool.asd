;;;; System definition for Q-THREAD-POOL.

(defpackage q-thread-pool-asd
  (:documentation
   "System definition for Q-THREAD-POOL.")
  (:use :cl :asdf))

(in-package :q-thread-pool-asd)

(defsystem q-thread-pool
  :description
  "Thread pool based on JPL-QUEUES."
  :version "1.0.2"
  :license "GNU AGPL"
  :author "Max Rottenkolber <max@mr.gy>"
  :components ((:file "thread-pool"))
  :depends-on ("bordeaux-threads"
		"jpl-queues"))

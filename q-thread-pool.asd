;;;; System definition for Q-THREAD-POOL.

(defpackage q-thread-pool-asd
  (:documentation
   "System definition for Q-THREAD-POOL.")
  (:use :cl :asdf))

(in-package :q-thread-pool-asd)

(defsystem q-thread-pool
  :description
  "Thread pool based on JPL-QUEUES"
  :components ((:file "thread-pool"))
  :depends-on ("bordeaux-threads"
		"jpl-queues"))

;;;; Thread pool based on JPL-QUEUES

(defpackage :q-thread-pool
  (:documentation
   "Thread pool based on JPL-QUEUES")
  (:use :cl
	:bordeaux-threads
 	:jpl-queues)
  (:export :make-thread-pool
	   :enqueue-task
	   :destroy-thread-pool))

(in-package :q-thread-pool)

(defun make-task-queue (capacity)
  "Make new task queue with CAPACITY."
  (make-instance 'synchronized-queue
		 :queue (make-instance 'bounded-fifo-queue
				       :capacity capacity)))

(defun make-threads (n task-queue)
  "Create N threads accepting tasks from TASK-QUEUE."
  (loop for i from 1 to n collect
       (make-thread (lambda () (loop do (funcall (dequeue task-queue))))
		    :name "pooling-thread")))

(defun make-thread-pool (n-threads task-capacity)
  "Return thread pool with N-THREADS and TASK-CAPACITY."
  (let* ((task-queue (make-task-queue task-capacity))
	 (threads (make-threads n-threads task-queue)))
    (cons task-queue threads)))

(defmacro enqueue-task (thread-pool &body body)
  "Enqeue a function with BODY in THREAD-POOL."
  `(enqueue (lambda () ,@body) (car ,thread-pool)))

(defun destroy-thread-pool (thread-pool)
  "Destroy threads in THREAD-POOL."
  (loop for thread in (cdr thread-pool)
     do (destroy-thread thread)))
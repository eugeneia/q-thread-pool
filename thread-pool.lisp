;;;; Thread pool based on JPL-QUEUES.

(defpackage q-thread-pool
  (:documentation
   "Simple thread pools based on concurrent queues.

    < Example Usage

     Use {make-thread-pool} to create a thread pool with 16 threads and a
     backlog of 32 and bind it to {pool}.

     #code#
     (setq pool (make-thread-pool 16 32))
     #

     Use {enqueue-task} to enqueue 64 tasks for parallel evaluation by
     {pool}. Note that the body forms of {enqueue-task} is a lexical
     closure. The threads of {pool} will work off the backlog
     concurrently. Once the backlog is full, e.g. when there are already
     32 other tasks waiting, {enqueue-task} will block until there is
     room in the queue.

     #code#
     (let ((out *standard-output*))
       (dotimes (i 64)
         (enqueue-task pool
           (sleep 5)
           (write-string \"x\" out))))
     #

     When {pool} is no longer needed, its threads should be destroy using
     {destroy-thread-pool} in order to avoid leaking resources.

     #code#
     (destroy-thread-pool pool)
     #

    >")
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
       (make-thread
        (lambda () (loop do (funcall (dequeue task-queue))))
        :name (format nil "q-thread-pool ~a ~a/~a " task-queue i n))))

(defun make-thread-pool (size backlog)
  "*Arguments and Values:*

   _size_, _backlog_—positive _integers_.

   *Description*:

   {make-thread-pool} returns a thread pool. _Size_ is the number of
   threads in the pool. _Backlog_ is the number of tasks that will be
   queued before {enqueue-task} blocks."
  (check-type size (integer 1))
  (check-type backlog (integer 1))
  (let* ((task-queue (make-task-queue backlog))
	 (threads (make-threads size task-queue)))
    (cons task-queue threads)))

(defmacro enqueue-task (thread-pool &body forms)
  "*Arguments and Values:*

   _thread-pool_—a thread pool.

   _forms_—_forms_.

   *Description*:

   {enqueue-task} enqueues a _lexical closure_ containing _forms_ for
   evaluation by _thread-pool_. If the backlog of of _thread-pool_ is
   full {enqueue-task} will block until another task is dequeued."
  `(enqueue (lambda () ,@forms) (car ,thread-pool)))

(defun destroy-thread-pool (thread-pool)
  "*Arguments and Values:*

   _thread-pool_—a thread pool.

   *Description*:

   {destroy-thread-pool} destroys all threads allocated by
   _thread-pool_."
  (loop for thread in (cdr thread-pool)
     do (destroy-thread thread)))

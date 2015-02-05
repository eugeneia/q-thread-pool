# q-thread-pool

**Description:**

A very simple thread pool implementation based on
[jpl-queues](http://www.thoughtcrime.us/software/jpl-queues/).

**Example Usage:**

```
;; Create a thread pool with 16 threads and a task queue capacity of
;; 32. Bind it to *THREAD-POOL*.
(defvar *thread-pool* (make-thread-pool 16 32))

;; Enqueue a task into the pool. Note how the body of ENQUEUE-TASK is a
;; lexical closure. The pool's threads will work off the task queue in
;; parallel. If the task queue is full, e.g. there are already 32 other
;; tasks waiting, ENQUEUE-TASK will block until there is room in the
;; queue.
(let ((out *standard-output*))
  (dotimes (i 64)
    (enqueue-task *thread-pool*
      (sleep 5)
      (write-string "x" out))))

;; Finally, when you are done with *THREAD-POOL*, you shoud call
;; DESTORY-THREAD-POOL in order to destroy the allocated threads.
(destroy-thread-pool *thread-pool*)
```

**Documentation:**

* [API documentation](http://mr.gy/software/q-thread-pool/api.html)

;;; LIST SORT benchmark
;;;
;;;
;;; Variables:
;;;
;;;  % of shuffling around of conses themselves
;;;  % of out of order key values
;;;  NOT USEFUL HERE: % of identical values
;;;  size
;;;
;;; Output
;;;
;;;  Time
;;;  # of comparisons

(defun make-latin-square (rows columns values &key combiner)
  (assert (<= (length rows)    (length values)))
  (assert (<= (length columns) (length values)))
  (let* ((combiner (or combiner #'list))
         (n (max (length rows) (length columns)))
         (cells (make-array (list n n))))
    (assert (= n (length values)))
    ;; standard latin square: just offset each column
    ;; by one
    (dotimes (i n)
      (dotimes (j n)
        (setf (aref cells i j)
              (elt values (mod (+ i j) n)))))
    ;; fisher-yates on columns
    (dotimes (i n)
      (let ((j (+ i (floor (* (random 1d0) (- n i))))))
        (unless (= i j)
          (dotimes (k n)
            (rotatef (aref cells k i) (aref cells k j))))))
    ;; then on rows
    (dotimes (i n)
      (let ((j (+ i (floor (* (random 1d0) (- n i))))))
        (unless (= i j)
          (dotimes (k n)
            (rotatef (aref cells i k) (aref cells j k))))))
    ;; finally, just read the triplets in order,
    ;; and skip extra columns/rows
    (let ((plans '()))
      (loop for i upfrom 0
            for row in rows
            do (loop for j upfrom 0
                     for column in columns
                     do (push (funcall combiner row column (aref cells i j))
                              plans)))
      (nreverse plans))))

(defun cartesian-product (accumulator &rest sets)
  (let ((values '())
        (accumulator (or accumulator #'list)))
    (labels ((rec (sets acc)
               (cond ((null sets)
                      (push (apply accumulator (reverse acc))
                            values))
                     ((atom (first sets))
                      (rec (rest sets) (cons (first sets) acc)))
                     (t
                      (dolist (value (first sets))
                        (rec (rest sets) (cons value acc)))))))
      (rec sets '())
      (nreverse values))))

(defvar *sizes* (loop for e from 5 upto 24 by (/ 19 9)
                      collect (round (expt 2 (float e 1d0)))))

(defvar *scatteredness* '(nil 10 500 #. (1- most-positive-fixnum)))
(defvar *unsortedness* '(nil 1 2 5 10 50 100 500 1000 #.(1- most-positive-fixnum)))
(defvar *quick-merge* '(nil 8 16 32))
(defvar *params* (cartesian-product #'list
                                    :leaf
                                    '(nil t :cmov)
                                    :cached-key
                                    '(nil t)
                                    :merge-branch
                                    '(t nil)))

(defun make-perf-square ()
  (make-latin-square *params*
                     (cartesian-product #'list
                                        :scatter
                                        *scatteredness*
                                        :size
                                        *sizes*)
                     (cartesian-product #'list
                                        :quick-merge *quick-merge*
                                        :shuffle     *unsortedness*)
                     :combiner (lambda (a b c)
                                 (append a c b))))

(defun make-count-square ()
  (make-latin-square (cartesian-product #'list
                                        :leaf '(nil t)
                                        :cached-key nil
                                        :merge-branch t)
                     (cartesian-product #'list
                                        :size
                                        (let ((len (* (length *quick-merge*)
                                                      (length *unsortedness*))))
                                          (loop for e from 5 upto 24
                                                by (/ 19 (1- len))
                                                collect (round
                                                         (expt 2
                                                               (float e 1d0))))))
                     (cartesian-product #'list
                                        :quick-merge *quick-merge*
                                        :shuffle     *unsortedness*)
                     :combiner (lambda (a b c)
                                 (append a c b))))

(defvar *random-states* (loop repeat 5
                              for i from 12345
                              collect
                              (let ((*random-state* (seed-random-state i)))
                                (loop repeat 1000
                                      do (random most-positive-fixnum))
                                *random-state*)))

(defun apply-params (&rest args &key size scatter shuffle &allow-other-keys)
  (prog2
      (gc :full t)
      (sb-sys:without-gcing
        (let ((sort (apply 'find-sort args))
              (inputs (loop for state in *random-states*
                            collect
                            (let ((*random-state* (make-random-state
                                                   state)))
                              (shuffle-conses
                               (shuffle-values
                                (alexandria:iota size)
                                (or shuffle 0))
                               (or scatter 0))))))
          (funcall sort (alexandria:iota 1024) #'< #'identity)
          (mapcar (lambda (list)
                    (funcall sort list #'< #'identity))
                  inputs)))
    (gc :full t)))

(defun run-all-params (task params)
  (format *trace-output*
          "Starting at: ~{~A ~}~%" (multiple-value-list
                                    (decode-universal-time
                                     (get-universal-time))))
  (let ((values
          (let* ((params (alexandria:shuffle params))
                 (count (length params))
                 (step (* count 5d-2))
                 (limit step)
                 (now (get-internal-run-time)))
            (loop for i upfrom 0
                  for param in params
                  do (when (>= i limit)
                       (format *trace-output*
                               "~2,1F% done (~A)~%" (/ (* 100d0 i)
                                                       count)
                               (/ (- (get-internal-run-time) now)
                                  (float internal-time-units-per-second 1d0)))
                       (incf limit step))
                  append (let ((values (apply 'apply-params :task task
                                              param)))
                           (mapcar (lambda (value)
                                     (cons value param))
                                   values))))))
    (dolist (value values)
      (destructuring-bind (value &key size scatter
                                   shuffle quick-merge
                                   leaf cached-key merge-branch
                           &allow-other-keys) value
        (format t "\"~Ax~A\"	\"~Ax~A\"	\"~Ax~Ax~A\"	~A~%"
                size (case scatter
                       ((nil) "F")
                       ((#. (1- most-positive-fixnum)) "T")
                       (t scatter))
                (case shuffle
                  ((nil) "F")
                  ((#. (1- most-positive-fixnum)) "T")
                  (t shuffle))
                (or quick-merge "F")
                (or leaf "F") (or cached-key "F") (or merge-branch "F")
                value)))))

(defvar *sorts* (make-hash-table :test 'equal))
(defun find-sort (&rest args
                  &key task leaf quick-merge merge-branch cached-key
                    &allow-other-keys)
  (or (gethash (list :task task
                     :leaf leaf
                     :quick-merge quick-merge
                     :merge-branch merge-branch
                     :cached-key cached-key)
               *sorts*)
      (error "Unknown sort parameters: ~A" args)))

(declaim (inline cmp-fixnum))
(defun cmp-fixnum (x y)
  (declare (fixnum x y))
  (< x y))

(defun make-all-sorts ()
  (loop for (task leaf quick-merge merge-branch cached-key)
        in (cartesian-product #'list
                              '(:count :inline :normal)
                              '(nil t :cmov)
                              '(nil 8 16 32)
                              '(t nil)
                              '(t nil))
        for args = (list :task task
                         :leaf leaf
                         :quick-merge quick-merge
                         :merge-branch merge-branch
                         :cached-key cached-key)
        for function = (compile nil (apply 'make-sort args))
        do (setf (gethash args *sorts*) function)))

(defun make-sort (&rest args &key task &allow-other-keys)
  `(lambda (list test key)
     (declare (type list list)
              (type function test key)
              (ignorable test key))
     (let* (,@(cond ((eql task :inline)
                     `((test     #'cmp-fixnum)
                       (key      #'identity)))
                    ((eql task :count)
                     `((count 0)
                       (test (lambda (x y)
                               (declare (type fixnum x y))
                               (incf count)
                               (< x y)))
                       (key #'identity))))
            (head (list :head)))
       (declare (dynamic-extent head))
       ,(let ((sort (apply 'make-sort-body args)))
          (if (eql task :count)
              `(progn ,sort count)
              `(nth-value 1
                          (sb-vm::with-cycle-counter ,sort)))))))

(defun make-sort-body (&rest args &key ((:leaf leaf))
                                       ((:quick-merge quick))
                                    &allow-other-keys)
  `(labels ((%merge (head list1 list2 test key &aux (tail head))
              (declare (type cons head list1 list2)
                       (type function test key)
                       (optimize speed))
              ,(apply 'make-merge-body args))
            (merge* (size list1 tail1 list2 tail2 rest)
              (declare (optimize speed)
                       (type (and unsigned-byte fixnum) size)
                       (type cons list1 tail1 list2 tail2)
                       (ignorable size tail1 tail2))
              ,(when quick
                 `(when (>= size ,quick)
                    (cond ((not (funcall test (funcall key (car list2))
                                         (funcall key (car tail1))))
                           (setf (cdr tail1) list2)
                           (return-from merge* (values list1 tail2 rest)))
                          ((funcall test (funcall key (car tail2))
                                    (funcall key (car list1)))
                           (setf (cdr tail2) list1)
                           (return-from merge* (values list2 tail1 rest))))))
              (values (%merge head list1 list2 test key)
                      ,(if quick
                           `(if (null (cdr tail1))
                                tail1 tail2)
                           'tail1)
                      rest))
            (recur (list size)
              (declare (optimize speed)
                       (type cons list)
                       (type (and fixnum unsigned-byte) size))
              ,(ecase leaf
                 ((nil)
                  `(if (= size 1)
                       (values list list (shiftf (cdr list) nil))
                       (let ((half (ash size -1)))
                         (multiple-value-bind (list1 tail1 rest)
                             (recur list half)
                           (multiple-value-bind (list2 tail2 rest)
                               (recur rest (- size half))
                             (merge* size list1 tail1 list2 tail2 rest))))))
                 ((t)
                  `(cond ((> size 3)
                          (let ((half (ash size -1)))
                            (multiple-value-bind (list1 tail1 rest)
                                (recur list half)
                              (multiple-value-bind (list2 tail2 rest)
                                  (recur rest (- size half))
                                (merge* size list1 tail1 list2 tail2 rest)))))
                         ((= size 3)
                          (let* ((second (cdr list))
                                 (third  (cdr second))
                                 (x (car list))
                                 (y (car second))
                                 (z (car third)))
                            (declare (type cons second third))
                            (when (funcall test (funcall key y)
                                           (funcall key x))
                              (rotatef x y))
                            (when (funcall test (funcall key z)
                                           (funcall key y))
                              (rotatef z y))
                            (when (funcall test (funcall key y)
                                           (funcall key x))
                              (rotatef x y))
                            (setf (car list) x
                                  (car second) y
                                  (car third) z)
                            (values list third (shiftf (cdr third) nil))))
                         ((= size 2)
                          (let ((second (cdr list)))
                            (declare (type cons second))
                            (when (funcall test (funcall key (car second))
                                           (funcall key (car list)))
                              (rotatef (car list) (car second)))
                            (values list second (shiftf (cdr second) nil))))
                         (t             ; (= size 1)
                          (values list list (shiftf (cdr list) nil)))))
                 (:cmov
                  `(cond ((> size 3)
                          (let ((half (ash size -1)))
                            (multiple-value-bind (list1 tail1 rest)
                                (recur list half)
                              (multiple-value-bind (list2 tail2 rest)
                                  (recur rest (- size half))
                                (merge* size list1 tail1 list2 tail2 rest)))))
                         ((= size 3)
                          (let* ((second (cdr list))
                                 (third  (cdr second))
                                 (x (car list))
                                 (y (car second))
                                 (z (car third)))
                            (declare (type cons second third))
                            (let ((test (funcall test (funcall key y)
                                                 (funcall key x)))
                                  (a x)
                                  (b y))
                              (setf x (if test b a)
                                    y (if test a b)))
                            (let ((test (funcall test (funcall key z)
                                                 (funcall key y)))
                                  (a y)
                                  (b z))
                              (setf y (if test b a)
                                    z (if test a b)))
                            (let ((test (funcall test (funcall key y)
                                                 (funcall key x)))
                                  (a x)
                                  (b y))
                              (setf x (if test b a)
                                    y (if test a b)))
                            (setf (car list) x
                                  (car second) y
                                  (car third) z)
                            (values list third (shiftf (cdr third) nil))))
                         ((= size 2)
                          (let* ((second (cdr list))
                                 (x (car list))
                                 (y (car second)))
                            (declare (type cons second))
                            (let ((test (funcall test (funcall key y)
                                                 (funcall key x)))
                                  (a x)
                                  (b y))
                              (setf x (if test b a))
                              (setf y (if test a b)))
                            (setf (car list) x
                                  (car second) y)
                            (values list second (shiftf (cdr second) nil))))
                         (t
                          (values list list (shiftf (cdr list) nil))))))))
     (when list
       (values (recur list (length list))))))

(defun make-merge-body (&key ((:merge-branch branch))
                          ((:cached-key cache))
                        &allow-other-keys)
  (cond ((and branch cache)
         `(let ((key1 (funcall key (car list1)))
                (key2 (funcall key (car list2))))
            (macrolet ((merge-one (l1 k1 l2)
                         `(progn
                            (setf (cdr tail) ,l1
                                  tail       ,l1)
                            (let ((rest (cdr ,l1)))
                              (cond (rest
                                     (setf ,l1 rest
                                           ,k1 (funcall key (first rest))))
                                    (t
                                     (setf (cdr ,l1) ,l2)
                                     (return (cdr head))))))))
              (loop
               (if (funcall test key2  ; this way, equivalent
                                 key1) ; values are first popped
                   (merge-one list2 key2 list1) ; from list1
                   (merge-one list1 key1 list2))))))
        (branch ;; branch, no cache
         `(macrolet ((merge-one (l1 l2)
                       `(progn
                          (setf (cdr tail) ,l1
                                tail       ,l1)
                          (let ((rest (cdr ,l1)))
                            (cond (rest
                                   (setf ,l1 rest))
                                  (t
                                   (setf (cdr ,l1) ,l2)
                                   (return (cdr head))))))))
            (loop
             (if (funcall test (funcall key (car list2))
                          (funcall key (car list1)))
                 (merge-one list2 list1)
                 (merge-one list1 list2)))))
        (cache ;; no branch, cache
         `(flet ((merge-one (cells least)
                   (declare (type (simple-vector 4) cells)
                            (type bit least))
                   (let* ((l1   (aref cells least))
                          (rest (cdr l1)))
                     (declare (type cons l1))
                     (setf (cdr tail) l1
                           tail       l1)
                     (cond (rest
                            (setf (aref cells least) rest
                                  (aref cells (+ least 2))
                                  (funcall key (car rest))))
                           (t
                            (setf (cdr l1) (aref cells (logxor least 1)))
                            (return-from %merge (cdr head)))))))
            (let ((cells (vector list1
                                 list2
                                 (funcall key (car list1))
                                 (funcall key (car list2)))))
              (declare (dynamic-extent cells))
              (loop
               (merge-one cells
                          (if (funcall test
                                       (aref cells 3)
                                       (aref cells 2))
                              1
                              0))))))
        (t ;; no branch, no cache
         `(flet ((merge-one (cells least)
                   (declare (type (simple-vector 2) cells)
                            (type bit least))
                   (let* ((l1   (aref cells least))
                          (rest (cdr l1)))
                     (declare (type cons l1))
                     (setf (cdr tail) l1
                           tail       l1)
                     (cond (rest
                            (setf (aref cells least) rest))
                           (t
                            (setf (cdr l1) (aref cells (logxor least 1)))
                            (return-from %merge (cdr head)))))))
            (let ((cells (vector list1 list2)))
              (declare (dynamic-extent cells))
              (loop
               (merge-one cells
                          (if (funcall test
                                       (funcall key (car (aref cells 1)))
                                       (funcall key (car (aref cells 0))))
                              1
                              0))))))))

(make-all-sorts)

(deftype index ()
  `(mod ,most-positive-fixnum))

(defun shuffle-conses (list n &optional (*random-state* *random-state*))
  (declare (type list list) (type index n))
  (when (null list)
    (return-from shuffle-conses list))
  (let* ((length (length list))
         (values (coerce list 'simple-vector))
         (cells  (make-array length))
         (state *random-state*))
    (loop for cell on list by #'cdr
          for i below length
          do (setf (aref cells i) cell))
    (flet ((swap (i j k)
             (declare (type index i j k)
                      (optimize speed))
             (let ((n (min (1+ (- j i))
                           (- length k))))
               (loop repeat n
                     for src of-type index upfrom i
                     for dst of-type index upfrom k
                     do (rotatef (aref cells src)
                                 (aref cells dst))))))
      (cond ((>= n length)
             (alexandria:shuffle cells))
            (t
             (dotimes (i n)
               (let ((i (random length state))
                     (j (random length state))
                     (k (random length state)))
                 (when (> i j)
                   (rotatef i j))
                 (when (> i k)
                   (rotatef i k))
                 (when (> j k)
                   (rotatef j k))
                 (swap i j k)))))
      (loop for i from 1 below length
            do (setf (cdr (aref cells (1- i)))
                     (aref cells i))
            finally (setf (cdr (aref cells (1- length))) nil)
                    (return (replace (aref cells 0) values))))))

(defun shuffle-values (values n &optional (*random-state* *random-state*))
  (declare (type index n))
  (let* ((length  (length values))
         (scratch (make-array length))
         (state *random-state*))
    (declare (type (and unsigned-byte fixnum) length))
    (typecase values
        (list
         (replace scratch values))
        (simple-vector
         (replace scratch values))
        (t
         (replace scratch values)))
    (flet ((swap (i j)
             (declare (type index i j)
                      (optimize speed))
             (loop repeat (truncate (- j i) 2)
                   for src of-type index upfrom i
                   for dst of-type index downfrom j
                   do (rotatef (aref scratch src)
                               (aref scratch dst)))))
      (cond ((>= n length)
             (alexandria:shuffle scratch))
            (t
             (dotimes (i n)
               (let ((i (random length state))
                     (j (random length state)))
                 (when (> i j)
                   (rotatef i j))
                 (swap i j)))))
      (typecase values
        (list
         (assert (= (length values) length))
         (assert (= (length scratch) length))
         (prog1 (replace values scratch)
           (assert (= (length values) length))))
        (simple-vector
         (replace values scratch))
        (t
         (replace values scratch))))))

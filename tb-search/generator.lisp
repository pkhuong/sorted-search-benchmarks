(require "alexandria")

(defun offset-quaternary-sequence (size)
  (let* ((chunk (ceiling (* 16 size) 63))
         (tail  (- size chunk)))
    (cond ((> size 3)
           (format t "~C~30@<QUATERNARY(~Du, ~Du, ~Du);~> ~
/* ~10:<~D~> -> ~10@<~D~> */~%"
                   #\Tab chunk (* 2 chunk) tail
                   size chunk)
           chunk)
          (t (ternary-sequence size)))))

(defun quaternary-sequence (size)
  (let* ((chunk (ceiling size 4))
         (tail  (- size chunk)))
    (cond ((> size 3)
           (format t "~C~30@<QUATERNARY(~Du, ~Du, ~Du);~> ~
/* ~10:<~D~> -> ~10@<~D~> */~%"
                   #\Tab chunk (* 2 chunk) tail
                   size chunk)
           chunk)
          (t
           (ternary-sequence size)))))

(defun ternary-sequence (size)
  (let* ((chunk (ceiling size 3))
         (tail  (- size chunk)))
    (cond ((/= chunk tail)
           (format t "~C~30@<TERNARY(~Du, ~Du);~> ~
/* ~10:<~D~> -> ~10@<~D~> */~%"
                   #\Tab chunk tail
                   size chunk)
           chunk)
          (t (binary-sequence size)))))

(defun binary-sequence (size)
  (assert (> size 1))
  (let* ((chunk (ceiling size 2))
         (tail  (- size chunk)))
    (format t "~C~30@<BINARY(~Du);~> ~
/* ~10:<~D~> -> ~10@<~D~> */~%"
            #\Tab tail size chunk)
    chunk))

(defun offset-sequence (size)
  (let* ((chunk (ceiling (* size 32) 63))
         (tail  (- size chunk)))
    (format t "~C~30@<BINARY(~Du);~> ~
/* ~10:<~D~> -> ~10@<~D~> */~%"
            #\Tab tail size chunk)
    chunk))

(defun tb-sequence (name size &key (crossover :sqrt))
  (let ((crossover (ecase crossover
                     ((sqrt :sqrt) (sqrt size))
                     ((nil)          0)
                     (t            most-positive-fixnum))))
    (format t "~
size_t ~A (unsigned key, unsigned * vector)
\{
	unsigned * base = vector;
"
            name)
    (loop while (> size 1)
          do (setf size
                   (funcall (cond ((= size 3)
                                   'ternary-sequence)
                                  ((= size 2)
                                   'binary-sequence)
                                  ((< size crossover)
                                   'binary-sequence)
                                  (t
                                   'ternary-sequence))
                            size)))
    (format t "	return base-vector;
\}
")))

(defun simple-sequence (name size &optional (default 'quaternary-sequence))
  (format t "~
size_t ~A (unsigned key, unsigned * vector)
\{
	unsigned * base = vector;
"
          name)
  (loop while (> size 1)
        do (setf size
                 (funcall (cond ((= size 3)
                                 'ternary-sequence)
                                ((= size 2)
                                 'binary-sequence)
                                (t
                                 default))
                          size)))
  (format t "	return base-vector;
\}
"))

(defun emit-sequences (format max-lb crossover)
  (loop for i from 3 upto max-lb
        do (tb-sequence (format nil format i) (ash 1 i)
                        :crossover crossover)
           (when (< i max-lb)
             (format t "~%"))))

(defun emit (&optional (max 25))
  (with-open-file (*standard-output* "tsearch.inc"
                                     :direction :output
                                     :if-exists :supersede)
    (emit-sequences "t_~D" max nil)
    (format t "~%search_fun ~%ternary[] = {NULL, NULL, NULL, ~{t_~D~^, ~}};~%"
            (alexandria:iota (- max 2) :start 3)))
  (with-open-file (*standard-output* "bsearch.inc"
                                     :direction :output
                                     :if-exists :supersede)
    (emit-sequences "b_~D" max t)
    (format t "~%search_fun ~%binary[] = {NULL, NULL, NULL, ~{b_~D~^, ~}};~%"
            (alexandria:iota (- max 2) :start 3)))
  (with-open-file (*standard-output* "tbsearch.inc"
                                     :direction :output
                                     :if-exists :supersede)
    (emit-sequences "tb_~D" max :sqrt)
    (format t "~%search_fun ~%tb[] = {NULL, NULL, NULL, ~{tb_~D~^, ~}};~%"
            (alexandria:iota (- max 2) :start 3)))
  (with-open-file (*standard-output* "qsearch.inc"
                                     :direction :output
                                     :if-exists :supersede)
    (loop for i from 3 upto max do
      (simple-sequence (format nil "q_~D" i) (ash 1 i) 'quaternary-sequence)
      (when (< i max)
        (format t "~%")))
    (format t "~%search_fun ~%quat[] = {NULL, NULL, NULL, ~{q_~D~^, ~}};~%"
            (alexandria:iota (- max 2) :start 3)))
  (with-open-file (*standard-output* "osearch.inc"
                                     :direction :output
                                     :if-exists :supersede)
    (loop for i from 3 upto max do
      (simple-sequence (format nil "o_~D" i) (ash 1 i) 'offset-sequence)
      (when (< i max)
        (format t "~%")))
    (format t "~%search_fun ~%offset[] = {NULL, NULL, NULL, ~{o_~D~^, ~}};~%"
            (alexandria:iota (- max 2) :start 3)))
  (with-open-file (*standard-output* "oqsearch.inc"
                                     :direction :output
                                     :if-exists :supersede)
    (loop for i from 3 upto max do
      (simple-sequence (format nil "oq_~D" i) (ash 1 i)
                       'offset-quaternary-sequence)
      (when (< i max)
        (format t "~%")))
    (format t "~%search_fun ~%off_quat[] = {NULL, NULL, NULL, ~{oq_~D~^, ~}};~%"
            (alexandria:iota (- max 2) :start 3))))

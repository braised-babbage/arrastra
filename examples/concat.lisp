(defpackage #:concat
  (:use #:cl #:arrastra))

(in-package #:concat)

(defun make-sources-target ()
  (make-instance 'target
		 :task (sources "concat-data/")))

(defun make-concat-target (sources)
  (flet ((task ()
	   (let ((output-path (merge-pathnames #P"concat.txt"
					       *target-output-directory*)))
	     (with-open-file (out output-path :direction :output :if-exists :supersede)
	       (loop :for input :in (target-value sources)
		     :do (write-string (alexandria:read-file-into-string input)
				       out)))
	     output-path)))
    (make-instance 'target
		   :dependencies (list sources)
		   :task #'task)))

(defvar sources (make-sources-target))
(defvar concat (make-concat-target sources))

(defclass compound-target (target)
  ((sources :initform nil)
   (concat :initform nil)))

(defmethod initialize-instance :after ((obj compound-target) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value obj 'sources)
	(make-sources-target))
  (setf (slot-value obj 'concat)
	(make-concat-target (slot-value obj 'sources))))

(defvar foo (make-instance 'compound-target))
(defvar bar (make-instance 'compound-target))


(defun run-all ()
  (let ((*output-root-directory* "concat-out/"))
    (run-all-targets :concat)))

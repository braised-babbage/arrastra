(defpackage #:concat-simple
  (:use #:cl #:arrastra))

(in-package #:concat-simple)

(defvar sources
  (make-instance 'target
		 :task (lambda ()
			 (uiop:directory-files
			  (uiop:ensure-pathname "concat-data/")))))


(defvar concat
  (flet ((do-concat ()
	   (let ((output-path (merge-pathnames #P"concat.txt"
					       *target-output-directory*)))
			   (with-open-file (out output-path :direction :output
							    :if-exists :supersede)
			     (loop :for input :in (target-value sources)
				   :do (write-string
					(alexandria:read-file-into-string input)
					out)))
	     output-path)))
    (make-instance 'target
		   :dependencies (list sources)
		   :task #'do-concat)))

(defun eval-concat ()
  (let ((*output-root-directory* "concat-out/"))
    (eval-target 'concat)))

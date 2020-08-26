(in-package #:arrastra)

(defvar *output-root-directory*)
(setf (documentation '*output-root-directory* 'variable)
      "The root directory for target output.")

(defvar *target-output-directory*)
(setf (documentation '*target-dest-directory* 'variable)
      "The output directory assigned to the active target.")

(defclass target ()
  ((dependencies :initarg :dependencies :initform nil
		 :reader target-dependencies)
   (task :initarg :task
	 :initform (constantly nil)
	 :accessor target-task)
   (value :initform nil)
   (inferred-name :initform nil
		  :accessor inferred-name
		  :documentation "The inferred name of the target. You probably shouldn't mess with this....")))


(defmethod print-object ((obj target) stream)
  "Print a target, with its name."
  (print-unreadable-object (obj stream :type t :identity t)
    (when (inferred-name obj)
      (format stream "~S" (inferred-name obj)))))


(defun target-p (obj)
  "Is OBJ a target?"
  (typep obj 'target))


(defun canonical-name (symbol)
  "Get a name from a symbol."
  (string-downcase (symbol-name symbol)))

(defun target-children (target)
  "Get all chilren of TARGET."
  (loop :for slot-def :in (closer-mop:class-slots (class-of target))
	:for slot-name := (closer-mop:slot-definition-name slot-def)
	:for child := (and (slot-boundp target slot-name)
			   (slot-value target slot-name))
	:when (target-p child)
	  :do (setf (inferred-name child)
		      (canonical-name slot-name))
	  :and 
	    :collect child))


(defun target-descendants-list (target)
  "Get a list of all descendants of TARGET."
  (loop :for child :in (target-children target)
	:collect child
	:append (target-descendants-list child)))


(defun target-subtree (target)
  "Get the subtree rooted at TARGET."
  (cons target (mapcar #'target-subtree (target-children target))))


;;; The call graph (the "task depends on" relation)
;;; - construct from single target
;;; - for a given target, its immediate dependencies
;;; - for a given target, topologically sorted list of all dependencies



;;; TODO: routine for walking the object hierarchy?
;;; TODO: routine for walking the call graph?

(defclass target-hierarchy ()
  ((root-package :initarg :root-package
		 :accessor target-hierarchy-root-package
		 :documentation "The package from which the target-hierarchy was produced.")
   (roots :initarg :roots
	  :accessor target-hierarchy-roots
	  :documentation "A list of roots of the object hierarchy.")
   (parents :initarg :parents
	    :accessor target-hierarchy-parents
	    :documentation "A hash table mapping targets to their parents.")
   (paths :initarg :paths
	  :accessor target-hierarchy-paths
	  :documentation "A hash table mapping targets to their (relative) paths."))
  (:documentation "A hierarchy of targets associated with a package.

A target A is a 'parent' of another target B if B is the value of one of the slots of 
A. In such a relationship, target B gets an INFERRED-NAME, equal to the corresponding
slot name. 'Root' targets are those who have no parents; they correspond to toplevel 
definition, and their INFERRED-NAME is the name of the bound variable or parameter."))

(defvar *target-hierarchy*)
(setf (documentation '*target-hierarchy* 'variable)
      "The active TARGET-HIERARCHY, used e.g. when evaluating targets.")


(defun resolve-target-hierarchy (package)
  "Construct a TARGET-HIERARCHY for targets in PACKAGE."
  (unless (packagep package)
    (setf package (find-package package)))
  (let ((roots nil)
	(parents (make-hash-table :test 'eq))
	(paths (make-hash-table :test 'eq)))
    (labels ((resolve-subtree (target)
	       (loop :for child :in (target-children target)
		     :for (parent seenp) := (multiple-value-list (gethash child parents))
		     :do (cond ((eq parent target)
				(error "Target ~A appears in multiple slots on ~A."
				       child target))
			       (seenp
				(error "Target ~A appears at multiple positions in the object hierarchy."
				       child))
			       (t
				(setf (gethash child parents) target)))
		     :do (resolve-subtree child))))
      ;; fill parents table
      (do-symbols (s package)
	(let ((obj (and (boundp s) (symbol-value s))))
	  (when (target-p obj)
	    (setf (inferred-name obj) (canonical-name s)
		  (gethash obj parents) nil)
	    (resolve-subtree obj)))))

    ;; fill paths & gather roots
    (loop :for target :being :the :hash-keys :of parents
	    :using (hash-value parent)
	  :do (setf (gethash target paths)
		    (target-relative-path target parents))
	  :when (null parent)
	    :do (push target roots))

    (make-instance 'target-hierarchy
		   :root-package package
		   :roots roots
		   :parents parents
		   :paths paths)))


(defun target-relative-path (target parents)
  "Compute the relative path from root to TARGET using the PARENTS table."
  (let ((dirs (loop :with obj := target
		    :while obj
		    :collect (inferred-name obj)
		    :do (setf obj (gethash obj parents)))))
    (make-pathname :directory (cons ':relative (nreverse dirs)))))



(defun resolve-target (target-name &rest slots)
  "Find a TARGET given a name and slots. 

Example: (RESOLVE-TARGET 'FOO::BAR) attempts find a target named BAR 
in package FOO.

Another example: (RESOLVE-TARGET 'FOO::BAR 'BAZ 'FROB) roughly corresponds
to (SLOT-VALUE (SLOT-VALUE FOO::BAR 'FOO::BAZ) 'FOO::FROB)."
  (loop :with target := (symbol-value target-name)
	:for slot-name :in slots
	:for actual-name := (find-symbol (symbol-name slot-name)
					 (symbol-package target-name))
	:do (setf target (slot-value target actual-name))
	:finally (return target)))


;;; Evaluating a target
;;; - build object hierarchy
;;; - build call graph from target
;;; - set up variables (e.g. *output-directory*)
;;; - if leaf node:
;;;   - evaluate task
;;;   - save metadata (output hash-code, serialized value)
;;;   - return result
;;; - if inner node:
;;;   - evaluate tasks we immediately depend on, obtaining a value and a hash
;;;   - if input hashes match what we have in metadata, read and return serialized value
;;;   - otherwise, evaluate task, save metadata, return result

;;; metadata
;;; - input hash
;;; - output hash
;;; - serialized value (use cl-store)


(defun target-value (target)
  "Determine the value of TARGET, relative to the ambient *TARGET-HIERARCHY*"
  (let ((*target-output-directory*
	  (merge-pathnames (gethash target (target-hierarchy-paths *target-hierarchy*))
			   *output-root-directory*)))
    (ensure-directories-exist *target-output-directory*)
    (when (null (slot-value target 'value))
      (setf (slot-value target 'value)
	    (funcall (target-task target))))
    (slot-value target 'value)))


(defun eval-target (target-name &rest slots)
  "Evaluate the target indicated by TARGET-NAME and SLOTS."
  (check-type target-name symbol)
  (let ((*target-hierarchy* (resolve-target-hierarchy (symbol-package target-name))))
    (let ((target (apply #'resolve-target target-name slots)))
      (target-value target))))


(defun run-all-targets (package)
  (unless (packagep package)
    (setf package (find-package package)))
  (let ((*target-hierarchy* (resolve-target-hierarchy package)))
    (loop :for target :being :the :hash-keys :of (target-hierarchy-paths *target-hierarchy*)
	  :do (target-value target))
    nil))


;;; some utils

(defun sources (path)
  (lambda ()
    (uiop:directory-files (uiop:ensure-pathname path :ensure-directory t))))

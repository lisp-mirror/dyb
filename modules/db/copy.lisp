(in-package :ems)

;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;; Generic (shallow) object copying for CLOS
;; Author: Michael Weber <michaelw@foldr.org>, 2008

;;; Notes:
;; This is a simple, but rather slow method of copying objects.  It
;; would be faster if COPY-INSTANCE methods would be partially
;; specialized on the class.  In other words: unroll the slot-copying
;; loop, and optoinally get rid of SLOT-BOUNDP checks if the users
;; says it is safe.  Care would need to be taken to regenerate this
;; specialized method if the result of COPY-CLASS-SLOTS changes,
;; either by class redefinition, or by adding methods on
;; COPY-CLASS-SLOTS.

;;;; Code
(defgeneric copy-class-slots (class)
  (:documentation "Returns the set of slots of CLASS which are
considered for copying by COPY-INSTANCE.

If CLASS is of type STANDARD-CLASS, all slots \(as returned by
CLASS-SLOTS) are considered.")
  (:method ((class standard-class))
    (class-slots class)))

(defgeneric make-uninitialized-instance (class)
  (:documentation "Allocates a fresh uninitialized instance of the
given class CLASS.

If CLASS is of type CLASS, ALLOCATE-INSTANCE is used.")
  (:method ((class class))
    (allocate-instance class)))

(defgeneric copy-object (object intentional-type &rest initargs &key  &allow-other-keys)
  (:documentation "Makes and returns a \(shallow) copy of OBJECT.

An uninitialized object of the same class as OBJECT is allocated by
calling MAKE-UNINITIALIZED-INSTANCE.  For all slots returned by
\(COPY-CLASS-SLOTS \(CLASS-OF OBJECT)), the returned object has the
same slot values and slot-unbound status as OBJECT.

REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) (type (eql 'standard-class)) 
            &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
           (copy (make-uninitialized-instance class)))
      (dolist (slot (copy-class-slots class))
        (let ((slot-name (slot-definition-name slot)))
          (when (slot-boundp object slot-name)
            (setf (slot-value copy slot-name)
                  (slot-value object slot-name)))))
      (apply #'reinitialize-instance copy initargs))))


(defmethod copy-object ((object cons) (type (eql 'cons)) &key &allow-other-keys )
    (cons (car object) (cdr object)))

(defmethod copy-object ((object list) (type (eql 'list)) &key &allow-other-keys)
  (copy-list object))

(defmethod copy-object ((object list) (type (eql 'alist)) &key &allow-other-keys)
  (copy-alist object))

(defmethod copy-object (object (type (eql 'cons-tree)) &key &allow-other-keys)
  (copy-tree object))

(defun copy (object &optional (type (type-of object)))
  (cond ((member type '(cons list alist cons-tree standard-class))
         (copy-object object type))
        ((typep object 'standard-object)
         (copy-object object 'standard-class))))

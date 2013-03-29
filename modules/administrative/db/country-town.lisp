(in-package :dyb)

(defclass municipality ()
  ((district-municipality :initarg :district-municipality 
                          :initform nil
                          :accessor district-municipality)
   (local-municipality :initarg :local-municipality 
                       :initform nil
                       :accessor local-municipality))
  (:metaclass storable-versioned-class))

(defclass country-town (doc)
  ((country :initarg :country 
            :initform nil
            :accessor country
            :key t)
   (province :initarg :province 
             :initform nil
             :accessor province
             :key t)
   (municipalities :initarg :municipalities
                   :initform nil
                   :accessor municipalities
                   :documentation "List of municipalities.")
   (town :initarg :town
         :initform nil
         :accessor town
         :key t)
   (longitude :initarg :longitude
              :initform nil
              :accessor longitude)
   (latitude :initarg :latitude
             :initform nil
             :accessor latitude)
   (guess-p :initarg :guess-p
            :initform nil))
  (:metaclass storable-versioned-class)
  (:default-initargs :doc-type "country-town"))

(defun country-towns-collection ()
  (get-collection (system-db) "country-towns"))

(defun country-towns ()
  (docs (country-towns-collection)))

(defmethod doc-collection ((doc country-town))
  (country-towns-collection))

(defun make-country-town (country province town 
                          &key longitude latitude municipalities guess-p crap-p)
  (make-instance 'country-town
                 :doc-type "country-town" 
                 :xid (if crap-p
                          nil
                          0)
                 :country country
                 :province province
                 :town town
                 :longitude longitude
                 :latitude latitude
                 :municipalities municipalities
                 :guess-p guess-p))

(defun get-country-town (country province town)
  (find-doc (country-towns-collection) 
            :test
            (lambda (doc)
              (and             
               (string-equal (get-val doc 'country) country)
               (string-equal (get-val doc 'province) province)
               (string-equal (get-val doc 'town) town)))))

(defun find-country-town (country province town)
  (let ((country-town (get-country-town country province town)))
    
    (unless  country-town
      
      (setf country-town (make-country-town country province town :crap-p T))
      (setf (get-val country-town 'xid) nil)
      (persist country-town)
      (setf country-town (get-country-town country province town))

      ;forcing crep country-town record(xid) to be nil 
      (setf (get-val country-town 'xid) nil)
      (persist country-town)
      (setf country-town (get-country-town country province town)))
    
    country-town))

(defun get-country-town-by-id (id)
  (get-doc (country-towns-collection) id
                       :element 'xid))

(defgeneric match-country-towns (doc country-towns))

(defmethod match-country-towns (doc country-towns)
  (find (get-val doc 'xid) country-towns))

(add-collection (system-db) "country-towns" 
                :collection-class 'dyb-collection)


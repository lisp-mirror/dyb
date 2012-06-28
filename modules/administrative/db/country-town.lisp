(in-package :ems)

(defclass municipality ()
  ((district-municipality :initarg :district-municipality 
                          :initform nil)
   (local-municipality :initarg :local-municipality 
                       :initform nil))
  (:metaclass storable-class))

(defclass country-town (doc)
  ((country :initarg :country 
            :initform nil
            :accessor country)
   (province :initarg :province 
             :initform nil
             :accessor province)
   (municipalities :initarg :municipalities
                   :initform nil
                   :accessor municipalities
                   :documentation "List of municipalities.")
   (town :initarg :town
         :initform nil
         :accessor town)
   (longitude :initarg :longitude
              :initform nil
              :accessor longitude)
   (latitude :initarg :latitude
             :initform nil
             :accessor latitude)
   (guess-p :initarg :guess-p
            :initform nil))
  (:metaclass storable-class))

(defun country-towns-collection ()
  (get-collection (system-db) "country-towns"))

(defun country-towns ()
  (docs (country-towns-collection)))

(defmethod persist-doc ((doc country-town) &key (force-stamp-p t))
  (store-doc (country-towns-collection) doc :force-stamp-p force-stamp-p))

(defun make-country-town (country province town &key longitude latitude municipalities guess-p)
  (make-instance 'country-town :key (list country province town) :doc-type "country-town" 
                 :xid (next-xid (country-towns-collection))
                 :country country
                 :province province
                 :town town
                 :longitude longitude
                 :latitude latitude
                 :municipalities municipalities
                 :guess-p guess-p))

(defun get-country-town (country province town)
  (get-doc (country-towns-collection) (list country province town)))

(defun get-country-town-by-id (id)
  (get-doc (country-towns-collection) id
                       :element 'xid))

(defgeneric match-country-towns (doc country-towns))

(defmethod match-country-towns (doc country-towns)
  (find (get-val doc 'xid) country-towns))

(add-collection (system-db) "country-towns" 
                :collection-class 'ems-collection
                :load-from-file-p t)


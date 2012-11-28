(in-package :dyb)

(defclass generic-post (doc)
  (;;(entity :initarg :entity :accessor entity)
   (channel-user :initarg :channel-user
                 :accessor channel-user)
   (post-type 
    :accessor post-type :initarg :post-type
    :documentation "Twitter,Facebook,LinkedIn,Wordpress")
   (payload 
    :accessor payload :initarg :payload
    :documentation "Usually the raw json or xml retrieved in an foreign api call")
   (payload-type 
    :accessor payload-type :initarg :payload-type
    :documentation "Subtype of post, it might influence how to parse payload.")
   (payload-source 
    :accessor payload-source :initarg :payload-source
    :documentation "There are differences between streams and rest calls for the same type of post. Once again this could influence the parsing of the payload.")
   (payload-parser-version 
    :accessor payload-parser-version :initarg :payload-parser-version
    :documentation "As api's change over time we need to be able to select the correct version of the parser.")
   (created-date 
    :accessor created-date :initarg :created-date
    :documentation "The creation date of the post. (Not the retrieval date that is the stamp).")
   (post-data :initarg :post-data
              :initform (make-hash-table :test 'equal))
   (last-change-date 
    :accessor last-change-date :initarg :last-change-date
    :documentation "The last time the post was changed. This should be used to sort inbox latest post activity. IE if a post has a new comment or something it should be brought to the users attetion."))
  (:metaclass storable-class))

(defun generic-post-collection ()
  (get-collection (system-db) "generic-post"))

(defun generic-posts ()
  (docs (generic-post-collection)))

(defun get-generic-post-by-id (id)
  (get-doc (generic-post-collection) id
           :element 'id))

(defun get-generic-post-by-post-id (id)
  (find-doc  
   (generic-post-collection)
    :test (lambda (doc)
            (string-equal id (or (gpv doc :id--str) (gpv doc :id)))) 
            ))

(defmethod doc-collection ((doc generic-post))
  (generic-post-collection))

(defun make-generic-post (channel-user post-type payload payload-source created-date 
                          &key payload-type last-change-date payload-parser-version)
  (make-instance 'generic-post
                 :key (list (xid channel-user) post-type (raw-post-id payload post-type))
                 :channel-user channel-user
                 :post-type post-type
                 :payload payload
                 :payload-source payload-source
                 :created-date created-date
                 :payload-type payload-type
                 :last-change-date last-change-date
                 :payload-parser-version payload-parser-version))

(unless (generic-post-collection)
  (add-collection (system-db) "generic-post" 
                  :collection-class 'dyb-collection
                  :load-from-file-p t))

(defgeneric gpkv (post &rest keys))

(defmethod gpkv (post &rest keys)
  (apply #'assoc-path post (flatten keys)))

(defmethod gpkv ((post generic-post) &rest keys)
  (apply #'assoc-path (payload post) (flatten keys)))

(defgeneric gpv (post &rest keys))

(defmethod gpv (post &rest keys)
  (cdr (gpkv post keys)))

(defgeneric post-id (generic-post))

(defmethod post-id ((post generic-post))
  (raw-post-id post (post-type post)))

(defgeneric post-text (generic-post))

(defmethod post-text ((post generic-post))
  (raw-post-text post (post-type post)))

(defgeneric post-user (generic-post))

(defmethod post-user ((post generic-post))
  (raw-post-user post (post-type post)))

(defgeneric post-user-name (generic-post))

(defmethod post-user-name ((post generic-post))
  (raw-post-user-name post (post-type post)))

(defgeneric post-user-id (generic-post))

(defmethod post-user-id ((post generic-post))
  (raw-post-user-id post (post-type post)))

(defgeneric raw-post-id (post post-type))

(defgeneric raw-post-text (post post-type))

(defgeneric raw-user (post post-type))

(defgeneric raw-user-name (post post-type))

(defgeneric raw-user-id (post post-type))


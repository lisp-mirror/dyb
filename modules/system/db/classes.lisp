(in-package :dyb)

(defclass service-app (document)
  ((client :initarg :client
           :accessor client)
   (service :initarg :service
            :accessor :service)
   (identifier :initarg :identifier
         :accessor identifier)
   (app-key :initarg :app-key
        :initform nil
        :accessor app-key)
   (app-secret :initarg :app-secret
           :initform nil
           :accessor app-secret)
   (callback-uri :initarg :callback-uri
                 :initform nil
                 :accessor callback-uri)
   (callback-port :initarg :callback-port
                  :initform 8000
                  :accessor callback-port)
   (request-token-endpoint :initarg :request-token-endpoint
                               :initform nil
                               :accessor request-token-endpoint)
   (auth-request-token-endpoint :initarg :auth-request-token-endpoint
                                :initform nil
                                :accessor auth-request-token-endpoint)
   (access-token-endpoint :initarg :access-token-endpoint
                              :initform nil
                              :accessor access-token-endpoint)
   (consumer-token :initarg :consumer-token
                   :initform ""
                   :accessor consumer-token))
  ;(:metaclass storable-class)
  )


(defmethod persist ((service service-app))
  (setf (doc-type service) 'service-app)
  (store-doc (services) service))

(defun get-service (client identifier service)
;  (break "huh? ~A" (services))
  (find-doc (services)
   :test
   (lambda (doc)

(if (listp doc)
    (setf doc (first doc)))

;(break "fuck ~A" doc (get-val doc 'service))
     (when (equal (doc-type doc) 'service-app)
     ;  (break "~A ~A ~A" (client doc) (get-val doc 'service) (identifier doc))
         (and (equal (client doc) client)
              (equal (get-val doc 'service) service)
              (equal (identifier doc) identifier))))))


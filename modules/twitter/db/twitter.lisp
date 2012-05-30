(in-package :ems)

(defclass twitter-user (service-user)
  ()
  ;(:metaclass storable-class)
  )

(defmethod persist ((user twitter-user))
  (setf (doc-type user) 'service-user)
  (setf (get-val user 'key) (list (get-val user 'client) (get-val user 'email) 'service-user))
  ;(setf (key user) (list (client user) (email user)))
  (setf (service user) 'twitter)
;(break "~A" user)
  (store-doc (users) user))

(defun get-twitter-user (client email)
  (get-service-user client email 'twitter))

(defun get-twitter-user-by-id (identifier)
  (find-doc (users)
   :test
   (lambda (doc)
     (equal (identifier doc) identifier))))

(defclass twitter-service (service-app)
  ()
 ;(:metaclass storable-class)
)

(defmethod persist ((service twitter-service))
  (setf (key service) (identifier service))
  (setf (service service) 'twitter)
  (store-doc (services) service))

(defun get-twitter-service (client identifier)
  (get-service client identifier 'twitter))


(defun make-twitter-service-app (client indentifier &key key secret callback-uri port)
  (let ((app (make-instance 
              'service-app 
 
              :service 'twitter
              :identifier indentifier
              :client client
              :app-key key
              :app-secret secret
              :callback-uri callback-uri
              :callback-port port
              :request-token-endpoint "http://twitter.com/oauth/request_token"
              :access-token-endpoint "http://twitter.com/oauth/access_token"
              :consumer-token (cl-oauth:make-consumer-token 
                               :key key 
                               :secret secret))))
;(break "aaaaaaaaaaa ~a" app)
    (setf *app* app)))
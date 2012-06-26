(in-package #:ems)

(defclass facebook-list (widget)
  ((posts :initarg :posts)))

(defmethod render ((widget facebook-list) &key)
  (with-html
    (let ((box (make-widget 'peach-box :name (format nil "~A-box" "fuck"))))
      (setf (header box) "Facebook Posts")
      (str (render box
                   :content
                   (with-html-output-to-string (*standard-output*)

                     (:ul :class "posts-list"
                          (dolist (post (get-val widget 'posts))
                            (htm
                             (:li
                            
                              (:span (str (get-val (get-val post 'from) 'name)))
                              (str "|")
                              (:span (str (get-val post 'message)))
        
                              )))))
                   :actions
                   (with-html-to-string ()
                     (:div :class "actions-left")
                           (:div :class "actions-right"
                                 (:a :class "button" :href "#" "Got to stats &raquo;")))))
      ))
  (with-html
    ))
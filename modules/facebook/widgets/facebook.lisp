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
                            
                              (:span (str (get-post-title post)))
			      (str (nulable-section "likes : " (get-val (get-val post 'likes) 'count)))
			      ;(:span (str (count-likes (get-val post 'likes))))
			      (dolist (com (get-val (get-val post 'comments) 'data)) 
				(str (nulable-section "comment : " (get-val com 'message)))
				(str (nulable-section "&nbsp;&nbsp;likes : " (get-val com 'likes))))
			      ;(str (nulable-section-two 
				   ; "Comment count = " 
				   ; (get-val (get-val post 'comments) 'count)))
			      ;(:span (str (count-comments (get-val post 'comments))))
                              )))))
                   :actions
                   (with-html-to-string ()
                     (:div :class "actions-left")
                           (:div :class "actions-right"
                                 (:a :class "button" :href "#" "Got to stats &raquo;")))))
      ))
  (with-html
    ))
(defun nulable-section (title content)
  (if content (format t "<br/>&nbsp;&nbsp;~A ~A" title content)))
(defun nulable-section-two (title content)
  (if (> content 0) (format t "<br/>&nbsp;&nbsp;~A ~A" title content)))

(defun get-post-title (post)
  (let (
    (msg (get-val post 'message))
    (story (get-val post 'story))
  )
  (if msg msg story)))
(defun count-likes (post)
  (let (
	(count (get-val post 'count))
	)
    (if count count 0)))
(defun count-comments (post)
;  (let* (
	(get-val post 'count))
;	 (ctr (get-val com 'count))
;	)
;   (type-of  (post-comments post)));)
   ; (if com (get-val com 'count) 0)))
   ; (type-of com)))
;(defun get-byforce (post)
;  (comments-count (post-comments post)))

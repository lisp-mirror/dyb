
(defun xxx (channel)
  (let ((id-hash (make-hash-table :test 'equal)))
    (find-docs 'list
               (lambda (doc)
                 (when (string-equal (post-type doc) channel)
                   (let ((post (gethash (post-id doc) id-hash))
                         (duplicate nil))
                     (when post
                       
                       (setf duplicate t))
                     (unless post
                       (setf (gethash (post-id doc) id-hash) doc))
                     duplicate)
                   ))
               (generic-post-collection))))

(defun yyy (channel)
  (let ((id-hash (make-hash-table :test 'equal)))
    (find-docs 'list
               (lambda (doc)
                 (when (string-equal (post-type doc) channel)
                   doc
                   ))
               (generic-post-collection))))

(defun zzz (payload-source)
  (find-docs 'list
               (lambda (doc)
                 (when (string-equal (payload-source doc) payload-source)
                   doc
                   ))
               (generic-post-collection)))

(defun delete-webfantix-tweets ()
  (find-docs 'list
               (lambda (doc)
                 (when (string-equal (post-type doc) "Twitter")
                   (when (string-equal (get-val (channel-user doc) 'channel-user-name) "webfanatix")
                     (remove-doc doc)
                     
                     (setf doc nil)
                     t)
                   ))
               (generic-post-collection)))


(defun delete-duplicate-posts (channel)
  (let ((id-hash (make-hash-table :test 'equal)))
    (find-docs 'list
               (lambda (doc)
                 (when (string-equal (post-type doc) channel)
                   (let ((post (gethash (post-id doc) id-hash))
                         (duplicate nil))
                     (when post
                       (remove-doc doc)
                       (setf doc nil)
                       (setf duplicate t))
                     (unless post
                       (setf (gethash (post-id doc) id-hash) doc))
                     duplicate)
                   ))
               (generic-post-collection))))
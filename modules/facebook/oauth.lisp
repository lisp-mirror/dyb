(in-package :ems)

;;https://graph.facebook.com/oauth/access_token?client_id=YOUR_APP_ID&redirect_uri=YOUR_REDIRECT_URI&client_secret=YOUR_APP_SECRET&code=CODE_GENERATED_BY_FACEBOOK

(defun split-string (string char)
    "Returns a list of substrings of string
divided by char."
    (loop for i = 0 then (1+ j)
          as j = (position char string :start i)
          collect (subseq string i j)
          while j))

(define-easy-handler (callback :uri "/ems/callback") ()
  (if (not (parameter "code"))
    (with-html-output-to-string (*standard-output*)
      ;;Clicking on this link will get you an access code.
      (:a :href "https://www.facebook.com/dialog/oauth?client_id=254949787943221&response_type=code&redirect_uri=http://local.dataxware.co.za:8000/ems/callback" 
               (str "Go Facebooking")))
    (multiple-value-bind (body status)
        ;;Use the access code to get a access token
        (drakma:http-request 
         (format nil "https://graph.facebook.com/oauth/access_token?client_id=254949787943221&redirect_uri=http://local.dataxware.co.za:8000/ems/callback&client_secret=b7ca6b0a7243cb0df0b39d009257e4b2&code=~A" 
                 (parameter "code")))
      (setf (session-value "facebook-access-token") nil)
      (if (equal status 200)
          (let* ((querystring (split-string body #\&))
                 (access-token (split-string (first querystring) #\=))
                 ;;(expiry (split-string (second querystring) #\=))
                 )
            (setf (session-value "facebook-access-token") access-token)
            ;;Fetch something interesting with your access token.
            (multiple-value-bind (bodyx)
                (drakma:http-request 
                 (format nil "https://graph.facebook.com/me?access_token=~A" 
                         (second access-token)))
                (format nil "~A" (json::decode-json-from-string bodyx))))
          (format nil "Something went wrong:~%~A"
                  (json::decode-json-from-string body))))))


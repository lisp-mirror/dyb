(in-package :ems)


;;Test
;;(twitter-oauth 1)

;;;http://jaanus.com/post/1451098316/understanding-the-guts-of-twit
;;;https://dev.twitter.com/docs/auth/creating-signature

;;use the following to test signing
;;;http://quonos.nl/oauthTester/

;;What to look out for when getting 401 errors
;;;http://codingthis.com/programming/php/when-oauth-goes-wrong-debugging-signature-mismatch-issues-in-php/


;; then we do https://api.twitter.com/oauth/authorize?oauth_token=oauth_token what shows login tw dialog
;; screen_name for additional con(m)fort (if in DB)
;; (format nil "~A&~A=~A" *twitter-oauth-authorize-uri* "oauth_token" user-oauth)


;; administration/channel-users.lisp save handler


       

     


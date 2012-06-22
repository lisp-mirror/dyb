(in-package #:ems)


(defun parse-profile (json-list)
  (if (listp json-list)
      (let ((stream ""))
        (dolist (parm json-list)
          (setf stream (concatenate 'string stream
                                    (format nil "~A : ~A~%" (car parm) (cdr parm)))))
        stream))
  
  )

(defun parse-picture (json-list)

  (if (listp json-list)
      (let ((stream ""))
        (dolist (parm json-list)
          
          (when (string-equal (car parm) "picture")
            
              (setf stream (concatenate 'string stream
                                        (cdr parm)))))
        stream)))



(define-easy-handler (ems :uri "/ems/proof") ()
  (with-html-output-to-string (*standard-output*)

    (let ((profile ))
      

      (cond ((parameter "profile")
             (setf profile (json::decode-json-from-string 
                            (drakma::http-request   
                              (format nil "https://graph.facebook.com/~A" 
                                      (if (parameter "token")
                                      (parameter "token")
                                      129436193802362))))))
            ((parameter "posts")
             (setf profile (json::decode-json-from-string 
                             (drakma::http-request 
                              (format nil "https://graph.facebook.com/~A/posts/" 
                                      (if (parameter "token")
                                      (parameter "token")
                                      129436193802362)))))))
      
      (htm
       (:form :action "" :method "post" :name "proof-form"
           
              (:table 
               (:tr
                (:td "Id:"
                     (:select :name "token"
                          (:option :value 100003313046336 "Phil" )
                          (:option :value 129436193802362 "Data X-Ware" )
                          (:option :value 1009173975 "Haji" )))
                (:td 
                 

                 ;;(:input :type "text" :name "token" :value (if (parameter "token")
                 ;;                                                  (parameter "token")
                 ;;                                                  129436193802362))
                 ))
               (:tr (:td  
                         (:textarea :cols "100" :rows "20"
                                    (if (parameter "profile")
                                        (str (parse-profile profile)))))
                    (:td (:img :src (parse-picture profile))
                         ))
               (:tr (:td :colspan 2
                         (:input :type :submit :name "profile" :value "Profile")
                         (:input :type :submit :name "post" :value "Posts")))
               )

           
    
              )))))


(defparameter *jsstr* "{ \"data\": [ { \"id\": \"100003921263922_115757298564978\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"to\": { \"data\": [ { \"name\": \"Mirjana Ivkovic\", \"id\": \"1380667962\" } ] }, \"with_tags\": { \"data\": [ { \"name\": \"Mirjana Ivkovic\", \"id\": \"1380667962\" } ] }, \"message\": \"with prop test\", \"type\": \"status\", \"created_time\": \"2012-06-11T04:11:10+0000\", \"updated_time\": \"2012-06-11T04:11:10+0000\", \"comments\": { \"count\": 0 } }, { \"id\": \"100003921263922_375554962493135\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"message\": \"finally properties\", \"picture\": \"http://vthumb.ak.fbcdn.net/hvthumb-ak-prn1/574144_115732341900807_115731985234176_16135_1243_t.jpg\", \"link\": \"http://www.facebook.com/photo.php?v=115731985234176\", \"source\": \"http://video.ak.fbcdn.net/cfs-ak-ash4/v/439750/647/115731985234176_12137.mp4?oh=723dac8fa9e837b5e6c383d94ca1c101&oe=4FDD0300&__gda__=1339884288_07ea39ee7b8c20ac858a3d7af1a35ef9\", \"name\": \"android cam\", \"description\": \"x45 eyepiece, moon through small scope\", \"properties\": [ { \"name\": \"Length\", \"text\": \"0:42\" } ], \"icon\": \"http://static.ak.fbcdn.net/rsrc.php/v2/yD/r/aS8ecmYRys0.gif\", \"type\": \"video\", \"object_id\": \"115731985234176\", \"application\": { \"name\": \"Video\", \"namespace\": \"video\", \"id\": \"2392950137\" }, \"created_time\": \"2012-06-11T03:07:35+0000\", \"updated_time\": \"2012-06-11T03:07:35+0000\", \"comments\": { \"count\": 0 } }, { \"id\": \"100003921263922_115731985234176\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"message\": \"x45 eyepiece, moon through small scope\", \"picture\": \"http://vthumb.ak.fbcdn.net/hvthumb-ak-prn1/574144_115732341900807_115731985234176_16135_1243_b.jpg\", \"link\": \"http://www.facebook.com/photo.php?v=115731985234176\", \"source\": \"http://video.ak.fbcdn.net/cfs-ak-ash4/v/439750/647/115731985234176_12137.mp4?oh=723dac8fa9e837b5e6c383d94ca1c101&oe=4FDD0300&__gda__=1339884288_07ea39ee7b8c20ac858a3d7af1a35ef9\", \"name\": \"android cam\", \"properties\": [ { \"name\": \"Length\", \"text\": \"0:42\" } ], \"icon\": \"http://static.ak.fbcdn.net/rsrc.php/v2/yD/r/DggDhA4z4tO.gif\", \"place\": { \"id\": \"108151539218136\", \"name\": \"Johannesburg, Gauteng\", \"location\": { \"latitude\": -26.2044, \"longitude\": 28.0456 } }, \"type\": \"video\", \"object_id\": \"115731985234176\", \"application\": { \"name\": \"Video\", \"namespace\": \"video\", \"id\": \"2392950137\" }, \"created_time\": \"2012-06-11T03:03:20+0000\", \"updated_time\": \"2012-06-11T03:03:20+0000\", \"shares\": { \"count\": 1 }, \"comments\": { \"count\": 0 } }, { \"id\": \"100003921263922_250487915057865\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"message\": \"Hobbit trailer, properties?\", \"picture\": \"http://external.ak.fbcdn.net/safe_image.php?d=AQCmEoWgtdxNAJT6&w=130&h=130&url=http\u00253A\u00252F\u00252Fmtv.mtvnimages.com\u00252Furi\u00252Fmgid\u00253Auma\u00253Avideo\u00253Amtv.com\u00253A721245\u00253Fwidth\u00253D140\u002526height\u00253D105\", \"link\": \"http://www.mtv.com/videos/movie-trailers/721245/the-hobbit-an-unexpected-journey.jhtml?fb_ref=fblike_web&fb_source=profile_multiline\", \"source\": \"http://media.mtvnservices.com/mgid:uma:video:mtv.com:721245\", \"name\": \"The Hobbit: An Unexpected Journey\", \"description\": \"Trailer 1 \", \"icon\": \"http://static.ak.fbcdn.net/rsrc.php/v2/yf/r/0HZPW6-lhQu.png\", \"type\": \"video\", \"application\": { \"name\": \"Likes\", \"id\": \"2409997254\" }, \"created_time\": \"2012-06-11T02:57:12+0000\", \"updated_time\": \"2012-06-11T02:57:12+0000\", \"comments\": { \"count\": 0 } }, { \"id\": \"100003921263922_418410954847988\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"message\": \"video example\", \"picture\": \"http://external.ak.fbcdn.net/safe_image.php?d=AQCwFfBBd9evR1qu&w=130&h=130&url=http\u00253A\u00252F\u00252Fi1.ytimg.com\u00252Fvi\u00252Fxiw1XAb8G9A\u00252Fmqdefault.jpg\", \"link\": \"http://www.youtube.com/watch?v=xiw1XAb8G9A&feature=share\", \"source\": \"http://www.youtube.com/v/xiw1XAb8G9A?version=3&autohide=1&autoplay=1\", \"name\": \"North Sea Jazz 2009 Live - Joe Bonamassa - Just got paid (HD)\", \"description\": \"North Sea Jazz 2009 Live - Joe Bonamassa - Just got paid (HD) Please visit Joe's Official Youtube Channel at http://www.youtube.com/joebonamassaofficial - an...\", \"icon\": \"http://static.ak.fbcdn.net/rsrc.php/v2/yj/r/v2OnaTyTQZE.gif\", \"type\": \"video\", \"application\": { \"name\": \"Share_bookmarklet\", \"id\": \"5085647995\" }, \"created_time\": \"2012-06-11T02:49:17+0000\", \"updated_time\": \"2012-06-11T02:51:05+0000\", \"comments\": { \"data\": [ { \"id\": \"100003921263922_418410954847988_4874404\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"message\": \"properties where are you? http://www.youtube.com/watch?v=uLkEZPOCpsU\", \"created_time\": \"2012-06-11T02:51:05+0000\" } ], \"count\": 1 } }, { \"id\": \"100003921263922_148697441932117\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"message\": \"Mint rulez\", \"picture\": \"http://external.ak.fbcdn.net/safe_image.php?d=AQDKYqIoIbvpfKD5&w=90&h=90&url=http\u00253A\u00252F\u00252Flinuxmint.com\u00252Fstore\u00252FmintBox\u00252Fthumb_4.png\", \"link\": \"http://blog.linuxmint.com/?p=2055\", \"name\": \"The Linux Mint Blog » Blog Archive » Introducing the mintBox\", \"caption\": \"blog.linuxmint.com\", \"description\": \"We’re passionate about what we do and for our very first Mint device, we wanted something unique, something special and extraordinary. The mintBox is Mint in a box. It’s tiny, it’s silent, it’s extremely versatile and it comes packed with connectivity.\", \"icon\": \"http://static.ak.fbcdn.net/rsrc.php/v2/yD/r/aS8ecmYRys0.gif\", \"type\": \"link\", \"application\": { \"name\": \"Share_bookmarklet\", \"id\": \"5085647995\" }, \"created_time\": \"2012-06-09T17:36:52+0000\", \"updated_time\": \"2012-06-09T17:39:58+0000\", \"likes\": { \"data\": [ { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" } ], \"count\": 1 }, \"comments\": { \"data\": [ { \"id\": \"100003921263922_148697441932117_205240\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"message\": \"now I like this 8-}\", \"created_time\": \"2012-06-09T17:39:58+0000\" } ], \"count\": 1 } }, { \"id\": \"100003921263922_114922041981837\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"story\": \"Filip Bulovic likes a link.\", \"story_tags\": { \"0\": [ { \"id\": 100003921263922, \"name\": \"Filip Bulovic\", \"offset\": 0, \"length\": 13, \"type\": \"user\" } ] }, \"picture\": \"http://external.ak.fbcdn.net/safe_image.php?d=AQDDk0tzSxhFNe4Q&w=90&h=90&url=http\u00253A\u00252F\u00252Fcontent6.flixster.com\u00252Fmovie\u00252F31\u00252F17\u00252F311700_pro.jpg\", \"link\": \"http://www.rottentomatoes.com/m/back_to_the_future/\", \"name\": \"Back to the Future\", \"description\": \"Inventive, funny, and breathlessly constructed, is rousing a time-travel adventure with an unforgettable spirit.\", \"icon\": \"http://static.ak.fbcdn.net/rsrc.php/v2/yD/r/aS8ecmYRys0.gif\", \"type\": \"link\", \"created_time\": \"2012-06-09T17:34:29+0000\", \"updated_time\": \"2012-06-09T17:39:09+0000\", \"comments\": { \"data\": [ { \"id\": \"100003921263922_114922041981837_54109\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"message\": \"OpenGraph for Java tutorial sent me there\", \"created_time\": \"2012-06-09T17:39:09+0000\" } ], \"count\": 1 } }, { \"id\": \"100003921263922_114190965388278\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"message\": \"yet another post blah blah\", \"type\": \"status\", \"created_time\": \"2012-06-08T14:06:30+0000\", \"updated_time\": \"2012-06-08T14:06:30+0000\", \"comments\": { \"count\": 0 } }, { \"id\": \"100003921263922_114190335388341\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"message\": \"more posts for more tests\", \"type\": \"status\", \"created_time\": \"2012-06-08T14:05:30+0000\", \"updated_time\": \"2012-06-08T14:05:55+0000\", \"comments\": { \"data\": [ { \"id\": \"100003921263922_114190335388341_50279\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"message\": \"some comment on post\", \"created_time\": \"2012-06-08T14:05:55+0000\" } ], \"count\": 1 } }, { \"id\": \"100003921263922_111427555664619\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"message\": \"OK then let me do Twitter app, nobody influential uses Facebook anyway\", \"type\": \"status\", \"created_time\": \"2012-06-04T21:15:32+0000\", \"updated_time\": \"2012-06-05T17:31:16+0000\", \"comments\": { \"data\": [ { \"id\": \"100003921263922_111427555664619_40315\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"message\": \"comment 1\", \"created_time\": \"2012-06-05T17:31:09+0000\", \"likes\": 1 }, { \"id\": \"100003921263922_111427555664619_40317\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"message\": \"comment 2\", \"created_time\": \"2012-06-05T17:31:16+0000\" } ], \"count\": 2 } }, { \"id\": \"100003921263922_111376109003097\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"message\": \"OK I confirmed twice phone number and stupid facebook still doesn't let me create app key\", \"type\": \"status\", \"created_time\": \"2012-06-04T20:01:31+0000\", \"updated_time\": \"2012-06-04T20:01:31+0000\", \"comments\": { \"count\": 0 } }, { \"id\": \"100003921263922_111241922349849\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"message\": \"where is my app dude?\", \"type\": \"status\", \"created_time\": \"2012-06-04T17:08:08+0000\", \"updated_time\": \"2012-06-04T17:08:08+0000\", \"comments\": { \"count\": 0 } }, { \"id\": \"100003921263922_111236582350383\", \"from\": { \"name\": \"Filip Bulovic\", \"id\": \"100003921263922\" }, \"story\": \"Filip Bulovic and Mirjana Ivkovic are now friends.\", \"story_tags\": { \"18\": [ { \"id\": 1380667962, \"name\": \"Mirjana Ivkovic\", \"offset\": 18, \"length\": 15, \"type\": \"user\" } ], \"0\": [ { \"id\": 100003921263922, \"name\": \"Filip Bulovic\", \"offset\": 0, \"length\": 13, \"type\": \"user\" } ] }, \"type\": \"status\", \"created_time\": \"2012-06-04T17:02:19+0000\", \"updated_time\": \"2012-06-04T17:02:19+0000\", \"comments\": { \"count\": 0 } } ], \"paging\": { \"previous\": \"https://graph.facebook.com/100003921263922/posts?limit=25&since=1339387870\", \"next\": \"https://graph.facebook.com/100003921263922/posts?limit=25&until=1338829338\" }}")


(define-easy-handler (facebook-page :uri "/ems/facebook") ()
  (let ((posts  (make-post-list (rest (first (json:decode-json-from-string *jsstr*)))))
        (page (make-widget 'ems-page :name "facebook-page"))
        (fb-list (make-widget 'facebook-list :name "fb-list")))

    (setf (get-val fb-list 'posts) posts)
    (with-html
      (render page
              :body
              (with-html-to-string ()
                (render fb-list)))
      ))
  
  
  )
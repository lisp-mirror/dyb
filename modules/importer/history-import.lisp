(in-package :dyb)

(defun import-history-csv (file)
  (csv-parser::map-csv-file 
   file
   #'(lambda (line)                                    
       (handler-case
           (when line
              ;;(format t "~A~%" line)
             (cond ((string-equal (nth 1 line) "Facebook")
                    )
                   ((string-equal (nth 1 line) "Twitter")
                    (let* ((link (nth 9 line))
                           (link-split (split-string 
                                        link
                                        #\/))
                           (id (last link-split)))
                      (twitter-get-history-tweet (get-channel-user-by-user-name "camafsa") id)))))
                                    
                                    
                                
         (error (e) (break "? ~A" e)))
                                
       )
                            ))
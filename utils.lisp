(in-package :ems)

(defun flatten (obj)
  (if (listp obj) (mapcan #'flatten obj) (list obj)))

(defun split-string (string char)
    "Returns a list of substrings of string
divided by char."
    (loop for i = 0 then (1+ j)
          as j = (position char string :start i)
          collect (subseq string i j)
          while j))

(defun fmt (control-string &rest args)
  (apply #'format nil control-string args))

(defun fmt-float (n)
  (fmt "~,2f" n))

(defun format-money (value &key (include-comma t))
  (typecase value
    (null "")
    (number
     (multiple-value-bind (quot rem) (truncate value)
       (format nil "~@?~0,2f"
               (if include-comma "~:d" "~d")
               quot rem)))
    (t
     (princ-to-string value))))


(defun format-coordinate (value)
  (format nil "~17$" (if value
                         value
                         0)))

(defun %intern (value)
  (intern (format nil "~:@(~a~)" value)
          #.*package*))

(defun intern-key (value)
  (intern (format nil "~:@(~a~)" value) "KEYWORD"))

(defun blank-p (value)
  (and value
       (not (equal value ""))))

(defun slot-val (instance slot-name)
  (if (and instance
           (slot-boundp instance slot-name))
      (slot-value instance slot-name)))

(defun find-direct-slot-definition (class slot-name)
  (labels ((find-slot (class)
             (or (find slot-name (class-direct-slots class)
                       :key #'slot-definition-name)
                 (some #'find-slot (class-direct-superclasses class)))))
    (find-slot class)))

(defun find-slot-writer (class slot-name)
  (let ((slot (find-direct-slot-definition class slot-name)))
    (and slot
         (car (slot-definition-writers slot)))))

(defun get-slot-setf-method (object slot-name)
  (let ((writer (find-slot-writer (class-of object)
                                  slot-name)))
    (when writer
      (fdefinition writer))))

(defun update-slot (instance slot-name value)
  (let ((method (get-slot-setf-method instance slot-name))
        (val (translate-possible-date-slot slot-name value)))
    (if method
        (funcall method val instance)
        (setf (slot-value instance slot-name) val))))

(defun ensure-parse-integer (x)
  (typecase x
    (string
     (multiple-value-bind (integer position)
         (parse-integer x :junk-allowed t)
       (when (= (length x) position)
         integer)))
    (integer x)))

(defun strip-leading-zeros (string)
  (if (every #'digit-char-p string)
      (if (position #\0 string :test #'char/=)
          (subseq string (position #\0 string :test #'char/=)))
      string))


(defun ffr (field alist-row &optional retain-case)
 (cdr
  (assoc
   (if (not (symbolp field))
       (intern (if retain-case
		   field
		   (string-upcase field)) "KEYWORD")
       field)
   alist-row :test #'string-equal)))

(defun safe-read-from-string (string)
  (let (*read-eval*)
    (ignore-errors (read-from-string string))))

(defun numeric-string-p (string)
  (numberp (safe-read-from-string string)))

(defun invalid-value-p (value)
  (or (equal value :null)
      (string-equal (format nil "~A" nil)
                    (if (stringp value)
                        value
                        (format nil "~A" value)))
      (not (blank-p value))))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

(defun remove-whitespace (string)
  (string-trim '(#\Space #\Newline #\Tab #\Return) string))

(defun tree-assoc (key tree)
  (loop for (x) on tree
        thereis
        (cond ((atom x)
               nil)
              ((equal (car x) key)
               x)
              (t
               (tree-assoc key x))))) 


(defun tree-assoc-all (key tree)
  (let (result)
    (labels ((recur (tree)
               (loop for (x) on tree
                  do
                  (cond ((atom x))
                        ((equal (car x) key)
                         (push x result)
                         (recur x))
                        (t
                         (recur x))))))
      (recur tree)
      (nreverse result))))

(defun assoc-path (tree &rest keys)
  (loop for key in keys
        for value = (assoc key tree :test #'equal)
        then (assoc key (cdr value) :test #'equal)
        while value
        finally (return value)))

(defconstant +unix-to-universal-time+ 2208988800)

(defun get-unix-time (&optional (ut (get-universal-time)))
  (- ut +unix-to-universal-time+))

(defun unix-time-to-universal (unix-time)
  (+ unix-time  +unix-to-universal-time+))

(defvar *short-months*
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defvar *long-months*
  #("January" "February" "March" "April" "May" "June" "July" "August" "September"
    "October" "November" "December"))

 (defun short-month-name (n)
  (when (array-in-bounds-p *short-months* (1- n))
    (aref *short-months* (1- n))))

(defun long-month-name (n)
  (when (array-in-bounds-p *long-months* (1- n))
    (aref *long-months* (1- n))))

(defun month-number (month)
  (let ((position (or (position month *short-months*
                                :test #'equalp)
                      (position month *long-months*
                                :test #'equalp))))
    (when position
      (1+ position))))

(defun format-universal-date (universal-date)  
  (if (stringp universal-date)
      universal-date
      (multiple-value-bind (a b c day month year)
          (decode-universal-time (or universal-date (get-universal-time)))
        (declare (ignore a b c))
        (format-date year month day))))


(defun format-date (year month day)
  (format nil "~d ~a ~d"  day (short-month-name month) year))

(defun format-date-time (year month day hour min sec
                        &optional timezone)
  (declare (ignore timezone))
  (format nil "~d ~a ~d ~@{~2,'0d~^:~}"
          day (short-month-name month) year hour min sec))


(defun format-universal-date-time (universal-date)  
  (if (stringp universal-date)
      universal-date
      (multiple-value-bind (sec min hour day month year)
          (decode-universal-time (or universal-date (get-universal-time)))
        (format-date-time year month day hour min sec))))



(defun string-to-date (date-string)
  (if (stringp date-string)
      (let ((split-date (split-string date-string #\space)))
        (encode-universal-time  
         0 0 0 
         (parse-integer (first split-date)) 
         (or (parse-integer (second split-date))
             (+ 1 (or 
                   (position (second split-date) *short-months* :test 'string-equal)
                   (position (second split-date) *long-months* :test 'string-equal)))) 
         (if (> (parse-integer (third split-date)) 1900) 
             (parse-integer (third split-date))
             1901)
         ))
      date-string))


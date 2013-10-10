(in-package :dyb)

(defun not-empty-p (value)
  (not (empty-p value)))

(setf drakma:*drakma-default-external-format* :utf-8)

(defun alist-to-url-encoded-string (alist external-format)
  (with-output-to-string (out)
    (loop for first = t then nil
          for (name . value) in alist
          unless first do (write-char #\& out)
          do (format out "~A~:[~;=~A~]"
                     (url-encode name external-format)
                     value
                     (url-encode value external-format)))))

(defvar *public-dir* "/var/www/dyb.co.za/public/")

(defparameter *extract-dir* "/var/www/dyb.co.za/extracts")

(defvar *version* "0.1.00.05")

(defun context-entities ()
  (if (stringp (session-value 'context-entities))
      (safe-read-from-string (session-value 'context-entities))
      (session-value 'context-entities)))

(defun context-reporting-period ()
  (session-value 'context-reporting-period))

(defun context-root-entity ()
  (session-value 'context-root-entity))

(defun field-permission (data-element)
  (declare (ignore data-element))
  t)


(defun ensure-render (ensure body)
  (and ensure
       body))

(defun render-edit-field (name value
                          &key data-element required
                          (type "text")
                          data (blank-allowed t)
                          min
                          max
                          width)

  (ensure-render
   (field-permission (or data-element name))
   (with-html
     (case type
       (:span
        (htm (:input
              :style (if width
                         (format nil "width:~A;" width)
                         (format nil "width:~A;" "300px"))
              :type "text"
                     :name name
                     :disabled t
                     :readonly t
                     :value (escape value))))
       (:select
        (render-select (or data-element name) data value
                       :blank-allowed blank-allowed))
       (:textarea
        (htm (:textarea
              :style (if width
                         (format nil "width:~A;" width)
                         (format nil "width:~A;" "300px"))
              :class (if required "required")
              :name name
              :cols 85 :rows 5
                        (str (escape value)))))
       (:password
        (htm (:input
              :style (if width
                         (format nil "width:~A;" width)
                         (format nil "width:~A;" "300px"))
              :type "password"
              :class (if required "required")
              :name name
              :value (escape value))))
       (:date
        (htm (:input :type "text"
                     :style (if width
                                (format nil "width:~A;" width)
                                (format nil "width:~A;" "300px"))
                     :class "datepicker"
                     :name name
                     :value (escape value)))
        (defer-js "$('.datepicker').datepicker({dateFormat: 'dd M yy'})"))
       (:time
        (htm (:input :type "text"
                     :id "time-entry"
                     ;; :style (if width
                     ;;            (format nil "width:~A;" width)
                     ;;            (format nil "width:~A;" "100px"))
                     :class (if required "required")
                     :name name
                     ;;  :pattern  "(\d{2}([\\:]\d{2})?)"
                     :value (escape value)
                     (if required "required")))
        (defer-js
            "$('#time-entry').timeEntry({spinnerImage: '/js/timeentry/spinnerDefault.png'})"))
       (t
        (htm
         (:div (:input :type type
                       :style (if width
                                  (format nil "width:~A;" width)
                                  (format nil "width:~A;" "300px"))
                       :class (if required "required")
                       :name name
                       :min min
                       :max max
                       :value (escape value)))
         (:div :style "display:none;"
               :name (format nil "validate-~A" name)
               :id (format nil "validate-~A" name)
               (:img :src "/appimg/q-icon.png"))))))))

(defun year (date)
  (values (decode-date date)))

(defun month (date)
  (nth-value 1 (decode-date date)))

(defun day (date)
  (nth-value 2 (decode-date date)))

(defun string-to-date (date-string &key (date-spacer #\space) reverse-date-sequence-p)
  (if (stringp date-string)
      (let ((split-date (split-string date-string date-spacer)))
        (if reverse-date-sequence-p
            (setf split-date (reverse split-date)))
        (encode-universal-time
         0 0 0
         (parse-integer (first split-date))
         (or (parse-integer (second split-date) :junk-allowed t)
             (+ 1 (or
                   (position (second split-date) *short-months* :test 'string-equal)
                   (position (second split-date) *long-months* :test 'string-equal))))
         (if (> (parse-integer (third split-date)) 1900)
             (parse-integer (third split-date))
             1901)
         (time-zone)))
      date-string))


(defun universal-to-gmt-0 (time)
  (- time
     (* (- (time-zone))
        (* 60 60))))

(defun format-date-dash (year month day)
  (format nil "~d-~d-~d"  year month day))

(defun format-universal-date-dash (universal-date)
  (if (stringp universal-date)
      universal-date
      (multiple-value-call #'format-date-dash (decode-date universal-date))))

(defun format-date (year month day)
  (format nil "~d ~a ~d"  day (short-month-name month) year))

(defun format-date-time (year month day hour min sec
                        &optional timezone)
  (declare (ignore timezone))
  (format nil "~d ~a ~d ~@{~2,'0d~^:~}"
          day (short-month-name month) year hour min sec))

(defun format-universal-date (universal-date)
  (when universal-date
      (if (stringp universal-date)
          universal-date
          (multiple-value-call #'format-date (decode-date universal-date)))))

(defun format-universal-date-time (universal-date)
  (if (stringp universal-date)
        universal-date
        (multiple-value-bind (sec min hour day month year)
            (decode-universal-time
             (or universal-date (get-universal-time))
             (time-zone))
          (format-date-time year month day hour min sec))))


(defun format-universal-time (universal-date)
  (if (stringp universal-date)
      universal-date
      (multiple-value-bind (sec min hour)
          (decode-universal-time
           (or universal-date (get-universal-time))
           (time-zone))
        (declare (ignore sec))
        (format nil "~2,'0d:~2,'0d" hour min))))

(defun n-days-from-now (n)
  (format-universal-date (+ (* 86400 n) (get-universal-time))))

(defparameter *time-zone* -2)

(defun time-zone ()
  (or (and (current-user)
           (preferences (current-user))
           (gethash :time-zone (preferences (current-user))))
      *time-zone*))

(defun period-date-to-universal (date-string)
  (if date-string
      (let ((split-date (split-string date-string #\-)))
        (encode-universal-time
         0 0 0
         (parse-integer (third split-date))
         (parse-integer (second split-date))
         (if (> (parse-integer (first split-date)) 1900)
             (parse-integer (first split-date))
             1901)
         (time-zone)))
      date-string))

(defun current-date-time ()
  (multiple-value-call #'format-date-time (date-calc:today-and-now)))

(defun current-date ()
  (multiple-value-call #'format-date (date-calc:today)))

(defun parse-trim-integer (string)
  (parse-integer (string-trim '(#\Space #\Tab #\Newline) string) :junk-allowed t))

(defun render-select (name items value &key first-value blank-allowed)
  (let ((select (make-widget 'select
                             :name name)))
    (setf (blank-allowed select) blank-allowed)
    (setf (first-item select) first-value)
    (setf (items select) items)
    (when value
     (setf (value select) value))
    (render select)
    select))

(defun build-validation-array (validation-list)
  (if validation-list
      (json:encode-json-to-string
       (loop for (element type required) in validation-list
          collect `((element . ,element)
                    (type . ,type)
                    (required . ,(equalp required
                                         "required")))))))

#|(defun partition (predicate list)
  (loop for i in list
        when (funcall predicate i)
        collect i into part1
        else collect i into part2
        finally (return (values part1 part2))))
|#

(defun gps-cord-formatter (value)
  (format nil "~17$" value))

(defun decode-date-string (date)
  (ppcre:register-groups-bind
      (day month year) ("(\\d+)\\s+(\\w+)\\s+(\\d+)" date)
    (when day
      (values (parse-integer year)
              (month-number month)
              (parse-integer day)))))

(defun decode-time-string (time)
  (ppcre:register-groups-bind
      ((#'parse-integer hour min) am/pm)
      ("(\\d{1,2}):(\\d{1,2})(PM|AM)" time)
    (let ((am (equalp am/pm "AM")))
      (values (cond ((and am (= 12 hour))
                     0)
                    ((or am (= 12 hour))
                     hour)
                    (t
                     (+ hour 12)))
              min 0))))

(defun decode-time-string-check (time)
  (multiple-value-bind (hour minute second)
        (decode-time-string time)
    (cond ((> hour 24)
           (values nil "Hour can not be more than 24."))
          ((or (> minute 60) (> second 60))
           (values nil "Minutes and seconds can not be more than 60."))
          ((or  (< second 0) (< minute 0) (< hour 0))
           (values nil "No time value can be less than 0."))
          ((and (= hour 24) (or (> minute 0) (> second 0)))
           (values nil "Time can not be more than 24 hours."))
          (t (values hour minute second)))))

(defun decode-date (date)
  (etypecase date
    (simple-date:date
     (simple-date:decode-date date))
    (string
     (decode-date-string date))
    (integer
     (multiple-value-bind (a b c day month year)
         (decode-universal-time date (time-zone))
       (declare (ignore a b c))
       (values year month day)))))

(defun make-icon (name &key (size 16)
                            (title ""))
  (let ((size (or size 16)))
    (with-html
      (:img :src (format nil "/img/icons/packs/fugue/~ax~a/~a.png"
                         size size name)
            :alt title
            :title title
            :width size :height size))))

(defun box-header (text &key icon (icon-size 16)
                             (class "header")
                             collapsible
                             body)
  (with-html
    (:div :class class
          (when icon
            (make-icon icon
                       :size icon-size))
          (:h3 (esc text))
          (when collapsible
            (htm (:div :class "collapse")))
          (str body))))

(defun print-entity-name (doc)
  (if (typep doc 'entity)
      (entity-name doc)
      (entity-name (entity doc))))

(defun context ()
  (and (current-user)
       (multiple-value-bind (context found)
           (session-value 'context)
         (if found
             context
             (setf (context) (last-context (current-user)))))))

(defun (setf context) (entities)
  (and (current-user)
       (setf (session-value 'context) entities)))

(defun valid-channel-user (user channel)
  (when (match-context-entities user)
    (when (and user (string-equal (get-val user 'doc-status) "Active"))
      (if (string-equal (get-val user 'channel-user-type) channel)
        user
        ))))


(defun http-call (url method &key content content-type parameters headers return-type)
  (when url
    (let ((result))
      (multiple-value-bind (body status)
          (drakma:http-request
           url
           :parameters parameters
           :content content
           :method (or method :get)
           :content-type content-type
           :additional-headers headers
           :preserve-uri t)
        (let ((decoded-body))

          (if  (stringp body)
               (setf decoded-body body)
               (setf decoded-body (babel:octets-to-string body)))

          (cond ((string-equal return-type "Query String")
                 (setf result (parse-query-string body)))
                ((string-equal return-type "JSON")
                 (setf result
                       (json::decode-json-from-string decoded-body)))
                ((string-equal return-type "XML")
                 ;;TODO: Implement xml parsing
                 (setf result decoded-body))
                (t
                 (setf result decoded-body)))

          (cond ((equal status 200)
                 (values result status nil))
                (t
                 (if (consp (caar result))
                     (setf result (car result)))

                 (if (or (assoc-path (first (cdr (assoc-path result :errors)))
                                     :message)
                         (assoc-path result :error :message))
                     (values nil
                             status
                             (cdr
                              (or
                               (assoc-path
                                (first (cdr (assoc-path result :errors))) :message)
                               (assoc-path result :error :message))))
                     (values result status nil)))))))))
;;;

(defvar *allowed-html-tags*
  '("a"))

(defvar *current-html-tags* ())

(defclass filter-sink (chtml::sink)
  ())

(defun make-filter-sink ()
  (make-instance 'filter-sink
                 :encoding "UTF-8"
                 :ystream
                 (runes:make-rod-ystream
                  :encoding (runes:find-output-encoding "UTF-8"))))

(defun find-pt-child (name pt)
  (find name (chtml:pt-children pt)
        :key #'chtml:pt-name))

(defun parse-html (string)
  (find-pt-child :body
                 (chtml:parse (coerce string 'runes:rod) nil)))

(defmethod hax:start-element ((sink filter-sink) name attributes)
  (when (find name *allowed-html-tags*
              :test #'equalp)
    (push name *current-html-tags*)
    (call-next-method)))

(defmethod hax:end-element ((sink filter-sink) name)
  (when (find name *allowed-html-tags*
              :test #'equalp)
    (pop *current-html-tags*)
    (call-next-method)))

(defmethod hax:characters ((sink filter-sink) data)
  (if (equal (car *current-html-tags*) "a")
      (call-next-method)
      (chtml::sink-write-rod (%linkify-string data) sink)))

(defun %linkify-string (text)
  (ppcre:regex-replace-all "((http://|www.)\\S+)" text "<a target=\"_blank\" href='\\1'>\\1</a>"))

(defun linkify (string)
  (chtml:serialize-pt
   (parse-html string)
   (make-filter-sink)))

(defun send-system-mail (subject message)
  (cl-smtp:send-email "mail.digyourbrand.com"
                      "system@digyourbrand.com"
                      "system@digyourbrand.com"
                      subject
                      message
                      :ssl :tls
                      :authentication
                      '(:login "system@digyourbrand.com"
                        "m3t$y$dyb")))

(defparameter *time-zones*
  '(("-12.0" . "(GMT -12:00) Eniwetok, Kwajalein")
    ("-11.0" . "(GMT -11:00) Midway Island, Samoa")
    ("-10.0" . "(GMT -10:00) Hawaii")
    ("-9.0" . "(GMT -9:00) Alaska")
    ("-8.0" . "(GMT -8:00) Pacific Time (US & Canada)")
    ("-7.0" . "(GMT -7:00) Mountain Time (US & Canada)")
    ("-6.0" . "(GMT -6:00) Central Time (US & Canada), Mexico City")
    ("-5.0" . "(GMT -5:00) Eastern Time (US & Canada), Bogota, Lima")
    ("-4.0" . "(GMT -4:00) Atlantic Time (Canada), Caracas, La Paz")
    ("-3.5" . "(GMT -3:30) Newfoundland")
    ("-3.0" . "(GMT -3:00) Brazil, Buenos Aires, Georgetown")
    ("-2.0" . "(GMT -2:00) Mid-Atlantic")
    ("-1.0" . "(GMT -1:00 hour) Azores, Cape Verde Islands")
    ("0.0" . "(GMT) Western Europe Time, London, Lisbon, Casablanca")
    ("1.0" . "(GMT +1:00 hour) Brussels, Copenhagen, Madrid, Paris")
    ("2.0" . "(GMT +2:00) Kaliningrad, South Africa")
    ("3.0" . "(GMT +3:00) Baghdad, Riyadh, Moscow, St. Petersburg")
    ("3.5" . "(GMT +3:30) Tehran")
    ("4.0" . "(GMT +4:00) Abu Dhabi, Muscat, Baku, Tbilisi")
    ("4.5" . "(GMT +4:30) Kabul")
    ("5.0" . "(GMT +5:00) Ekaterinburg, Islamabad, Karachi, Tashkent")
    ("5.5" . "(GMT +5:30) Bombay, Calcutta, Madras, New Delhi")
    ("5.75" . "(GMT +5:45) Kathmandu")
    ("6.0" . "(GMT +6:00) Almaty, Dhaka, Colombo")
    ("7.0" . "(GMT +7:00) Bangkok, Hanoi, Jakarta")
    ("8.0" . "(GMT +8:00) Beijing, Perth, Singapore, Hong Kong")
    ("9.0" . "(GMT +9:00) Tokyo, Seoul, Osaka, Sapporo, Yakutsk")
    ("9.5" . "(GMT +9:30) Adelaide, Darwin")
    ("10.0" . "(GMT +10:00) Eastern Australia, Guam, Vladivostok")
    ("11.0" . "(GMT +11:00) Magadan, Solomon Islands, New Caledonia")
    ("12.0" . "(GMT +12:00) Auckland, Wellington, Fiji, Kamchatka")))

(defparameter *countries*
  '(("Afghanistan") 
    ("Albania") 
    ("Algeria") 
    ("American Samoa") 
    ("Andorra") 
    ("Angola") 
    ("Anguilla") 
    ("Antarctica") 
    ("Antigua and Barbuda") 
    ("Argentina") 
    ("Armenia") 
    ("Aruba") 
    ("Australia") 
    ("Austria") 
    ("Azerbaijan") 
    ("Bahamas") 
    ("Bahrain") 
    ("Bangladesh") 
    ("Barbados") 
    ("Belarus") 
    ("Belgium") 
    ("Belize") 
    ("Benin") 
    ("Bermuda") 
    ("Bhutan") 
    ("Bolivia") 
    ("Bosnia and Herzegovina") 
    ("Botswana") 
    ("Bouvet Island") 
    ("Brazil") 
    ("British Indian Ocean Territory") 
    ("Brunei Darussalam") 
    ("Bulgaria") 
    ("Burkina Faso") 
    ("Burundi") 
    ("Cambodia") 
    ("Cameroon") 
    ("Canada") 
    ("Cape Verde") 
    ("Cayman Islands") 
    ("Central African Republic") 
    ("Chad") 
    ("Chile") 
    ("China") 
    ("Christmas Island") 
    ("Cocos (Keeling) Islands") 
    ("Colombia") 
    ("Comoros") 
    ("Congo") 
    ("Congo, The Democratic Republic of The") 
    ("Cook Islands") 
    ("Costa Rica") 
    ("Cote D'ivoire") 
    ("Croatia") 
    ("Cuba") 
    ("Cyprus") 
    ("Czech Republic") 
    ("Denmark") 
    ("Djibouti") 
    ("Dominica") 
    ("Dominican Republic") 
    ("Ecuador") 
    ("Egypt") 
    ("El Salvador") 
    ("Equatorial Guinea") 
    ("Eritrea") 
    ("Estonia") 
    ("Ethiopia") 
    ("Falkland Islands (Malvinas)") 
    ("Faroe Islands") 
    ("Fiji") 
    ("Finland") 
    ("France") 
    ("French Guiana") 
    ("French Polynesia") 
    ("French Southern Territories") 
    ("Gabon") 
    ("Gambia") 
    ("Georgia") 
    ("Germany") 
    ("Ghana") 
    ("Gibraltar") 
    ("Greece") 
    ("Greenland") 
    ("Grenada") 
    ("Guadeloupe") 
    ("Guam") 
    ("Guatemala") 
    ("Guinea") 
    ("Guinea-bissau") 
    ("Guyana") 
    ("Haiti") 
    ("Heard Island and Mcdonald Islands") 
    ("Holy See (Vatican City State)") 
    ("Honduras") 
    ("Hong Kong") 
    ("Hungary") 
    ("Iceland") 
    ("India") 
    ("Indonesia") 
    ("Iran, Islamic Republic of") 
    ("Iraq") 
    ("Ireland") 
    ("Israel") 
    ("Italy") 
    ("Jamaica") 
    ("Japan") 
    ("Jordan") 
    ("Kazakhstan") 
    ("Kenya") 
    ("Kiribati") 
    ("Korea, Democratic People's Republic of") 
    ("Korea, Republic of") 
    ("Kuwait") 
    ("Kyrgyzstan") 
    ("Lao People's Democratic Republic") 
    ("Latvia") 
    ("Lebanon") 
    ("Lesotho") 
    ("Liberia") 
    ("Libyan Arab Jamahiriya") 
    ("Liechtenstein") 
    ("Lithuania") 
    ("Luxembourg") 
    ("Macao") 
    ("Macedonia, The Former Yugoslav Republic of") 
    ("Madagascar") 
    ("Malawi") 
    ("Malaysia") 
    ("Maldives") 
    ("Mali") 
    ("Malta") 
    ("Marshall Islands") 
    ("Martinique") 
    ("Mauritania") 
    ("Mauritius") 
    ("Mayotte") 
    ("Mexico") 
    ("Micronesia, Federated States of") 
    ("Moldova, Republic of") 
    ("Monaco") 
    ("Mongolia") 
    ("Montserrat") 
    ("Morocco") 
    ("Mozambique") 
    ("Myanmar") 
    ("Namibia") 
    ("Nauru") 
    ("Nepal") 
    ("Netherlands") 
    ("Netherlands Antilles") 
    ("New Caledonia") 
    ("New Zealand") 
    ("Nicaragua") 
    ("Niger") 
    ("Nigeria") 
    ("Niue") 
    ("Norfolk Island") 
    ("Northern Mariana Islands") 
    ("Norway") 
    ("Oman") 
    ("Pakistan") 
    ("Palau") 
    ("Palestinian Territory, Occupied") 
    ("Panama") 
    ("Papua New Guinea") 
    ("Paraguay") 
    ("Peru") 
    ("Philippines") 
    ("Pitcairn") 
    ("Poland") 
    ("Portugal") 
    ("Puerto Rico") 
    ("Qatar") 
    ("Reunion") 
    ("Romania") 
    ("Russian Federation") 
    ("Rwanda") 
    ("Saint Helena") 
    ("Saint Kitts and Nevis") 
    ("Saint Lucia") 
    ("Saint Pierre and Miquelon") 
    ("Saint Vincent and The Grenadines") 
    ("Samoa") 
    ("San Marino") 
    ("Sao Tome and Principe") 
    ("Saudi Arabia") 
    ("Senegal") 
    ("Serbia and Montenegro") 
    ("Seychelles") 
    ("Sierra Leone") 
    ("Singapore") 
    ("Slovakia") 
    ("Slovenia") 
    ("Solomon Islands") 
    ("Somalia") 
    ("South Africa") 
    ("South Georgia and The South Sandwich Islands") 
    ("Spain") 
    ("Sri Lanka") 
    ("Sudan") 
    ("Suriname") 
    ("Svalbard and Jan Mayen") 
    ("Swaziland") 
    ("Sweden") 
    ("Switzerland") 
    ("Syrian Arab Republic") 
    ("Taiwan, Province of China") 
    ("Tajikistan") 
    ("Tanzania, United Republic of") 
    ("Thailand") 
    ("Timor-leste") 
    ("Togo") 
    ("Tokelau") 
    ("Tonga") 
    ("Trinidad and Tobago") 
    ("Tunisia") 
    ("Turkey") 
    ("Turkmenistan") 
    ("Turks and Caicos Islands") 
    ("Tuvalu") 
    ("Uganda") 
    ("Ukraine") 
    ("United Arab Emirates") 
    ("United Kingdom") 
    ("United States") 
    ("United States Minor Outlying Islands") 
    ("Uruguay") 
    ("Uzbekistan") 
    ("Vanuatu") 
    ("Venezuela") 
    ("Viet Nam") 
    ("Virgin Islands, British") 
    ("Virgin Islands, U.S.") 
    ("Wallis and Futuna") 
    ("Western Sahara") 
    ("Yemen") 
    ("Zambia") 
    ("Zimbabwe")))

(defparameter *industries*
  '(("Agriculture")
    ("Manfacturing")
    ("Accounting")
    ("Advertising")
    ("Banking")
    ("Entertainment")
    ("Health Care")
    ("Information Technology")
    ("Internet")
    ("Legal")
    ("Music")
    ("Real Estate")
    ("Retail")
    ("Telecommunications")
    ("Tourism")
    ("Other")))

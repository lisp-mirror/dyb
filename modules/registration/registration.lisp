(in-package :dyb)

(defclass registration (widget)
  ((current-widget :initarg :current-widget
                   :initform nil
                   :accessor current-widget)
   (previous-widgets :initarg :previous-widgets
                     :initform nil
                     :accessor previous-widgets))
  (:metaclass widget-class)
  (:include-css "/appcss/reg.css"))

(defmethod render ((widget registration) &key)
  (render (current-widget widget)))

(defun advance-widget (registration next-widget)
  (push (current-widget registration) (previous-widgets registration))
  (setf (current-widget registration) next-widget))

(defun go-back (registration)
  (setf (current-widget registration) (pop (previous-widgets registration))))

(defclass wizard-widget (widget)
  ((parent :initarg :parent
           :initform nil
           :accessor parent)
   (form :initarg :form
         :initform (make-instance 'reg-form)
         :accessor form))
  (:metaclass widget-class))

(defclass cancel (wizard-widget)
  ()
  (:metaclass widget-class))

(defmethod render ((widget cancel) &key)
  (with-html
    (:div :class "nonboxy-widget"
          (:div :class "widget-content"
                (:form :name "reg-form"
                       :class "form-horizontal well"
                       :action ""
                       :method "post"
                       (:img :src "/img/q-mark.png")
                       (:h3 "Are you sure you wish to cancel?")
                       (:h3 "Should you wish to return to Account Setup please click <strong>Go Back</strong> below.")
                       (:div :class "form-actions"
                             (:button
                              :class "btn btn-info"
                              :type "submit"
                              :name "go-back"
                              "Go back")
                             (:button :class "btn btn-warning"
                                      :type "submit"
                                      :name "exit"
                                      "Exit")))))))

(defmethod action-handler ((widget cancel))
  (cond ((post-parameter "go-back")
         (go-back (parent widget)))
        ((post-parameter "exit")
         (let ((parent (parent widget)))
           (setf (current-widget parent) nil)
           (loop for previous in (previous-widgets parent)
                 do
                 (setf (state (form previous)) nil))
           (setf (previous-widgets parent) nil)
           (redirect "/")))))
;;;

(defclass congratulations (wizard-widget)
  ()
  (:metaclass widget-class))

(defmethod render ((widget congratulations) &key)
  (with-html
    (:div :class "nonboxy-widget"
          (:div :class "widget-content"
                (:div :class "well"
                      (:img :src "/img/check-mark.png")
                      (:h3 "Congratulations!")
                      "You have successfully completed your DigYourBrand 30-Day FREE Trial registration."
                      (:p
                       "What would you like to do now?")
                      (:p
                       (:a :href "/dyb/profile"
                           "View my DigYourBrand Account Profile."))
                      (:p
                       (:a :href "/dyb/dashboard"
                           "Visit my DigYourBrand Dashboard.")))))))

;;;

(defclass create-account (wizard-widget)
  ()
  (:metaclass widget-class))

(defmethod render ((widget create-account) &key)
  (with-html
    (:div :class "nonboxy-widget"
          (:div :class "widget-head"
                (:span :style "float:left;"
                       (:img :src "/img/create-account.png"))
                (:h3 :class "title-header"
                     "Create an account"))
          (:div :class "widget-content"
                (:form :name "reg-form"
                       :class "form-horizontal well"
                       :action ""
                       :method "post"
                       (setf (fields (form widget))
                             `(("First Name*" :name "first-name" :type :text)
                               ("Last Name*" :name "last-name" :type :text)
                               ("Email*" :name "email" :type :text)
                               ("Password*" :name "password" :type :password)
                               ("Confirm Password*" :name "confirm-password" :type :password)
                               ("Ts & Cs*"
                                :name "terms"
                                :type :checkbox
                                :description
                                "I accept the <a href=\"#\">Terms & Conditions</a> and <a href=\"#\">Privacy Policy</a>
as stipulated on the DigYourBrand.com website.")
                               ("Sign me up*"
                                :name "trial"
                                :type :radio
                                :description
                                (("Annual subscription ($600 <em>*15% discount incl.</em>)"
                                  :value "1"
                                  :default t)
                                 ("Monthly subscription ($59)"
                                  :value "2")
                                 ("Activate my FREE Trial"
                                  :value "3")))))
                       (render (form widget))
                       (:div :class "cal-note"
                             :style (unless (equal (post-parameter "trial") "3")
                                      "display:none;")
                             (:span :class "dashboard-icons-colors calendar_sl")
                             (:p :style "padding-top:10px;"
                                 "Your 30-day Free Trial lasts until midnight"
                                 (:strong (esc (n-days-from-now 30)))))
                       (:div :class "form-actions"
                             (:button
                              :class "btn btn-info"
                              :type "submit"
                              :name "submit"
                              "Confirm")
                             (:button :class "btn btn-warning"
                                      :name "cancel"
                                      "Cancel")))))
    (defer-js "$('[name=trial]').change(function(){if($(this).val() == '3'){$('.cal-note').show();}else{$('.cal-note').hide();}})")
    (defer-js "$('[name=first-name]').focus()")))

(defmethod action-handler ((widget create-account))
  (with-slots (form parent) widget
    (setf (error-messages form) nil)
    (with-parameters (submit cancel password
                             confirm-password)
      (when (or submit cancel)
        (form-fill-state form))
      (cond (submit
             (let ((valid (validate-reg-form (form widget))))
               (cond ((and (not (empty-p password))
                           (not (empty-p confirm-password))
                           (not (equal password confirm-password)))
                      (set-field-error form "confirm-password" "Passwords don't match"))
                     (valid
                      (advance-widget parent
                                      (make-widget 'account-setup :parent parent))))))
            (cancel
             (advance-widget parent
                             (make-widget 'cancel :parent parent)))))))
;;;

(defclass account-setup (wizard-widget)
  ()
  (:metaclass widget-class))

(defmethod render ((widget account-setup) &key)
  (with-html
    (:div :class "nonboxy-widget"
          (:div :class "widget-head"
                (:span :style "float:left;"
                       (:img :src "/img/setup.png"))
                (:h3 :class "title-header"
                     "Account setup"))
          (:div :class "widget-content"
                (:form :name "reg-form"
                       :class "form-horizontal well"
                       :action ""
                       :method "post"
                       (setf (fields (form widget))
                             `(("Timezone*" :name "time-zone"
                                            :type :select
                                            :description ,*time-zones*)))
                       (render (form widget))
                       (:div :class "social"
                        (:h4  "Add Your Social Accounts")
                        (:div :class "account-buttons"
                              (:a :href "#"
                                  (:img :class "add-acc-btn"
                                        :src "/img/fb.jpg"))
                              (:a :href "#"
                                  (:img :class "add-acc-btn"
                                        :src "/img/tw.jpg"))
                              (:a :href "#"
                                  (:img :class "add-acc-disabled"
                                        :src "/img/li.jpg"))))
                       (:div :class "form-actions"
                             (:button
                              :class "btn btn-info"
                              :type "submit"
                              :name "submit"
                              "Confirm")
                             (:button :class "btn btn-warning"
                                      :name "cancel"
                                      "Cancel")))))))

(defmethod action-handler ((widget account-setup))
  (with-slots (form parent) widget
    (setf (error-messages form) nil)
    (with-parameters (submit cancel)
      (when (or submit cancel)
        (form-fill-state form))
      (cond (submit
             (let ((valid (validate-reg-form (form widget))))
               (cond ((not valid))
                     ((equal (form-field-value
                              (form (car (previous-widgets parent)))
                              "trial")
                             "3")
                      (advance-widget
                       parent
                       (make-widget 'congratulations :parent parent)))
                     (t
                      (advance-widget
                       parent
                       (make-widget 'billing-details :parent parent))))))
            (cancel
             (advance-widget parent
                             (make-widget 'cancel :parent parent)))))))
;;;

(defclass billing-details (wizard-widget)
  ()
  (:metaclass widget-class))

(defmethod render ((widget billing-details) &key)
  (with-html
    (:div :class "nonboxy-widget"
          (:div :class "widget-head"
                (:span :style "float:left;"
                       (:img :src "/img/setup.png"))
                (:h3 :class "title-header"
                     "Billing details"))
          (:div :class "widget-content"

                (:form :name "reg-form"
                       :class "form-horizontal well"
                       :action ""
                       :method "post"
                       (setf (fields (form widget))
                             `(("Company Name*" :name "company-name" :type :text)
                               ("Are you a South African company?*"
                                :name "south-african"
                                :type :radio
                                :description
                                (("Yes"
                                  :value "yes"
                                  :default t)
                                 ("No"
                                  :value "no")))
                               ("Country*" :name "country"
                                           :type :select
                                           :description ,*countries*)
                               ("Billing address*" :name "billing-address" :type :textarea)
                               ("VAT No" :name "vat" :type :text
                                         :required nil)
                               ("Company Reg No" :name "reg-no" :type :text :required nil)
                               ("Industry*" :name "industry"
                                            :type :select
                                            :description ,*industries*)))
                       (render (form widget))
                       (:div :class "form-actions"
                             (:button
                              :class "btn btn-info"
                              :type "submit"
                              :name "submit"
                              "Confirm")
                             (:button :class "btn btn-warning"
                                      :name "cancel"
                                      "Cancel")))))))

(defmethod action-handler ((widget billing-details))
  (break)
  (with-slots (form parent) widget
    (setf (error-messages form) nil)
    (with-parameters (submit cancel)
      (when (or submit cancel)
        (form-fill-state form))
      (cond (submit
             (let ((valid (validate-reg-form (form widget))))
               (cond (valid
                      (advance-widget parent
                       (make-widget 'congratulations :parent parent))))))
            (cancel
             (advance-widget parent
                             (make-widget 'cancel :parent parent)))))))

;;;

(define-easy-handler (reg-page :uri "/dyb/registration"
                               :for-everyone t) ()
  (let ((reg-widget (make-widget 'registration)))
    (unless (current-widget reg-widget)
      (setf (current-widget reg-widget)
            (make-widget 'create-account :parent reg-widget)))
    (render (make-widget 'special-html-framework-page :header "Register")
            :body (render-to-string reg-widget))))

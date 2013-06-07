(in-package :dyb)

(defvar *string-substitution*
  '((#\FIGURE_DASH "-")
    (#\EN_DASH "-")
    (#\EM_DASH "-")
    (#\HYPHEN "-")
    (#\MACRON "-")
    (#\DOUBLE_LOW_LINE "_")
    (#\COMBINING_GRAVE_ACCENT "'")
    (#\COMBINING_ACUTE_ACCENT "'")
    (#\RIGHT_SINGLE_QUOTATION_MARK "'")
    (#\LEFT_SINGLE_QUOTATION_MARK "'")
    (#\GREEK_TONOS "'")
    (#\SINGLE_HIGH-REVERSED-9_QUOTATION_MARK "'")
    (#\SINGLE_LOW-9_QUOTATION_MARK "'")
    (#\DOUBLE_ACUTE_ACCENT "\"")
    (#\LEFT_DOUBLE_QUOTATION_MARK "\"")
    (#\RIGHT_DOUBLE_QUOTATION_MARK "\"")
    (#\DOUBLE_LOW-9_QUOTATION_MARK "\"")
    (#\… "...")))

(defun sanitize-string (string)
  (let ((string (string-trim '(#\space #\tab #\newline #\linefeed #\return)
                             string)))
    (if #+sbcl(typep string 'simple-base-string)
        #-sbcl nil
        string
        (with-output-to-string (str)
          (loop for char across string
                for subst = (cadr (assoc char *string-substitution* :test #'char=))
                do (if subst
                       (write-string subst str)
                       (write-char char str)))))))

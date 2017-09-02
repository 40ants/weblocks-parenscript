(in-package :cl-user)
(defpackage weblocks.parenscript
  (:use :cl)
  (:export
   #:make-dependency))
(in-package :weblocks.parenscript)


(defclass parenscript-dependency (weblocks.dependencies:local-dependency)
  ((js :type string
       :initarg :js
       :reader get-js)))


(defmethod weblocks.dependencies:get-url ((dependency parenscript-dependency))
  "Returns a hash-like url for this dependency because it does not have
a real filename."
  (values (format nil "/static/js/~a.js"
                  (weblocks::md5 (get-js dependency)))
          :local))


(defmethod weblocks.dependencies:serve ((dependency parenscript-dependency))
  "Serves static dependency from the disk."
  (values (get-js dependency)
          (weblocks.dependencies:get-content-type dependency)))


(defmacro make-dependency (&body parenscript-code)
  `(make-instance 'parenscript-dependency
                  :type :js
                  :js (ps:ps ,@parenscript-code)))

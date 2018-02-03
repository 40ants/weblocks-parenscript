(in-package :cl-user)
(defpackage weblocks-parenscript
  (:use :cl)
  (:import-from #:weblocks/dependencies
                #:local-dependency
                #:get-url
                #:get-content-type
                #:serve)
  (:import-from #:weblocks/utils/misc
                #:md5)
  (:import-from #:parenscript
                #:ps
                #:ps*)

  (:export
   #:make-dependency
   #:make-dependency*))
(in-package weblocks-parenscript)


(defclass parenscript-dependency (local-dependency)
  ((js :type string
       :initarg :js
       :reader get-js)))


(defmethod get-url ((dependency parenscript-dependency))
  "Returns a hash-like url for this dependency because it does not have
a real filename."
  (values (format nil "/static/js/~a.js"
                  (md5 (get-js dependency)))
          :local))


(defmethod serve ((dependency parenscript-dependency))
  "Serves static dependency from the disk."
  (values (get-js dependency)
          (get-content-type dependency)))


(defmacro make-dependency (&body parenscript-code)
  `(make-instance 'parenscript-dependency
                  :type :js
                  :js (ps ,@parenscript-code)))


(defun make-dependency* (parenscript-code)
  (make-instance 'parenscript-dependency
                 :type :js
                 :js (ps* parenscript-code)))

#|
  This file is a part of weblocks-parenscript project.
|#

(in-package :cl-user)
(defpackage weblocks-parenscript-test-asd
  (:use :cl :asdf))
(in-package :weblocks-parenscript-test-asd)

(defsystem weblocks-parenscript-test
  :author ""
  :license ""
  :depends-on (:weblocks-parenscript
               :prove
               :hamcrest-prove)
  :components ((:module "t"
                :components
                ((:test-file "weblocks-parenscript"))))
  :description "Test system for weblocks-parenscript"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))

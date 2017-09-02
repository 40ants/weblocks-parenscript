(in-package :cl-user)
(defpackage weblocks-parenscript-test
  (:use :cl
        :weblocks-parenscript
        :prove
        :hamcrest.matchers))
(in-package :weblocks-parenscript-test)


(plan 1)

(subtest "Replace this test with real staff."
  (assert-that (foo 1 2 3)
               (contains 1 2 3)))

(finalize)

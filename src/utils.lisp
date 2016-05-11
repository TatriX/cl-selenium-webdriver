(in-package :selenium-utils)

(defparameter *timeout* 30)

(defun wait-for (selector &key (timeout *timeout*))
  (loop
     for i from 0
     for elem = (handler-case
                    (find-element selector)
                  (no-such-element-error () nil))
     until elem
     if (= i (* 2 timeout))
     do (error "Element ~a didn't appeared" selector)
     else
     do (sleep 0.5)
     finally (return elem)))


(defun get-cookie (cookie name)
  (assoc-value (find name
                     cookie
                     :test #'equal
                     :key (rcurry #'assoc-value :name))
               :value))

(defun elem (&optional selector)
  (if selector
      (wait-for selector)
      (active-element)))

(defun attr (name &optional selector)
  (element-attribute (elem selector) name))

(defun id (&optional selector)
  (attr "id" selector))

(defun text (&optional selector)
  (element-text (elem selector)))

(defun send-key (key &optional selector)
  (element-send-keys (elem selector) (key key)))

(defun send-keys (keys &optional selector)
  (element-send-keys (elem selector) keys))

(defun click (&optional selector)
  (element-click (elem selector)))

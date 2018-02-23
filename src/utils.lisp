(in-package :cl-selenium-utils)

(defparameter *timeout* 30)
(defparameter *default-element-func* #'active-element)

(defun find-elem (selector)
  (handler-case (find-element selector)
    (no-such-element-error () nil)))

(defun wait-for (selector &key (timeout *timeout*))
  (loop
     for i from 0
     for elem = (find-elem selector)
     until elem
     if (= i (* 2 timeout))
     do (error "Element ~a didn't appear" selector)
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
      (funcall *default-element-func*)))

(defun attr (name &optional selector)
  (element-attribute (elem selector) name))

(defun id (&optional selector)
  (attr "id" selector))

(defun classname (&optional selector)
  (attr "className" selector))

(defun classlist (&optional selector)
  (split-sequence:split-sequence #\Space (classname selector)))

(defun text (&optional selector)
  (element-text (elem selector)))

(defun send-key (key &optional selector)
  (element-send-keys (elem selector) (key key)))

(defun send-keys (keys &optional selector)
  (element-send-keys (elem selector) keys))

(defun click (&optional selector)
  (element-click (elem selector)))

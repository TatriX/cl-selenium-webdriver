(in-package :cl-selenium)

(defun mouse-move-to (x y &key element (session *session*))
  (http-post-check (session-path session "/moveto")
                   (list :element (when element (element-id element))
                         :xoffset x
                         :yoffset y)))

(defun mouse-click (button &key (session *session*))
  (http-post-check (session-path session "/click")
                   `(:button ,(ecase button
                                     (:left 0)
                                     (:middle 1)
                                     (:right 2)))))

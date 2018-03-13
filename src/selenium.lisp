(in-package :cl-selenium)

(defun (setf url) (url &key (session *session*))
  (http-post-value (session-path session "/url") `(:url ,url)))

(defun url (&key (session *session*))
  (http-get-value (session-path session "/url")))

(defun back (&key (session *session*))
  (http-post-check (session-path session "/back")))

(defclass element ()
  ((id :initarg :id
       :initform (error "Must supply :id")
       :reader element-id)))

(defmethod print-object ((element element) stream)
  (with-slots (id) element
    (format stream
            "#<cl-selenium::element {id:~a} id=~a>"
            id
            (element-attribute element "id"))))

(defun handle-find-error (err &key value by)
  (error
   (case (protocol-error-status err)
     (7 (make-instance 'no-such-element-error :value value :by by))
     (10 (make-instance 'stale-element-reference :value value :by by))
     (t err))))

(defun find-element (value &key (by :css-selector) (session *session*))
  (handler-case
      (let ((response (http-post (session-path session "/element") `(:value ,value :using ,(by by)))))
        ;; TODO: find/write json -> clos
        (if (= 0 (cdr (assoc :status response)))
            (make-instance 'element
                           :id (cdadr (assoc :value response)))
            (error 'protocol-error :body response)))
    (protocol-error (err) (handle-find-error err :value value :by by))))

(defun find-elements (value &key (by :css-selector) (session *session*))
  (handler-case
      (let ((response (http-post (session-path session "/elements") `(:value ,value :using ,(by by)))))
        (loop for ((nil . id)) in (cdr (assoc :value response))
           collect (make-instance 'element :id id)))
    (protocol-error (err) (handle-find-error err :value value :by by))))

(defun by (type)
  (ecase type
    (:id "id")
    (:xpath "xpath")
    (:link-text "link text")
    (:partial-link-text "partial link text")
    (:name "name")
    (:tag-name "tag name")
    (:class-name "class name")
    (:css-selector "css selector")))

(defun active-element (&key (session *session*))
  (make-instance 'element
                 :id (cdadr (assoc :value (http-post (session-path session "/element/active"))))))


(defun element-clear (element &key (session *session*))
  (http-post-check (session-path session "/element/~a/clear" (element-id element))))


(defun element-send-keys (element keys &key (session *session*))
  (check-type keys (string))
  (http-post-check (session-path session "/element/~a/value"
                                 (element-id element))
                   `(:value ,(coerce keys 'list))))

(defun element-click (element &key (session *session*))
  (http-post-check (session-path session "/element/~a/click"
                                 (element-id element))))

(defun element-text (element &key (session *session*))
  (http-get-value (session-path session "/element/~a/text" (element-id element))))

(defun element-displayed (element &key (session *session*))
  (http-get-value (session-path session "/element/~a/displayed" (element-id element))))

(defun element-location (element &key (session *session*))
  (http-get-value (session-path session "/element/~a/location" (element-id element))))

(defun element-tagname (element &key (session *session*))
  (http-get-value (session-path session "/element/~a/name" (element-id element))))

(defun element-attribute (element name &key (session *session*))
  (http-get-value (session-path session "/element/~a/attribute/~a" (element-id element) name)))

(defun log-types (&key (session *session*))
  (http-get-value (session-path session "/log/types")))

(defun logs (type &key (session *session*))
  (http-post-value (session-path session "/log") `(:type ,type)))

(defun screenshot (&key (session *session*))
  (http-get-value (session-path session "/screenshot")))

(defclass cookie ()
  ((name :initarg :name)
   (value :initarg :value)
   (path :initarg :path)
   (domain :initarg :domain)
   (secure :initarg :secure)
   (expiry :initarg :expiry)))

(defun make-cookie (name value &key path domain secure expiry)
  (make-instance 'cookie
                 :name name
                 :value value
                 :path path
                 :domain domain
                 :secure secure
                 :expiry expiry))

(defun (setf cookie) (cookie &key (session *session*))
  (check-type cookie cookie)
  (http-post-check (session-path session "/cookie") `(:cookie ,cookie)))

(defun cookie (&key (session *session*))
  (http-get-value (session-path session "/cookie")))

(defun refresh (&key (session *session*))
  (http-post (session-path session "/refresh")))

(defun switch-to-frame (id &key (session *session*))
  (http-post-check (session-path session "/frame")
                   `(:id ,id)))

(defun close-current-window (&key (session *session*))
  (http-delete (session-path session "/window")))

(defun execute-script (script args &key (session *session*))
  (check-type script string)
  (check-type args list)
  (http-post-value (session-path session "/execute")
                   `(:script ,script :args ,(or args #()))))

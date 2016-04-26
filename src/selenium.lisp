(in-package :selenium)

(defun encode (plist)
  (cl-json:encode-json-plist-to-string plist))

(defun decode (json)
  (cl-json:decode-json-from-string json))

(defparameter *uri* "http://127.0.0.1:4444/")
(defparameter *prefix* "/wd/hub")

(defmacro with-decode-and-handler (body)
  `(handler-case (decode ,body)
     (dex:http-request-failed (err) (error 'protocol-error :body (decode (dex:response-body err))))))

(defun http-get (path)
  (with-decode-and-handler
      (dex:get (make-uri path))))

(defun http-get-value (path value)
  (assoc-value (http-get path) value))

(defun http-post (path &optional params)
  ;; (format t "Sending ~a -> ~a~%" (encode params) path)
  (with-decode-and-handler
      (dex:post (make-uri path)
                :content (encode params)
                :headers '(("Content-Type" . "application/json;charset=UTF-8")))))

(defun http-delete (path)
  (with-decode-and-handler
      (dexador:delete (make-uri path))))

(defun make-uri (path)
  (quri:make-uri :defaults *uri*
                 :path (format nil "~a~a" *prefix* path)))

(defclass session ()
  ((id :initarg :id
       :initform (error "Must supply an id")
       :reader session-id)))

(defvar *session* nil)

(defun make-session (&key
                       (browser-name :chrome) ; TODO: autodetect?
                       browser-version
                       platform-name
                       platform-version
                       accept-ssl-certs)
  (let ((response (http-post "/session"
                             `(:session-id nil
                                           :desired-capabilities ((browser-name . ,browser-name)
                                                                  (browser-version . ,browser-version)
                                                                  (platform-name . ,platform-name)
                                                                  (platform-version . ,platform-version)
                                                                  (accept-ssl-certs . ,accept-ssl-certs))))))
    ;; TODO: find/write json -> clos
    (make-instance 'session
                   :id (assoc-value response :session-id))))

(defun delete-session (session)
  (http-delete (session-path session "")))

;; TODO: make eldoc-friendly
(defmacro with-session ((&rest capabilities) &body body)
  (with-gensyms (session)
    `(let (,session)
       (unwind-protect
            (progn
              (setf ,session (make-session ,@capabilities))
              (let ((*session* ,session))
                ,@body))
         (when ,session
           (delete-session ,session))))))

(defun session-path (session fmt &rest args)
  (format nil "/session/~a~a" (session-id session) (apply #'format nil fmt args)))

(defun (setf url) (url &key (session *session*))
  (http-post (session-path session "/url") `(:url ,url)))

(defun url (&key (session *session*))
  (http-get-value (session-path session "/url") :value))

(defun back (&key (session *session*))
  (http-post (session-path session "/back")))


(defclass element ()
  ((id :initarg :id
       :initform (error "Must supply :id")
       :reader element-id)))

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
        (make-instance 'element
                       :id (cdadr (assoc :value response))))
    (protocol-error (err) (handle-find-error err :value value :by by))))

(defun by (type)
  (ecase type
    (:id "id")
    (:xpath "xpath")
    (:link-text "link text")
    (:partial-link-text "partial link text")
    (:name "name")
    (:tag-name "tag name")
    (:class-name "class-name")
    (:css-selector "css selector")))

(defun active-element (&key (session *session*))
  (make-instance 'element
                 :id (cdadr (assoc :value (http-post (session-path session "/element/active"))))))


(defun element-clear (element &key (session *session*))
  (http-post (session-path session "/element/~a/clear" (element-id element))))


(defun element-send-keys (element keys &key (session *session*))
  (check-type keys (string))
  (http-post (session-path session "/element/~a/value"
                           (element-id element))
             `(:value ,(coerce keys 'list))))

(defun element-click (element &key (session *session*))
  (http-post (session-path session "/element/~a/click"
                           (element-id element))))

(defun element-text (element &key (session *session*))
  (http-get-value (session-path session "/element/~a/text" (element-id element))
                  :value))

(defun element-attribute (element name &key (session *session*))
  (http-get-value (session-path session "/element/~a/attribute/~a" (element-id element) name)
                  :value))

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
  (http-post (session-path session "/cookie") `(:cookie ,cookie)))

(defun cookie (&key (session *session*))
  (http-get-value (session-path session "/cookie") :value))

(defun refresh (&key (session *session*))
  (http-post (session-path session "/refresh")))

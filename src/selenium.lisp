(in-package selenium)

(defun encode (plist)
  (cl-json:encode-json-plist-to-string plist))

(defun decode (json)
  (cl-json:decode-json-from-string json))

(defparameter *uri* "http://127.0.0.1:4444/")
(defparameter *prefix* "/wd/hub")

(defun http-get (path)
  (dex:get (make-uri path)))

(defun http-post (path &optional params)
  ;; (format t "Sending ~a -> ~a~%" (encode params) path)
  (dex:post (make-uri path)
            :content (encode params)
            :headers '(("Content-Type" . "application/json;charset=UTF-8"))))

(defun http-delete (path)
  (dexador:delete (make-uri path)))

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
                       accept-ssl-certs
                       &allow-other-keys)
  (let ((response (decode (http-post "/session"
                                     `(:session-id nil
                                                   :desired-capabilities ((browser-name . ,browser-name)))))))
    ;; TODO: find/write json -> clos
    (make-instance 'session
                   :id (cdr (assoc :session-id response)))))

(defun delete-session (session)
  (http-delete (format nil "/session/~a" (session-id session))))

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
  (cdr (assoc :value (decode (http-get (session-path session "/url"))))))

(defun back (&key (session *session*))
  (http-post (session-path session "/back")))


(defclass element ()
  ((id :initarg :id
       :initform (error "Must supply :id")
       :reader element-id)))

(defun find-element (value &key (by :css-selector) (session *session*))
  (let ((response (decode (http-post (session-path session "/element") `(:value ,value :using ,(by by))))))
    ;; TODO: find/write json -> clos
    (make-instance 'element
                   :id (cdadr (assoc :value response)))))

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


(defun element-clear (element &key (session *session*))
  (http-post (session-path session "/element/~a/clear" (element-id element))))


(defun element-send-keys (element keys &key (session *session*))
  (http-post (session-path session "/element/~a/value"
                           (element-id element))
             `(:value ,(coerce keys 'list))))

(defun element-click (element &key (session *session*))
  (http-post (session-path session "/element/~a/click"
                           (element-id element))))

(defun element-text (element &key (session *session*))
  (let ((response (decode (http-get (session-path session "/element/~a/text" (element-id element))))))
    ;; TODO: find/write json -> clos
    (cdr (assoc :value response))))

(in-package :cl-selenium)

;; uri

(defparameter *uri* "http://127.0.0.1:4444/")
(defparameter *prefix* "/wd/hub")

(defun make-uri (path)
  (quri:make-uri :defaults *uri*
                 :path (format nil "~a~a" *prefix* path)))

;; json

(defun encode (plist)
  (cl-json:encode-json-plist-to-string plist))

(defun decode (json)
  (cl-json:decode-json-from-string json))

;; http

(defmacro with-decode-and-handler (body)
  `(handler-case (decode ,body)
     (dex:http-request-failed (err) (error 'protocol-error :body (decode (dex:response-body err))))))

(defun check (response)
  (zerop (assoc-value response :status)))

(defun value (response)
  (assoc-value response :value))

(defun http-get (path)
  (with-decode-and-handler
      (dex:get (make-uri path))))

(defun http-get-value (path)
  (value (http-get path)))

(defun http-get-check (path)
  (check (http-get path)))

(defun http-post (path &optional params)
  ;; (format t "Sending ~a -> ~a~%" (encode params) path)
  (with-decode-and-handler
      (dex:post (make-uri path)
                :content (encode params)
                :headers '(("Content-Type" . "application/json;charset=UTF-8")))))

(defun http-post-value (path &rest params)
  (value (apply #'http-post path params)))

(defun http-post-check (path &rest params)
  (check (apply #'http-post path params)))

(defun http-delete (path)
  (with-decode-and-handler
      (dexador:delete (make-uri path))))

(defun http-delete-check (path)
  (check (http-delete path)))

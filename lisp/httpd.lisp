(require "socket")
(require "sleep")
(require "time-it")
(provide "httpd")

(defpackage httpd 
  (:use lispyboi
        lispyboi.socket
        lispyboi.sleep)
  (:export http-error))

(in-package :httpd)


(defstruct http-header
  (method) (path) (protocol)
  (user-agent)
  (other-headers)
  (request))

(defun try-parse-integer (string &optional (radix 10))
  (ignore-errors (parse-integer string radix)))

(defun parse-protocol (protocol-string)
  (destructuring-bind (http ver)
      (string-split protocol-string #\/ 1)
    (when (string/= "HTTP" (string-upcase! http))
      (signal 'http-error 400 "Malformed protocol string: ~a" protocol-string))
    (destructuring-bind (major minor)
        (map1 #'try-parse-integer (string-split ver #\. 1))
      (unless (fixnump major)
        (signal 'http-error 400 "Unrecognized HTTP major version: ~a" protocol-string))
      (unless (fixnump minor)
        (signal 'http-error 400 "Unrecognized HTTP minor version: ~a" protocol-string))
      (list major minor))))

(let ((method-map
        (map1 (lambda (s) (cons (symbol-name s) s))
              '(:get :head :post :put :delete :connect
                :options :trace :patch))))
  (defun parse-method (method-string)
    (or (cdr (assoc method-string method-map #'string=))
        (signal 'http-error 400 "Unrecognized method: ~a" method-string))))

(defun parse-http-next (header-string)
  (destructuring-bind (current rest)
      (string-split header-string #\Return 1)
    (list (string-trim current)
          rest)))

(defun unescape-path (path)
  (let ((ss (make-string-stream))
        (i 0))
    (while (< i (length path))
           (cond ((eql #\+ (aref path i))
                  (incf i)
                  (string-stream-write-char ss #\Space))
                 ((eql #\% (aref path i))
                  (string-stream-write-char ss (code-char (parse-integer (substring path (+ i 1) (+ i 3)) 16)))
                  (incf i 3))
                 (t
                  (string-stream-write-char ss (aref path i))
                  (incf i))))
    (string-stream-str ss)))

(defun parse-http-header (header-string)
  (let ((method) (path) (protocol)
        (user-agent)
        (other-headers)
        (header-lines (parse-http-next header-string)))
    (destructuring-bind (meth pat prot . rest)
        (string-split (first header-lines) #\Space 3)
      (when rest
        (signal 'http-error 400 "Too many literal spaces: ~a" (first header-lines)))
      (setf method (parse-method meth))
      (setf path (unescape-path pat))
      (setf protocol (parse-protocol prot)))
    (let ((lines (parse-http-next (second header-lines))))
      ;; go until no more lines or we hit an empty lines
      (until (or (null (first lines))
                 (= 0 (length (first lines))))
        (destructuring-bind (key value)
            (string-split (first lines) #\: 1)
          (when (and key value)
            (push (cons key (string-trim value)) other-headers)))
        (setf lines (parse-http-next (second lines))))
      (setf user-agent (cdr (assoc "User-Agent" other-headers #'string=)))
      (make-http-header :method method
                        :path path
                        :protocol protocol
                        :user-agent user-agent
                        :other-headers other-headers
                        :request (if (second lines)
                                     (string-trim-left (second lines))
                                     (make-string))))))



(let ((status-code-message-map))
  (defun http-status-code-add (status-code message)
    (push (cons status-code message) status-code-message-map))

  (http-status-code-add 100 "Continue")

  (http-status-code-add 200 "OK")
  (http-status-code-add 201 "Created")
  (http-status-code-add 202 "Accepted")

  (http-status-code-add 300 "Multiple Choice")
  (http-status-code-add 301 "Moved Permanently")
  (http-status-code-add 302 "Found")
  (http-status-code-add 307 "Temporary Redirect")
  (http-status-code-add 308 "Permanent Redirect")

  (http-status-code-add 400 "Bad Request")
  (http-status-code-add 401 "Unauthorized")
  (http-status-code-add 403 "Forbidden")
  (http-status-code-add 404 "Not Found")
  (http-status-code-add 405 "Method Not Allowed")
  (http-status-code-add 406 "Not Acceptable")
  (http-status-code-add 411 "Length Required")
  (http-status-code-add 412 "Precondition Failed")
  (http-status-code-add 413 "Payload Too Large")
  (http-status-code-add 414 "URI Too Long")
  (http-status-code-add 415 "Unsupported Media Type")
  (http-status-code-add 429 "Too Many Requests")
  (http-status-code-add 431 "Request Header Fields Too Large")

  (http-status-code-add 500 "Internal Server Error")
  (http-status-code-add 501 "Not Implemented")
  (http-status-code-add 502 "Bad Gateway")
  (http-status-code-add 503 "Service Unavailable")
  (http-status-code-add 505 "HTTP Version Not Supported")
  
  (defun http-status-code-message (status-code)
    (cdr (assoc status-code status-code-message-map #'=))))

(defun http-error (error-code &optional content)
  (array-join "\r\n"
              (format nil "HTTP/1.0 ~a ~a" error-code (http-status-code-message error-code))
              "Connection: close"
              ""
              (if content content "")))

(defun http-ok (content)
  (array-join "\r\n"
              "HTTP/1.0 200 OK"
              "Connection: close"
              "Content-Type: text/html; UTF-8"
              ""
              content))

(defun seconds-from-now (n)
  (+ (get-clock-ticks) (* (clocks-per-second) n)))

(defstruct http-client
  (socket)
  (address)
  (port)
  (time-to-live (seconds-from-now 60)))

(let ((get)
      (post))
  (defun defroute (method route-path view-template controller-function)
    ;;(format t "defroute: ~s~%" route-path)
    (ecase method
      (:get (push (list route-path view-template controller-function) get))
      (:post (push (list route-path view-template controller-function) post))))

  (defun dispatch-path (header)
    (destructuring-bind (the-path the-args)
        (string-split (http-header-path header) #\? 1)
      (when the-args
        (setq the-args (string-split the-args #\&))
        (setq the-args (map1 (lambda (str) (string-split str #\= 1))
                             the-args)))
      (destructuring-bind (path view func)
          (assoc the-path
                 (case (http-header-method header)
                   (:get get)
                   (:post post))
                 #'string=)
        (if func
            (if (eq :raw view) 
                (list :raw (funcall func header path view the-args))
                (list :string (http-ok (funcall func header path view the-args))))
            (list :string (http-error 404 (404-handler header path the-args))))))))

(defconstant +dummy-page+
  "<html>
    <body>
        <h1>Hello world</h1>
        <p>~a<p>
    </body>
</html>")

(defun 404-handler (header path args)
  "<html>
    <body>
        <h1>404.</h1></br>
        <p>That page was not found.</p>
    </body>
</html>")

(defroute :get "/books/" +dummy-page+
  (lambda (header path view args)
    (let ((lines))
      (dolist (book (directory-listing "/home/max/books"))
        (push (format nil "<a href=\"~a\">~a</a></br>~%" (basename book) (basename book)) lines))
      (format nil view (apply #'concatenate lines)))))

(defun get-book-bin (book)
  (let ((bin (make-array 16 'fixnum 0)))
    (with-open-file (file book 'read)
      (if (file-ok-p file)
          (until (file-eof-p file)
                 (let ((b (file-read-byte file)))
                   (when (/= -1 b)
                     (array-push-back bin b))))))
    bin))

(dolist (book (directory-listing "/home/max/books"))
  (defroute :get (concatenate "/books/" (basename book)) :raw
    (lambda (header path view args)
      (let* ((abs-path (concatenate "/home/max" path))
             (book-name (basename abs-path))
             (book-bytes (get-book-bin abs-path))
             (header-bytes (string-bytes
                            (array-join "\r\n"
                                        "HTTP/1.0 200 OK"
                                        "Content-Type: application/pdf"
                                        (format nil "Content-Length: ~a" (length book-bytes))
                                        (format nil "Content-Disposition: inline; filename=~s" book-name)
                                        "" ""))))
        (array-join nil header-bytes book-bytes)))))

(defroute :post "/foo" +dummy-page+
  (lambda (header path view args)
    (format nil view "POST")))

(defroute :get "/baz" +dummy-page+
  (lambda (header path view args)
    (format t "ARGS: ~s~%" args)
    view))

(defun http-handle-one (client)
  (let* ((socket (http-client-socket client))
         (recv (socket-recv-string socket))
         (timestamp (timestamp)))
    (handler-case
        (let ((header (parse-http-header recv)))
          (format t "~a ~a ~s~%"
                  timestamp (http-header-method header) (http-header-path header))
          (destructuring-bind (type response)
              (dispatch-path header)
            (cond ((eq :raw type)
                   (socket-send-bytes socket response))
                  ((eq :string type)
                   (socket-send socket response)))))
      (http-error (code &rest args)
        (if args
            (format t "~a HTTP ERROR ~d: ~s~%" timestamp code (apply #'format nil args))
            (format t "~a HTTP ERROR ~d~%" timestamp code))
        (socket-send socket (http-error code)))
      (t (tag &rest args)
        (format t "~a INTERNAL ERROR ~s: ~s~%" timestamp tag args)
        (socket-send socket (http-error 400))))
    (socket-close socket)
    t))

(defun run-simple-http-server (port &optional (max-connections 20))
  (let* ((server (socket-open-server port :nonblock))
         (clients (list nil)))
    (format t "Listening on port ~a~%" port)
    (while t
      (when (< (length clients) max-connections)
        (destructuring-bind (client-socket client-addr client-port)
            (ignore-errors
             (socket-accept server))
          (when client-socket
            (push (make-http-client :socket client-socket
                                    :address client-addr
                                    :port client-port)
                  clients))))
      (let ((clients clients))
        (while (car clients)
          (let ((socket (http-client-socket (car clients))))
            (if (and socket (socket-alive-p socket))
                (when (http-handle-one (car clients))
                  (pop! clients))
                (progn
                  (socket-close socket)
                  (pop! clients))))
          (setf clients (cdr clients)))
        (msleep 250)))))


(run-simple-http-server (or (and (second *command-line*) (parse-integer (second *command-line*)))
                            8080))

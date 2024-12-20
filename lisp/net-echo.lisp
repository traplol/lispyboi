(require "socket")
(provide "net-echo")

(defpackage net-echo.server
  (:use lispyboi
        lispyboi.socket))

(in-package :net-echo.server)

(defun run-simple-echo-server (port)
  (let* ((server (socket-open-server port))
         (client (car (socket-accept server))))
    (while t
           (let ((recv (socket-recv-string client)))
             (format t ">~a~%" recv)
             (socket-send client recv)))))

(defpackage net-echo.client
  (:use lispyboi
        lispyboi.socket))

(in-package :net-echo.client)

(defun run-simple-echo-client (port)
  (let ((socket (socket-open "localhost" port)))
    (while t
           (format t "CLIENT> ")
           (socket-send socket (file-read-line *standard-input*))
           (format t "SERVER< ~s~%" (socket-recv-string socket)))))

(defpackage net-echo
  (:use lispyboi)
  (:import-from :net-echo.server
                run-simple-echo-server)
  (:import-from :net-echo.client
                run-simple-echo-client))

(in-package :net-echo)

(let ((port nil)
      (mode 'client))

  (dolist (arg *command-line*)
    (cond ((string= "--server" arg)
           (setf mode 'server))
          ((string= "--client" arg)
           (setf mode 'client))
          (t
           (handler-case
               (setf port (parse-integer arg))
             (parse-error (&rest args)
               ;; pass
               )))))
  (when (eq nil port)
    (setf port 11111))

  (if (eq 'server mode)
      (run-simple-echo-server port)
      (run-simple-echo-client port)))

;;; microblog.lisp
;;;
;;; This is a simple microblog I wrote in Common Lisp.  I found that
;;; keeping short status updates were a great way for me to keep a
;;; journal after originally writing this application in Seaside in
;;; Pharo.  Unfortunately, that program seemed to take up a lot of
;;; resources on my laptop, and I've come to really appreciate lisp,
;;; so here we are.
;;;
;;; Copyright (c) 2011 Tom Small
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;; Instructions
;;;
;;; To run this, do the following:
;;;
;;;   1) Start Slime in Emacs
;;;   2) Eval (ql:quickload '(:hunchentoot :cl-who :css-lite :split-sequence))
;;;   3) Compile and load this file (C-x C-k)
;;;   4) Eval (hunchentoot:start *acceptor*)
;;;   5) Eval (in-package :microblog)
;;;   6) Eval (setf *statuses* (load-statuses "statuses"))

(defpackage :microblog
  (:use :common-lisp :hunchentoot :cl-who :css-lite :split-sequence))

(in-package :microblog)

;;;; Model

(defparameter *statuses* '())

(defclass status ()
  ((message      :reader status-message
                 :initarg :message)
   (date-created :reader status-date-created
                 :initform (get-universal-time)
                 :initarg :date-created)))

(defun statuses ()
  (sort (copy-list *statuses*) #'> :key #'status-date-created))

(defun add-status (message)
  (push (make-instance 'status :message message)
        *statuses*))

;;;; Persistence

(defmethod print-object ((stat status) stream)
  (if *print-readably*
      (with-slots (message date-created)
          stat
        (format stream
                (concatenate 'string
                             "#.(make-instance 'status "
                             ":message ~S :date-created ~D)")
                message date-created))
      (call-next-method)))

(defun save-statuses (statuses pathname)
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-readably* t)
            (*package* (find-package :common-lisp)))
        (print statuses stream))))
  pathname)

(defun load-statuses (pathname)
  (with-open-file (stream pathname :direction :input)
    (with-standard-io-syntax
      (let ((*package* (find-package :microblog)))
        (read stream)))))

;;;; Helpers

(defconstant day-names
  '("Monday" "Tuesday" "Wednesday" "Thursday"
    "Friday" "Saturday" "Sunday"))

(defconstant month-names
  '("January" "February" "March" "April" "May" "June" "July"
    "August" "September" "October" "November" "December"))

(defconstant month-short-names
  (map 'list
       (lambda (month) (subseq month 0 3))
       month-names))

(defun humanize-date (universal-time)
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time universal-time)
    (format nil "~a ~d ~d"
            (nth (1- month) month-short-names)
            date
            year)))

(defun humanize-time (universal-time)
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time universal-time)
    (format nil "~d:~2,'0d" hour minute)))

;;;; View

(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html :xmlns "http://www.w3.org/1999/xhtml"
            :xml\:lang "en"
            :lang "en"
            (:head
             (:meta :http-equiv "Content-Type"
                    :content    "text/html;charset=utf-8")
             (:link :rel  "stylesheet"
                    :href "/style")
             (:link :rel "icon"
                    :href "/favicon"
                    :type "image/png")
             (:title ,title))
            (:body
             (:h1 "The Lisp Microblog")
             ,@body))))

(defmacro make-handler ((name) &body body)
  `(push (create-prefix-dispatcher
          ,(format nil "/~(~a~)" name)
          (lambda () ,@body))
         *dispatch-table*))

(make-handler (index)
  (standard-page (:title "The Lisp Microblog")
    (:div :id "body"
          (:form :action "/status" :method "post"
                 (:p (:label "What are you doing?" (:br)
                             (:textarea :id "message"
                                        :name "message"
                                        :autofocus t)))
                 (:p (:input :type "submit"
                             :value "Log It")))
          (:h2 "What have you done recently?")
          (:table
           (dolist (status (statuses))
             (htm
              (:tr
               (:th
                (fmt (humanize-date (status-date-created status)))
                (:br)
                (fmt (humanize-time (status-date-created status))))
               (:td (fmt (status-message status))))))))))

(make-handler (status)
  (let ((message (parameter "message")))
    (add-status message)
    (redirect "/index")))

(make-handler (style)
  (setf (hunchentoot:content-type* hunchentoot:*reply*) "text/css")
  (css-lite:css
    (("body")
     (:background-color "#ddd"
      :font-family "\"Helvetica Neue\", \"Helvetica\", \"Arial\", sans-serif"
      :font-size "12pt"
      :line-height "16pt"
      :width "75%"
      :margin-left "auto"
      :margin-right "auto"
      :padding "1em 0"))
    (("h1")
     (:margin "0 0 1em 0"))
    (("h2")
     (:font-size "1em"
      :font-weight "normal"
      :border-bottom "1px solid #000"))
    (("table")
     (:width "100%"))
    (("th")
     (:color "#777"
      :font-size "0.75em"
      :font-weight "normal"
      :line-height "1.2em"
      :text-align "left"
      :text-transform "uppercase"
      :padding-right "1em"
      :padding-top "0.4em"
      :vertical-align "top"
      :width "13%"))
    (("td")
     (:padding-bottom "1em"))
    (("input[type=\"submit\"]")
     (:font "inherit"))
    (("textarea")
     (:font "inherit"
      :height "4em"
      :width "100%"))
    (("#body")
     (:background-color "#fff"
      :padding "1em 2em"))))

(push (create-static-file-dispatcher-and-handler "/favicon" "favicon.png" "image/png")
      *dispatch-table*)

;;;; Web Server

(defparameter *acceptor*
  (make-instance 'hunchentoot:acceptor :port 9001))

;; (hunchentoot:start *acceptor*)
;; (hunchentoot:stop *acceptor*)
(in-package #:tumble)

(defvar *timezone*)

(defun configure ()
  (setf *prologue* "<!DOCTYPE html>")
  (setf *attribute-quote-char* #\")
  (local-time::reread-timezone-repository)
  (setf *timezone* (find-timezone-by-location-name "Europe/London"))
  (enable-read-macros))

(defun render-tumble-to-files (&optional (pathname #P"random.tumble"))
  (let ((*default-timezone* *timezone*))
    (let* ((tumble (read-tumble-from-file pathname))
           (tags (iterate
                   (for entry in tumble)
                   (unioning (getf (cdr entry) :tags)))))
      (render-tumble-to-file #P"index.html" tumble)
      (iterate
        (for tag in tags)
        (render-tumble-to-file
         (make-pathname :directory (list :relative "tags" (string-downcase tag))
                        :name "index"
                        :type "html")
         (remove-if-not (lambda (entry)
                          (member tag (getf (cdr entry) :tags)))
                        tumble))))))

(defun render-tumble-to-file (pathname tumble)
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (with-html-output (stream stream :prologue T)
      (:html
       (:head
        (:title "random stuff")
        (:meta :charset "utf-8")
        (:link :rel "stylesheet" :href #U"/default.css"))
       (:body
        (:a :href #U"/" (:h1 "random stuff"))
        "おはよう。"
        (mapc (rcurry #'render-entry stream) (mark-dates tumble)))))
    (terpri)))

(defun mark-dates (tumble)
  (iterate
    (with last-date)
    (for entry in tumble)
    (let ((date (getf (cdr entry) :date)))
      (when (or (not last-date)
                (/= (timestamp-year last-date)
                    (timestamp-year date))
                (/= (timestamp-month last-date)
                    (timestamp-month date)))
        (collect `(month :date ,date)))
      (when (or (not last-date)
                (/= (timestamp-day last-date)
                    (timestamp-day date)))
        (collect `(day :date ,date)))
      (setf last-date date))
    (collect entry)))

(defun read-tumble-from-file (pathname)
  (iterate
    (with result)
    (for form in-file pathname)
    (ecase (car form)
      ((video image link)
       (push form result)))
    (finally (return result))))

(defun parse-query (query)
  (mapcar (lambda (parameter)
            (let ((split (split-sequence #\= parameter :count 2)))
              (cons (intern (string-upcase (car split)))
                    (cadr split))))
          (split-sequence #\& query)))

(defun print-query (query)
  (with-output-to-string (stream)
    (iterate
      (for parameter in query)
      (unless (first-iteration-p)
        (format stream "&"))
      (format stream "~(~A~)=~A" (car parameter) (cdr parameter)))))

(defun prepare-embed-url (provider url)
  (ecase provider
    (youtube
     (let ((query (parse-query (uri-query url))))
       (merge-uris (copy-uri #U"." :parsed-path `(:relative ,(cdr (assoc 'v query)))
                                   :query (and (assoc 'list query)
                                               (print-query `((list . ,(cdr (assoc 'list query)))))))
                   #U"https://www.youtube.com/embed/")))))

(defun render-entry (entry stream)
  (let ((body (cdr entry)))
    (with-html-output (stream stream)
      (:div
       (ecase (car entry)
         (link
          (htm
           (:a
            :href (getf body :url)
            (esc (getf body :title)))))
         (month
          (htm
           (:h2 (str (format-timestring NIL (caddr entry) :format '(:long-month " " :year))))))
         (day
          (htm
           (:h3 (str (format-timestring NIL (caddr entry) :format '(:long-weekday ", " :long-month " " :day ", " :year))))))
         (image
          (htm
           (:a
            :href (getf body :url)
            (:img
             :src (getf body :src)
             :alt (getf body :alt)))))
         (video
          (let ((provider (getf body :provider))
                (url (getf body :url))
                (title (getf body :title)))
            (ecase provider
              (youtube
               (htm
                (:iframe
                 :class "noborder"
                 :width 560
                 :height 315
                 :src (or (getf body :embed)
                          (prepare-embed-url provider url))
                 (:a :href url (esc title)))))
              (soundcloud
               (htm
                (:iframe
                 :class "noborder noscrolling"
                 :width 450
                 :height 450
                 :src (or (getf body :embed)
                          (prepare-embed-url
                           provider
                           (getf body :url)))
                 (:a :href url (esc title))))))
            (htm (:br)))))
       (awhen (getf body :tags)
         (htm
          " "
          (:span
           :class "tag"
           (htm "[")
           (iterate
             (for tag in it)
             (unless (first-iteration-p)
               (htm " "))
             (let ((name (string-downcase tag)))
               (htm (:a :href (namestring (make-pathname :directory (list :absolute "tags" name)))
                        (str name)))))
           (htm "]"))))))))

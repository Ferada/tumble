(in-package #:tumble)

(defvar *timezone*)

(defun configure ()
  (setf *attribute-quote-char* #\")
  (local-time::reread-timezone-repository)
  (setf *timezone* (find-timezone-by-location-name "Europe/London"))
  (enable-read-macros)
  (let-over-lambda:|enable-#"-reader-syntax|))

(defun render-tumble-to-files (&key (pathname #P"random.tumble")
                                    (base-url #U"http://tumble.macrolet.net/")
                                    (feedp T))
  (let ((*default-timezone* *timezone*))
    (let* ((tumble (read-tumble-from-file pathname))
           (tags (iterate
                   (for entry in tumble)
                   (unioning (getf (cdr entry) :tags)))))
      (let ((title (format-feed-title NIL)))
        (render-tumble-to-html-file #P"index.html" #P"" base-url NIL title tumble)
        (when feedp
          (render-tumble-to-atom-file #P"index.atom" #P"" base-url base-url NIL title tumble)))
      (iterate
        (for tag in tags)
        (for title = (format-feed-title tag))
        (for relative = (list :relative "tags" (string-downcase tag)))
        (for pathname = (make-pathname :directory relative))
        (for base = #P"../../")
        (for url = (merge-uris (copy-uri #U"." :parsed-path relative) base-url))
        (for filtered-tumble = (remove-if-not
                                (lambda (entry)
                                  (member tag (getf (cdr entry) :tags)))
                                tumble))
        (render-tumble-to-html-file
         (merge-pathnames #P"index.html" pathname)
         base
         url
         tag
         title
         filtered-tumble)
        (when feedp
          (render-tumble-to-atom-file
           (merge-pathnames #P"index.atom" pathname)
           base
           url
           base-url
           tag
           title
           filtered-tumble))))))

(defun format-feed-title (tag)
  (format NIL "~(~A~) feed" (or tag 'full)))

(defun render-tumble-to-atom-file (pathname base url base-url name title tumble)
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (with-html-output (stream stream :prologue "<?xml version='1.0' encoding='utf-8'?>")
      (:feed
       :xmlns "http://www.w3.org/2005/Atom"
       (:title "random stuff")
       (:subtitle (str title))
       (:link :href (merge-uris (file-namestring pathname) url) :rel "self")
       (:link :href url)
       (:id (str url))
       (:updated (str (now)))
       (:author
        (:name "Olof-Joachim Frahm"))
       (mapc (rcurry #'render-entry-to-atom pathname url base-url base stream) tumble)))
    (terpri stream)))

(defun render-entry-to-atom (entry pathname url base-url base stream)
  (let ((body (cdr entry))
        (type (car entry)))
    (with-html-output (stream stream)
      (:entry
       (:link :type "text/html" :href url)
       (:title (str (or (getf body :title) "Untitled")))
       (:id (str (merge-uris (copy-uri #U"." :fragment (princ-to-string (getf body :id))) base-url)))
       (:updated (str (getf body :date)))
       (:content
        :type "html"
        (:xml . :base) base-url
        (esc
         (with-output-to-string (stream)
           (render-entry-to-html
            entry
            (merge-pathnames (file-namestring pathname) base)
            stream
            :iframep NIL))))))))

(defun render-tumble-to-html-file (pathname base url name title tumble)
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (with-html-output (stream stream :prologue "<!DOCTYPE html>")
      (:html
       (:head
        (:title "random stuff")
        (:meta :charset "utf-8")
        (:link :rel "stylesheet" :href (merge-pathnames #P"default.css" base) :title "Default stylesheet")
        (:link :rel "alternate" :href (merge-pathnames #P"index.atom" base) :type "application/atom+xml" :title title))
       (:body
        (:a :href #U"/" (:h1 "random stuff"))
        "おはよう。"
        (mapc (rcurry #'render-entry-to-html base stream) (mark-dates tumble)))))
    (terpri stream)))

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
      ((video image link text)
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
                   #U"https://www.youtube.com/embed/")))
    (vimeo
     (merge-uris (copy-uri #U"." :parsed-path `(:relative ,(cadr (uri-parsed-path url))))
                 #U"https://player.vimeo.com/video/"))))

(defun render-tags (entry base stream)
  (awhen (getf (cdr entry) :tags)
    (with-html-output (stream stream)
      " "
      (:span
       :class "tag"
       (htm "[")
       (iterate
         (for tag in it)
         (unless (first-iteration-p)
           (htm " "))
         (let ((name (string-downcase tag)))
           (htm (:a :href (merge-pathnames (make-pathname :directory (list :relative "tags" name)) base)
                    (str name)))))
       (htm "]")))))

(defun render-entry-to-html (entry base stream &key (iframep T))
  (let ((body (cdr entry))
        (type (car entry)))
    (with-html-output (stream stream)
      (:div
       :class "post"
       (ecase type
         (link
          (htm
           (:a
            :href (getf body :url)
            (esc (getf body :title)))))
         (text
          (markdown (getf body :text) :stream stream)
          (when (getf body :tags)
            (htm (:p (render-tags entry base stream)))))
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
            (flet ((url ()
                     (htm (:a :href url (esc title)))))
              (cond
                ((not iframep)
                 (url))
                (T
                 (ecase provider
                   (youtube
                    (htm
                     (:iframe
                      :class "noborder"
                      :width 560
                      :height 315
                      :src (or (getf body :embed)
                               (prepare-embed-url provider url))
                      (url))))
                   (vimeo
                    (htm
                     (:iframe
                      :class "noborder"
                      :width 560
                      :height 239
                      :src (or (getf body :embed)
                               (prepare-embed-url provider url))
                      (url))))
                   (soundcloud
                    (htm
                     (:iframe
                      :class "noborder noscrolling"
                      :width 560
                      :height 560
                      :src (or (getf body :embed)
                               (prepare-embed-url
                                provider
                                (getf body :url)))
                      (url))))))))
            (htm (:br)))))
       (unless (eq type 'text)
         (render-tags entry base stream))))))

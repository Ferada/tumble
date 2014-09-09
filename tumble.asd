;;; -*- mode: lisp; syntax: common-lisp; package: cl-user; -*-

(in-package #:cl-user)

(asdf:load-system '#:counted-feature-tests)
(counted-feature-tests:file-enable-counted-feature-tests-syntax)

(asdf:defsystem #:tumble
  #2+asdf-unicode :encoding :utf-8
  :depends-on (#:closer-mop #:alexandria #:arnesi #:logv #:cl-who #:iterate #:local-time #:puri #:anaphora #:cl-markdown #:let-over-lambda)
  :serial T
  :components ((:file "package")
               (:file "tumble")))

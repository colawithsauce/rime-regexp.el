;;; rime-regexp.el --- Build regexp from rime easily -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 ColaWithSauce
;;
;; Author: ColaWithSauce <https://github.com/colawithsauce>
;; Maintainer: ColaWithSauce <cola_with_sauce@foxmail.com>
;; Created: August 01, 2021
;; Modified: August 01, 2021
;; Version: 0.0.1
;; Keywords: i18n
;; Homepage: https://github.com/colawithsauce/rime-regexp
;; Package-Requires: ((emacs "25.4") (emacs-rime "1.0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Build regexp from rime easily
;;
;;; Code:

(require 'rime)

(defun rime-regexp-init ()
  "Init rime-regexp."
  (module-load rime--module-path))

(defun rime-regexp-get-candidates-list (str)
  "Read STR, and return the RIME candidates words list."
  (rime-lib-clear-composition)
  (mapc (lambda (c) (rime-lib-process-key c 0)) str)
  (let ((candidates (alist-get 'candidates (alist-get 'menu (rime-lib-get-context))))
        (commit (rime-lib-get-commit))
        (result nil))
    (dolist (c candidates)
      (cl-pushnew (car c) result))
    ;; Commit 是已经肯定的输入；而 candidates 是还没有肯定的输入
    (if (or commit result) `(,commit . ,(reverse result)) nil)))

(defun rime-regexp-build-regexp-string (str)
  "Build regexp with rime use STR."
  (let* ((sep "#####&&&&#####")
         (lst (remove "" (split-string
                          (replace-regexp-in-string
                           "\\([a-z]+'*\\)" (concat sep "\\1" sep) str)
                          sep))))
    ;; 1. pure alpha string: return with ( commit ( context ) ),
    ;; and should be expanded to (seq (or origin (seq commit (seq (or ,contexts)))))
    ;; 2. else  : stay it's origin
    (mapconcat
     (lambda (str)
       "Turn code to regexp."
       (if (or (string-match-p "[^a-z']+" str)
               (equal str ""))
           str
         (let* ((str1 (replace-regexp-in-string "'" "" str))
                (commit-and-candidates (rime-regexp-get-candidates-list str1)) ;; ("计算机科学" ("与" "瓦"))
                (commit (car commit-and-candidates))
                (candidates (cdr commit-and-candidates)))
           ;; Prevent build regexp if not match.
           (if (or commit candidates)
               (rx-to-string
                `(: (or ,str (: ,@commit ,(if candidates `(or ,@candidates) "")))))
            str))))
      lst "")))

(rime-regexp-init)
(provide 'rime-regexp)
;;; rime-regexp.el ends here

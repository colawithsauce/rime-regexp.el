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
;; Homepage: https://github.com/colawithsauce/rime-regexp.el
;; Package-Requires: ((emacs "25.4") (rime "1.0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Build regexp from rime easily
;;
;;; Code:

(require 'rime)

(defvar rime-regexp--max-code-length 0
  "Max code length, if less equal than 0 then no limit on its length. This option is to prevent inputs like:

Input: english
Output: 汀

Explain: 'engl' hit none on wubi table, but 'ish' hit character '汀'.")

;; TODO: 目前对于像 "yfth tuip" 这种中间有空格的输入，仍然使用“原样发给 librime”的方式处理
;; 但是这样会有一个问题：我有的时候可能并不想 commit 第一个字。
;; 拿一个例子来说明，如果我输入：
;;    -> "ni shijie"
;; 现在的逻辑会处理成
;;    -> ("你" "世界" "师姐" "时节" "十届" "视界")
;; 可以看到，只有 “shijie” 会有一个一对多的处理，而 “ni” 没有。我们期望 "ni" 也能一对多。
(defun rime-regexp-get-candidates-list (str)
  "Read STR, and return a list contain RIME commit and candidates.

Input:  yfth
Output: (nil \"计算\" \"谋算\")

Input: yfthgn
Output: (\"计算\" \"与\" \"瓦\")

This function is designed to only take consistent alpha string as args."
  (when (or (<= rime-regexp--max-code-length 0)
            (<= (length str) rime-regexp--max-code-length))
    (setq result nil)

    (rime-lib-clear-composition)
    (mapc (lambda (c) (rime-lib-process-key c 0)) str)

    (setq continue t)
    (while-let ((candidates (alist-get 'candidates (alist-get 'menu (rime-lib-get-context))))
                (continuep continue))
      ;; handle this page
      (let ((first-elem (caar candidates)))
        (if (cl-member first-elem result :test 'string=) ; met recurence
            (setq continue nil) ; then break
          (dolist (c candidates)
            (cl-pushnew (car c) result))))
      ;; goto next page
      (rime-lib-process-key #xff56 0))

    (rime-lib-clear-composition)
    (let ((commit (rime-lib-get-commit)))
      (if (or commit result) `(,commit . ,(reverse result)) nil))))

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
                (commit-and-candidates (rime-regexp-get-candidates-list str1)) ;; ("计算科学" "与" "瓦")
                (commit (car commit-and-candidates))
                (candidates (cdr commit-and-candidates)))
           ;; Prevent build regexp if not match.
           (if (or commit candidates)
               (rx-to-string
                `(: (or ,str (: ,@commit ,(if candidates `(or ,@candidates) "")))))
             str))))
     lst "")))

(defun rime-regexp-load-rime ()
  "Load rime."
  (unless rime--lib-loaded
    (unless (file-exists-p rime--module-path)
      (rime-compile-module))
    (rime--load-dynamic-module)))

(defun rime-regexp-filter-args (args)
  "Get regexp from pinyin."
  (setf (car args) (rime-regexp-build-regexp-string (car args))) args)

(defun isearch-function-with-rime ()
    `(lambda (string &optional bound noerror count)
       (funcall (if ,isearch-forward
                    're-search-forward
                  're-search-backward)
                (rime-regexp-build-regexp-string string) bound noerror count)))

;;;###autoload
(define-minor-mode rime-regexp-mode
  "Search thing using rime."
  :global t
  (if rime-regexp-mode
      (progn
        (rime-regexp-load-rime)
        (advice-add 'orderless-regexp :filter-args #'rime-regexp-filter-args)
        (setq isearch-search-fun-function 'isearch-function-with-rime)
        ;; Add support for evil user
        (advice-add 'evil-ex-search-full-pattern :filter-args #'rime-regexp-filter-args))
    (advice-remove 'orderless-regexp #'rime-regexp-filter-args)
    (setq isearch-search-fun-function 'isearch-function-with-rime)
    (advice-remove 'evil-ex-search-full-pattern #'rime-regexp-filter-args)))

(provide 'rime-regexp)
;;; rime-regexp.el ends here

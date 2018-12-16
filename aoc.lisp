;; -*- mode: Lisp; Syntax: common-lisp; Package: aoc; Base: 10 -*-

(ql:quickload '(:alexandria
                :cl-ppcre
                :array-operations
                :cl-hash-util))

(defpackage :aoc
  (:use :cl
   :alexandria)
  (:import-from :cl-hash-util :hash-get))

(in-package :aoc)

(defun read-aoc-data (file)
  (ppcre:split "\\n" (alexandria:read-file-into-string file)))

(defun day-1 ()
  (let ((result 0))
    (dolist (current (read-aoc-data "day1.txt"))
      (setf result (+ result (read-from-string current))))
    result))

(defun day-2 (&key (result-table (make-hash-table)) (result 0))
  (dolist (current (read-aoc-data "day1.txt"))
    (let ((temp-result (+ result (read-from-string current))))
      (when (gethash temp-result result-table)
        (return-from day-2 temp-result))
      (setf (gethash temp-result result-table) 1)
      (setf result temp-result)))
  (day-2 :result-table result-table :result result))

(defun day-2-2 ()
  (flet ((score-for-box-code (box-code)
           (let ((tmp-result-table (make-hash-table :test 'equal)))
             (dolist (character (coerce box-code 'list))
               (if (gethash character tmp-result-table)
                   (let ((count (gethash character tmp-result-table)))
                     (setf (gethash character tmp-result-table) (+ count 1)))
                   (setf (gethash character tmp-result-table) 1)))
             (let ((two-counts 0)
                   (three-counts 0))
               (dolist (key (alexandria:hash-table-keys tmp-result-table))
                 (when (= (gethash key tmp-result-table) 2)
                   (setf two-counts (+ two-counts 1)))
                 (when (= (gethash key tmp-result-table) 3)
                   (setf three-counts (+ three-counts 1))))
               (list two-counts three-counts)))))
    (let ((twos 0)
          (threes 0))
      (dolist (box-code (read-aoc-data "day2.txt"))
        (destructuring-bind (box-twos box-threes)
            (score-for-box-code box-code)
          (when (< 1 box-twos)
            (setf box-twos 1))
          (when (< 1 box-threes)
            (setf box-threes 1))
          (setf twos (+ twos box-twos))
          (setf threes (+ threes box-threes))))
      (* twos threes))))

(defun day-2-similar-box-ids ()
  (flet ((box-id-sort-fn (a b result-table)
           (let ((a-list (coerce a 'list))
                 (b-list (coerce b 'list))
                 (score 0)
                 (common ""))
             (when (string= a b)
               (return-from box-id-sort-fn (values 0 "" "" "")))
             (dotimes (pos (length a-list))
               (when (string= (nth pos a-list) (nth pos b-list))
                 (setf score (+ score 1))
                 (setf common (format nil "~a~a" common (nth pos b-list)))))
             (when (< 0 score)
               (setf (gethash score result-table) common)))))
    (let ((full-list (read-aoc-data "day2.txt"))
          (result-table (make-hash-table)))
      (dolist (box-code full-list)
        (dolist (b-list full-list)
          (box-id-sort-fn box-code b-list result-table)))
      (let ((highest-score (car (sort (alexandria:hash-table-keys result-table) #'>))))
        (gethash highest-score result-table)))))

(defun parse-line (line)
  (declare (optimize (debug 3)))
  (destructuring-bind (pattern at start-end dims)
      (ppcre:split " " line)
    (declare (ignore at))
    (let ((edges (ppcre:split "," (car (ppcre:split ":" start-end))))
          (dimensions (ppcre:split "x" dims)))
      (list pattern
            (read-from-string (car edges))
            (read-from-string (cadr edges))
            (read-from-string (car dimensions))
            (read-from-string (cadr dimensions))))))

(defun set-pattern (pattern top-edge left-edge width height fabric)
  (declare (optimize (debug 3))
           (type string pattern))
  (dotimes (left-count height)
    (dotimes (top-count width)
      (let ((left (+ left-edge left-count))
            (top (+ top-edge top-count)))
        (if (typep (aref fabric left top) 'string )
            ;; It's already been set once, so now we're overlapping
            (setf (aref fabric left top) "X")
            ;; First visit to this slot.
            (setf (aref fabric left top) pattern))))))

(defun day3-1 ()
  (let ((fabric (make-array '(1001 1001) :initial-element 0 :adjustable t)))
    (dolist (line (read-aoc-data "elf-tailors.txt"))
      (destructuring-bind (pattern left top width height)
          (parse-line line)
        (set-pattern pattern left top width height fabric)))
    (length
     (remove-if-not (lambda (slot)
                      (and (stringp slot)
                           (string= slot "X")))
                    (coerce (aops:flatten fabric) 'list)))))

(defun scan-pattern (pattern top-edge left-edge width height fabric)
  (dotimes (left-count height)
    (dotimes (top-count width)
      (let ((left (+ left-edge left-count))
            (top (+ top-edge top-count)))
        (unless (and (typep (aref fabric left top) 'string )
                     (string= (aref fabric left top) pattern))
          (return-from scan-pattern nil)))))
  pattern)

(defun day3-2 ()
  (let ((lines (ppcre:split "\\n" (alexandria:read-file-into-string "elf-tailors.txt")))
        (fabric (make-array '(1001 1001) :initial-element 0 :adjustable t)))
    (dolist (line lines)
      (destructuring-bind (pattern left top width height)
          (parse-line line)
        (set-pattern pattern left top width height fabric)))
    ;; ewww... but hey, I'm kinda tired.
    ;; I'll fix this tomorrow while I wait for the next challenge.
    (remove-if #'null
               (mapcar (lambda (line)
                         (destructuring-bind (pattern left top width height)
                             (parse-line line)
                           (scan-pattern pattern left top width height fabric)))
                       lines))))



(defparameter *guard-test-data* "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
")

(defstruct guard-record
  year
  month
  day
  hour
  minute
  description
  timestamp)

(defun drop-from-string (pattern string)
  (ppcre:regex-replace-all pattern string ""))

(defun drop-brackets (string)
  (drop-from-string
   "\\]"
   (drop-from-string "\\[" string)))

(defun pad-value (a)
  (format nil "~2,'0d" a))

(defun parse-day4-data ()
  (sort (mapcar (lambda (line)
                  (let* ((words (ppcre:split " " line))
                         (date (ppcre:split "-" (drop-brackets (car words))))
                         (time (ppcre:split ":" (drop-brackets (cadr words))))
                         (sentence (subseq words 2))
                         (year (read-from-string (car date)))
                         (month (read-from-string (nth 1 date)))
                         (day (read-from-string (nth 2 date)))
                         (hour (read-from-string (car time)))
                         (minute (read-from-string (cadr time))))
                    (let  ((record (make-guard-record :year year
                                                      :month month
                                                      :day day
                                                      :hour hour
                                                      :minute minute
                                                      :description (format nil "~{~a~^ ~}" sentence)
                                                      :timestamp (local-time:parse-timestring
                                                                  (format nil
                                                                          "~a-~a-~aT~a:~a:00"
                                                                          year
                                                                          (pad-value month)
                                                                          (pad-value day)
                                                                          (pad-value hour)
                                                                          (pad-value minute))))))
                      record)))
                ;;(read-aoc-data "/home/cmoore/quicklisp/local-projects/aoc2018/guards.txt")
                (ppcre:split "\\n" *guard-test-data*)
                )
        #'< :key #'(lambda (x)
                     (local-time:timestamp-to-unix
                      (guard-record-timestamp x)))))

(defun guard-number-from-string (thingy)
  ;; We want this to go to the debugger on error.
  (let ((this-guard (drop-from-string " begins shift"
                                      (drop-from-string "Guard " thingy))))
    (if (< 0 (length this-guard))
        this-guard
        (break))))


;; ok, now the easy part.
;; Kinda fucked up that getting them sorted was the hardest part, but that's a problem with me
;; and not the language, obviously.




;; shit.  I bet they can fall asleep and wake up more than once
;; in a shift.

(defun day-4-1 ()
  (labels ((range (max &key (min 0) (step 1))
             (loop for n from min below max by step
                   collect n))
           (timestamp-to-minute (timestamp)
             (read-from-string (with-output-to-string (sink)
                                 (local-time:format-timestring sink
                                                               timestamp
                                                               :format (list :min))))))
    (let ((current-guard nil)
          (guard-asleep-time nil)
          (the-results (make-hash-table :test 'equal)))
      
      (dolist (current (parse-day4-data))
        (let ((description (guard-record-description current)))
          (cond
            ((ppcre:scan "begins shift" description)
             (setf current-guard (guard-number-from-string description)))

            ((ppcre:scan "falls asleep" description)
             (setf guard-asleep-time (timestamp-to-minute (guard-record-timestamp current))))

            ((ppcre:scan "wakes up" description)
             ;; hash-get will return null instead of throw an error if the key
             ;; isn't in the hash.
             (let ((old-results (hash-get the-results (list current-guard))))
               ;; Going for a big-ass list of numbers per guard.
               ;; { "guard#" -> (list 1 2 3 31 2  12  3 12 4 4  5 1 2 12 ...)
               (setf (gethash current-guard the-results) (append old-results
                                                                 (range (timestamp-to-minute
                                                                         (guard-record-timestamp current))
                                                                        :min guard-asleep-time)))
               ;; YES, I AM A MORON, I KNOW THX
               (setf guard-asleep-time nil))))))

      ;; ok, now we have the guards and a list for each
      ;; of all of the hours they were asleep.
      ;; Now we need a new list of the guard # and
      ;; the one hour for which there are the most entries
      ;; in that list of hours for them.
      ;;
      ;; "1212" -> (hour,count)
      ;; or something like that.

      ;; (mapcar (lambda (record)
      ;;           (destructuring-bind (guard records) record
      ;;             (log:info (car records))
      ;;             (list guard (car (sort records #'< :key #'cddr))))))
      (destructuring-bind (guard (minute count))
          (car (sort (mapcar (lambda (guard)
                               (let* ((records (gethash guard the-results))
                                      (unique-minutes (remove-duplicates records)))
                                 ;; this is about the time in the problem that
                                 ;; I start getting lazy, partially because the
                                 ;; hard stuff has been figured out.
                                 ;; Yea, I'm a middle-aged kid.
                                 (list guard (car (sort (mapcar (lambda (minute)
                                                                  (list minute (count minute records)))
                                                                unique-minutes)
                                                        #'> :key 'cadr)))))
                             (alexandria:hash-table-keys the-results))
                     #'< :key #'caadr))
        (declare (ignore count))
        (let ((guard-number (drop-from-string "\\#" guard)))
          (list (* (read-from-string guard-number)
                   minute)
                guard-number
                minute))))))


;; I'm sort of lost here.
;; Test data works, but it doesn't like my answer for a full data set.
;; maybe it's read-from-string?... maybe.




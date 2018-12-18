;; -*- mode: Lisp; Syntax: common-lisp; Package: aoc; Base: 10 -*-

(ql:quickload '(:alexandria
                :cl-ppcre
                :local-time
                :log4cl
                :array-operations
                :cl-hash-util))

(defpackage :aoc
  (:use :cl
   :alexandria)
  (:import-from :alexandria :flatten :alist-hash-table :hash-table-keys :read-file-into-string))

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

(defun drop-from-string (pattern string)
  (ppcre:regex-replace-all pattern string ""))

(defun drop-brackets (string)
  (drop-from-string
   "\\]"
   (drop-from-string "\\[" string)))

(defun pad-value (a)
  (format nil "~2,'0d" a))

(defun guard-number-from-string (thingy)
  ;; We want this to go to the debugger on error.
  (let ((this-guard (drop-from-string " begins shift"
                                      (drop-from-string "Guard " thingy))))
    (if (< 0 (length this-guard))
        (parse-integer this-guard :start 1)
        (break))))

(defun format-date (timestamp format)
  (with-output-to-string (sink)
    (local-time:format-timestring sink timestamp :format format)))

(defun parse-day4-data ()
  (sort (mapcar (lambda (line)
                  (let* ((words (ppcre:split " " line))
                         (date (ppcre:split "-" (drop-brackets (car words))))
                         (time (ppcre:split ":" (drop-brackets (cadr words))))
                         (sentence (subseq words 2))
                         (year (parse-integer (nth 0 date)))
                         (month (parse-integer (nth 1 date)))
                         (day (parse-integer (nth 2 date)))
                         (hour (parse-integer (car time)))
                         (minute (parse-integer (cadr time)))
                         (timestamp (local-time:parse-timestring
                                     (format nil
                                             "~a-~a-~aT~a:~a:00"
                                             year
                                             (pad-value month)
                                             (pad-value day)
                                             (pad-value hour)
                                             (pad-value minute)))))
                    (list timestamp (format nil "~{~a~^ ~}" sentence))))
                (read-aoc-data "/home/cmoore/quicklisp/local-projects/aoc2018/guards.txt"))
        #'< :key #'(lambda (x)
                     (local-time:timestamp-to-unix (car x)))))

(defun day-4-1 ()
  (declare (optimize (debug 3)))
  (labels ((range (min max)
             (loop for n from min below (+ 1 max) by 1
                   collect n)))
    (let ((current-guard nil)
          (guard-asleep-timestamp nil)
          (results (make-hash-table :test 'equal)))
      
      (dolist (current (parse-day4-data))
        (destructuring-bind (timestamp description) current
          (cond
            ((ppcre:scan "begins shift" description)
             (setf current-guard (guard-number-from-string description)))

            ((ppcre:scan "falls asleep" description)
             (setf guard-asleep-timestamp timestamp))

            ((ppcre:scan "wakes up" description)
             (let ((old-results (gethash current-guard results nil))
                   (new-results (range (parse-integer (format-date guard-asleep-timestamp (list :min)))
                                       (parse-integer (format-date timestamp (list :min))))))
               (setf (gethash current-guard results)
                     (append old-results new-results))
               (setf guard-asleep-timestamp nil))))))

      
      (let ((part-1-result (destructuring-bind (guard (minute count) total-hours-slept)
                               (car (sort (mapcar (lambda (guard)
                                                    (let ((minutes (gethash guard results)))
                                                      (list guard
                                                            (car (sort (mapcar (lambda (minute)
                                                                                 (list minute (count minute minutes)))
                                                                               (remove-duplicates minutes))
                                                                       #'> :key #'cadr))
                                                            (length minutes))))
                                                  (hash-table-keys results))
                                          #'> :key #'caddr))
                             (declare (ignore count total-hours-slept))
                             (* guard minute)))
            (part-2-result
              (destructuring-bind (guard (minute xcount))
                  (car (sort (mapcar (lambda (guard)
                                       (let* ((minutes (gethash guard results))
                                              (unique-minutes (remove-duplicates minutes)))
                                         (list guard
                                               (car (sort (mapcar (lambda (minute)
                                                                    (list minute (count minute minutes)))
                                                                  unique-minutes)
                                                          #'> :key #'cadr)))))
                                     (hash-table-keys results))
                             #'> :key #'cadadr))
                (declare (ignore xcount))
                (* guard minute))))
        (list part-1-result part-2-result)))))

(defun strip (string)
  (ppcre:regex-replace-all "\\n" string ""))

(defparameter *day5-test-data* "dabAcCaCBAcCcaDA")

(defparameter *day5-real-data* (strip (alexandria:read-file-into-string "polymer.txt")))

(defun collapse (a b)
  ;; There's an eq(?) for everything.
  (cond ((and (not (string= a b))
              (equalp a b))
         "")
        (t (format nil "~A~A" a b))))

(defun collapse-polymer (current-char input-stream output-stream)
  (declare (optimize (debug 0) (speed 2) (space 3)))
  (let ((next-char (read-char input-stream nil)))
    (unless next-char
      (and current-char
           (format output-stream "~a" current-char))
      (return-from collapse-polymer (get-output-stream-string output-stream)))
    
    (let ((collapsed (collapse current-char next-char)))
      (if (string= collapsed "")
          (setf next-char (read-char input-stream nil))
          (and current-char
               (format output-stream "~a" current-char))))
    (collapse-polymer next-char input-stream output-stream)))

(defun scan-polymer (polymer)
  (declare (optimize (debug 0) (speed 2) (space 3)))
  (let ((output (make-string-output-stream)))
    (with-input-from-string (input polymer)
      (let ((first-character (read-char input)))
        (collapse-polymer first-character input output)))))

(defun react-polymer (polymer)
  (declare (optimize (debug 0) (speed 3) (space 3)))
  (loop
    (let ((new-polymer (scan-polymer polymer)))
      (if (string= polymer new-polymer)
          (return-from react-polymer new-polymer)
          (setf polymer new-polymer)))))

(defun day-5-1 ()
  (length
   (react-polymer (ppcre:regex-replace-all "\\n" *day5-real-data* ""))))

(defun clean-polymer (char polymer)
  (let ((ugh (ppcre:regex-replace-all (format nil "~a" char) polymer "")))
    (ppcre:regex-replace-all (format nil "~a" (string-upcase
                                               (format nil "~a" char)))
                             ugh "")))

(defun day-5-2 ()
  (car (sort (mapcar (lambda (char)
                       (length (react-polymer (clean-polymer char *day5-real-data*))))
                     (coerce "abcdefghijklmnopqrstuvxyzw" 'list))
             #'<)))












































(defparameter *day6-test-data* "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9")
(defparameter *day6-real-data* (alexandria:read-file-into-string "coords.txt"))

(defun distance (pair x y)
  (+ (abs (- (car pair) x))
     (abs (- (cadr pair) y))))

(defun print-pair (pair)
  (format nil "~a/~a" (car pair) (cadr pair)))

(defun make-pairs (string)
  (map 'list (lambda (x)
               (destructuring-bind (a b)
                   (ppcre:split ", " x)
                 (list (parse-integer a) (parse-integer b))))
       (ppcre:split "\\n" string)))

(defun cell-owner (pairs x y)
  (declare (optimize (debug 0) (speed 3) (space 3)))
  ;; Determine the 'owner' of this cell
  (let ((values (sort (mapcar (lambda (pair)
                                (list pair (distance pair x y)))
                              pairs)
                      #'< :key #'cadr)))
    (cond ((= (cadr (car values))
              (cadr (cadr values)))
           ".")
          (t (caar values)))))

(defun bounding-box-values (board)
  (declare (optimize (debug 0) (speed 3) (space 3)))
  ;; values at 0,0 -> 0,size + 0,0 -> size,0 + size,0 -> size,size + 0,size + size,size
  ;; etc.
  (let ((result nil))
    (destructuring-bind (size-x size-y) (array-dimensions board)
      (dotimes (element size-x)
        (setf result (append result (list (aref board 0 element)))))
      (dotimes (element size-x)
        (setf result (append result (list (aref board (- size-x 1) element)))))
      (dotimes (element size-y)
        (setf result (append result (list (aref board element 0)))))
      (dotimes (element size-y)
        (setf result (append result (list (aref board element (- size-y 1)))))))
    result))

(defun make-board (pairs)
  (let ((board-size (+ 1 (car (sort (alexandria:flatten pairs) #'>)))))
    (make-array (list board-size board-size) :initial-element "")))

(defun plot-pair (board pair)
  (declare (optimize (debug 0) (speed 3) (space 3)))
  (destructuring-bind (x y) pair
    (setf (aref board y x) pair)))

(defun plot-owners (board pairs)
  (declare (optimize (debug 0) (speed 3) (space 3)))
  ;; Set the 'owner' of all cells
  ;; it's both side affecting, and returns a new value
  ;; so basically all of the worst parts of the bible
  ;; in one function.
  (let ((cell-values nil)
        (board-size (car (array-dimensions board))))
    (dotimes (x board-size)
      (dotimes (y board-size)
        (let ((owner-result (cell-owner pairs x y)))
          (setf cell-values (append cell-values (list owner-result)))
          (setf (aref board y x) owner-result))))
    cell-values))

(defun get-bounding-pairs (board)
  (declare (optimize (debug 0) (speed 3) (space 3)))
  (remove-if (lambda (x)
               (equalp x "."))
             (remove-duplicates (bounding-box-values board))))

(defun day6-1 ()
  (declare (optimize (debug 0) (speed 3) (space 3)))
  (let* ((pairs (make-pairs *day6-real-data*))
         (board (make-board pairs)))
    (let ((cell-values (plot-owners board pairs)))
      (let* ((bounding-pairs (get-bounding-pairs board))
             (unbounded-pairs (remove-if (lambda (x)
                                           (member x bounding-pairs))
                                         pairs))
             (new-cell-values (remove-if (lambda (x)
                                           (equal x "."))
                                         cell-values)))
        (car (sort (mapcar (lambda (unbound-pair)
                             (list unbound-pair (count unbound-pair new-cell-values :test #'equalp)))
                           unbounded-pairs)
                   #'> :key #'cadr))))))

(defun calculate-distances (x y pairs)
  (mapcar (lambda (pair)
            (distance pair x y))
          pairs))

(defun day6-2 ()
  (let* ((pairs (make-pairs *day6-real-data*))
         (threshold 10000)
         (board (make-board pairs))
         (board-size (car (array-dimensions board)))
         (results nil))

    (dolist (x (range 0 board-size))
      (dolist (y (range 0 board-size))
        (let ((distances (reduce #'+ (calculate-distances x y pairs))))
          (when (<= distances threshold)
            (setf results (append results (list (list x y distances))))))))
    (length results)))




















(defparameter *day7-test-data* "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")
(defparameter *day7-real-data* (alexandria:read-file-into-string "steps.txt"))

(defun parse-steps (string)
  (let* ((raw-steps (mapcar (lambda (line)
                          (let ((elements (ppcre:split " " line)))
                            (list (nth 7 elements) (nth 1 elements))))
                        (ppcre:split "\\n" string)))
         (unique (sort (remove-duplicates
                        (alexandria:flatten
                         (list (mapcar #'car raw-steps)
                               (mapcar #'cdr raw-steps)))
                        :test #'string=)
                       #'string<)))
    (mapcar (lambda (step)
              ;; oops, I think I deleted depends-of
              (list step (depends-of step raw-steps)))
            unique)))

;; (defun depends-on (step steps)
;;   ;; Has to be in alphabetical order.
;;   (remove-if-not (lambda (x)
;;                    (member (car step)
;;                            (cadr x) :test #'string=))
;;              steps))

(defun first-step (steps)
  (car (remove-if-not #'null steps :key #'cadr)))

(defun clear-step (step steps)
  "Wipes step from the step list and dependencies."
  (mapcar (lambda (x)
            (destructuring-bind (name deps) x
              (list name (remove-if (lambda (x)
                                      (string= x step))
                                    deps))))
          (remove-if (lambda (x)
                       (string= step (car x)))
                     steps)))

(defun next-step (steps)
  (let ((next-step (remove-if-not (lambda (x)
                                    (null (cadr x)))
                                  (sort steps #'string< :key #'car))))
    (car next-step)))

(defun interpret (step steps output)
  (format output (car step))
  (let* ((new-step-list (clear-step (car step) steps))
         (next-step (next-step new-step-list)))
    (when next-step
      (current-step next-step new-step-list output))))

(defun day7-1 ()
  (let* ((steps (parse-steps *day7-real-data*))
         (first-step (next-step steps))
         (output (make-string-output-stream)))
    (interpret first-step steps output)
    (pprint (get-output-stream-string output))))


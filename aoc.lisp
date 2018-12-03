

;;
;; Yea, I don't like loop.  It's true.
;;

(ql:quickload '(:alexandria
                :cl-ppcre
                :array-operations))

(defpackage :aoc
  (:use :cl
        :alexandria))

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

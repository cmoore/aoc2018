
(defpackage #:goldlists
  (:use #:cl
        #:iterate)
  (:export :circular-list
           :immutable-circular-list
           :print-object
           :make-circular-list
           :clist-equal
           :clist-focused
           :clist-insert
           :clist-rotate
           :clist-remove-focused))

(in-package #:goldlists)

(defclass circular-list ()
  ((current :initarg :current))
  (:documentation
   "A circular list represents a series of objects that conceptually form
   a ring.  The *focus* of a circular list allows O(1) access to the
   focused object and O(1) insertion adjacent to it.  A circular list may
   be *rotated* to shift the focus."))

(defclass immutable-circular-list (circular-list)
  ((left :initarg :left
         :initform nil)
   (right :initarg :right
          :initform nil))
  (:documentation
   "This circular list implementation is immutable.  All of the
   modification methods return a new list (which might share structure
   with the old one)."))

(defclass mutable-circular-list (circular-list)
  ((left :initarg :left
         :initform nil)
   (right :initarg :right
          :initform nil))
  (:documentation
   "This circular list implementation is mutable."))

(defmethod print-object ((clist immutable-circular-list) stream)
  (labels ((summarize-list (n list)
             (cond
               ((endp list)
                nil)
               ((zerop n)
                '("..."))
               (t
                (cons (car list) (summarize-list (1- n) (cdr list)))))))
    (with-slots (left current right) clist
      (let ((left-summary (reverse (summarize-list 2 left)))
            (right-summary (summarize-list 2 right)))
        (format stream "#<IMMUTABLE-CIRCULAR-LIST ~{~A ~}[~A]~{~#[~:; ~]~A~}>"
                left-summary current right-summary)))))

(defmethod print-object ((clist mutable-circular-list) stream)
  (format stream "#<MUTABLE-CIRCULAR-LIST ... ~A ~A [~A] ~A ~A ...>"
          (clist-focused (slot-value (slot-value clist 'left) 'left))
          (clist-focused (slot-value clist 'left))
          (clist-focused clist)
          (clist-focused (slot-value clist 'right))
          (clist-focused (slot-value (slot-value clist 'right) 'right))))

(defun make-circular-list (initial-element-or-sequence &key (mutable nil))
  "Creates a circular list.  If INITIAL-ELEMENT-OR-SEQUENCE is a sequence,
  the focus will be on the first element in the sequence.  Nested
  sequences are not supported."
  (when (and mutable
             (typep initial-element-or-sequence 'sequence)
             (typep (elt initial-element-or-sequence 0) 'sequence))
    (error "Nested sequences are not supported for mutable circular lists."))
  (if mutable
      (if (typep initial-element-or-sequence 'sequence)
          (let ((clist (make-circular-list (elt initial-element-or-sequence 0) :mutable t)))
            (iter (for e in-sequence (subseq initial-element-or-sequence 1))
                  (for current-clist first (clist-insert clist e :right)
                                     then (clist-insert current-clist e :right)))
            clist)
          (let ((clist (make-instance 'mutable-circular-list :current initial-element-or-sequence)))
            (setf (slot-value clist 'left) clist)
            (setf (slot-value clist 'right) clist)
            clist))
      (if (typep initial-element-or-sequence 'sequence)
          (let ((initial-list (map 'list #'identity initial-element-or-sequence)))
            (make-instance 'immutable-circular-list
                           :current (car initial-list)
                           :right (cdr initial-list)))
          (make-instance 'immutable-circular-list :current initial-element-or-sequence))))

(defgeneric clist-equal (clist1 clist2)
  (:documentation
   "Returns t if both circular lists have the same contents and the same
   focus.")
  (:method (obj1 obj2)
    nil))

(defmethod clist-equal ((clist1 immutable-circular-list) (clist2 immutable-circular-list))
  (with-slots ((left1 left) (current1 current) (right1 right)) clist1
    (with-slots ((left2 left) (current2 current) (right2 right)) clist2
      (and (equal current1 current2)
           (equal (append right1 (reverse left1))
                  (append right2 (reverse left2)))))))

(defmethod clist-equal ((clist1 mutable-circular-list) (clist2 mutable-circular-list))
  (and (equal (clist-focused clist1) (clist-focused clist2))
       (iter (for c1 first (clist-rotate clist1 1) then (clist-rotate c1 1))
             (for c2 first (clist-rotate clist2 1) then (clist-rotate c2 1))
             (until (eq c1 clist1))
             (always (equal (clist-focused c1) (clist-focused c2))))))

(defgeneric clist-focused (clist)
  (:documentation "Returns the focused object in the circular list."))

(defmethod clist-focused ((clist circular-list))
  (slot-value clist 'current))

(defgeneric clist-insert (clist new-object &optional position)
  (:documentation
   "Adds NEW-OBJECT to a circular list and returns the new list.  The
   newly-added object will be focused.  If POSITION is :left, the object
   will be added to the left of the previously-focused object.  If
   POSITION is :right, the object will be added to the right."))

(defmethod clist-insert ((clist immutable-circular-list) new-object &optional (position :left))
  (when (and (not (eq position :left))
             (not (eq position :right)))
    (error "POSITION must be either :left or :right, not ~A" position))
  (with-slots (left current right) clist
    (if (eq position :left)
        (make-instance 'immutable-circular-list
                       :left left
                       :current new-object
                       :right (cons current right))
        (make-instance 'immutable-circular-list
                       :left (cons current left)
                       :current new-object
                       :right right))))

(defmethod clist-insert ((clist mutable-circular-list) new-object &optional (position :left))
  (when (and (not (eq position :left))
             (not (eq position :right)))
    (error "POSITION must be either :left or :right, not ~A" position))
  (with-slots (left current right) clist
    (let ((new-node (if (eq position :left)
                        (make-instance 'mutable-circular-list
                                       :left left
                                       :current new-object
                                       :right clist)
                        (make-instance 'mutable-circular-list
                                       :left clist
                                       :current new-object
                                       :right right))))
      (setf (slot-value (slot-value new-node 'right) 'left) new-node)
      (setf (slot-value (slot-value new-node 'left) 'right) new-node)
      new-node)))

(defgeneric clist-rotate (clist n)
  (:documentation
   "Moves the focus N steps to the right and returns the newly-focused
   circular list.  N may be negative (which will move the focus to the
   left)."))

(defmethod clist-rotate ((clist immutable-circular-list) n)
  (with-slots (left current right) clist
    (cond
      ((zerop n)
       clist)
      ((plusp n)
       (destructuring-bind (new-left new-right)
           (if (endp right)
               (list nil
                     (reverse (cons current left)))
               (list (cons current left)
                     right))
         (clist-rotate (make-instance 'immutable-circular-list
                                      :left new-left
                                      :current (car new-right)
                                      :right (cdr new-right))
                       (1- n))))
      ((minusp n)
       (destructuring-bind (new-left new-right)
           (if (endp left)
               (list (reverse (cons current right))
                     nil)
               (list left
                     (cons current right)))
         (clist-rotate (make-instance 'immutable-circular-list
                                      :left (cdr new-left)
                                      :current (car new-left)
                                      :right new-right)
                       (1+ n)))))))

(defmethod clist-rotate ((clist mutable-circular-list) n)
  (with-slots (left current right) clist
    (cond
      ((zerop n)
       clist)
      ((plusp n)
       (clist-rotate right (1- n)))
      ((minusp n)
       (clist-rotate left (1+ n))))))

(defgeneric clist-remove-focused (clist)
  (:documentation
   "Removes the focused value and replaces it with the value to its right.
   Returns two values: a circular list with the value removed; and the
   value that was removed.  Removing the last value in a circular list
   leads to undefined behavior."))

(defmethod clist-remove-focused ((clist immutable-circular-list))
  (with-slots (left current right) clist
    (values (destructuring-bind (new-left new-right)
                (if (endp right)
                    (list nil (reverse left))
                    (list left right))
              (make-instance 'immutable-circular-list
                             :left new-left
                             :current (car new-right)
                             :right (cdr new-right)))
            current)))

(defmethod clist-remove-focused ((clist mutable-circular-list))
  (with-slots (left current right) clist
    (setf (slot-value right 'left) left)
    (setf (slot-value left 'right) right)
    (values right
            current)))


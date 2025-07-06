;;; Filename: ww-hstack.lisp

;;; An hstack (hash stack) is a stack representation containing an adjustable
;;; one-dimensional array to organize elements, plus a hash table for quickly
;;; determining if an element is in the stack. New elements are pushed
;;; at the fill-pointer, and popped at the fill-pointer minus 1. 

(in-package :hs)


(defstruct (hstack (:conc-name hstack.))
  "An hstack (hash stack) is a functional stack containing an adjustable
   one-dimensional array of elements, plus a hash table for quickly
   determining if an element is in the stack. Keyfn is applied to elements to
   access the hash table. New elements are pushed at the fill-pointer, and
   popped at the fill-pointer minus 1."
  (vector (make-array 0 :adjustable t :fill-pointer t) :type (array * (*)))
  (table (make-hash-table))  ;can take a normal hash-table or genhash generic hash table
  (keyfn #'identity :type function))  ;fn to get hash table keys


(defun push-hstack (elt hstk &key new-only)  ;only push if new
  "Pushes an element onto hstack's vector and table.
   Returns the hstack and whether the element was added."
  (let ((key (funcall (hstack.keyfn hstk) elt))
        (vector (hstack.vector hstk))
        (table (hstack.table hstk)))
    (if new-only  ;only push if new specified
      (if (nth-value 1 (gethash key table))  ;key is already present
        (values hstk nil)  ;element's key already present, do nothing
        (progn (setf (gethash key table) (list elt))
               (vector-push-extend elt vector)
               (values hstk t)))
      (progn (push elt (gethash key table))
             (vector-push-extend elt vector)
             (values hstk t)))))



(defun pop-hstack (hstk)
  "Pops an element from hstack's vector and removes it from the table. Error if hstk empty."
  (let* ((vector (hstack.vector hstk))
         (table (hstack.table hstk))
         (fptr-1 (1- (fill-pointer vector)))
         (key (funcall (hstack.keyfn hstk) (aref vector fptr-1))))  ;key in elt at top of vec stack
    (let ((values (gethash key table)))  ;key guaranteed to be in table with list of values
      (pop values)
      (when (null values)
        (remhash key table))  ;key's value is now nil so remove it
      (vector-pop vector))))



(defun empty-hstack (hstk)
  "Determine if a hash stack is empty."
  (zerop (fill-pointer (hstack.vector hstk))))


(defun length-hstack (hstk)
  (length (hstack.vector hstk)))


(defun nth-hstack (n hstk)
  (aref (hstack.vector hstk) n))


(defun deletef-nth-hstack (n hstk)
  "Deletes the nth entry in a hash stack and returns it."
  (declare (type fixnum n) (type hstack hstk))
  (let* ((vec (hstack.vector hstk))
         (tbl (hstack.table hstk))
         (nth-entry (aref vec n))
         (key (funcall (hstack.keyfn hstk) nth-entry)))
    (remhash key tbl)
    (setf vec (delete-if (constantly t) vec :start n :count 1))
    nth-entry))


(defun size-hstack (hstk)
  (array-total-size (hstack.vector hstk)))


(defun peek-hstack (hstk)
  (let ((fptr (fill-pointer (hstack.vector hstk))))
    (when (> fptr 0)
      (aref (hstack.vector hstk) (1- fptr)))))


(defun clear-hstack (hstk)
  (setf (fill-pointer (hstack.vector hstk)) 0)
  (clrhash (hstack.table hstk)))

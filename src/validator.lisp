(in-package :nz.geek.abhishek.py-configvalidator)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

;; Possible specifier forms:
;;
;; "string"
;; True
;; False
;; [boolean] True
;; [boolean] False
;; [number] n
;; lisp-number
;; [range x y] n
;; [range x y]
;; [min x] n
;; [min x]
;; [max x] n
;; [max x]
;; string
;;
;; Value forms are the same with the exception of constraints (i.e. with [...]).

(define-condition no-option-error (error)
  ((section-name :initarg :section-name :reader c/section-name)
   (option-name :initarg :option-name :reader c/option-name)))

(define-condition no-section-error (error)
  ((section-name :initarg :section-name :reader c/section-name)
   (option-name :initarg :option-name :reader c/option-name)))

(define-condition no-conf-option-error (no-option-error) ()
  (:report (lambda (c s)
             (format s "No conf option \"~a\" in section \"~a\"."
                     (c/option-name c) (c/section-name c)))))

(define-condition no-spec-option-error (no-option-error) ()
  (:report (lambda (c s)
             (format s "No spec option \"~a\" in section \"~a\"."
                     (c/option-name c) (c/section-name c)))))

(define-condition no-section-error (error)
  ((section-name :initarg :section-name :reader c/section-name)))

(define-condition no-conf-section-error (no-section-error) ()
  (:report (lambda (c s)
             (format s "No conf section \"~a\"." (c/section-name c)))))

(define-condition no-spec-section-error (no-section-error) ()
  (:report (lambda (c s)
             (format s "No spec section \"~a\"." (c/section-name c)))))

(defclass spec-value ()
  ((%value :initform nil :initarg :value :reader spec-value)))

(defclass spec-boolean (spec-value) ())

(defclass spec-number (spec-value)
  ((%min :initform nil :initarg :min :reader spec-number-min)
   (%max :initform nil :initarg :max :reader spec-number-max)))

(defclass spec-string (spec-value) ())

(defun tokenize-words (words)
  "Return a list of string tokens separated by spaces in the string WORDS."
  (remove-if (lambda (s) (string= s ""))
             (cl-utilities:split-sequence #\Space words)))

(defun unparse-value (value)
  (cond ((stringp value) value)
        ((numberp value) (format nil "~a" value))
        ((eq t value) "true")
        ((eq nil value) "false")
        (t "")))

(defmethod print-object ((o spec-value) s)
  (format s "#<~a value: ~a>" (class-name (class-of o))
          (unparse-value (spec-value o))))

(defmethod print-object ((o spec-number) s)
  (format s "#<~a value: ~a min: ~a max: ~a>"
          (class-name (class-of o))
          (unparse-value (spec-value o))
          (spec-number-min o)
          (spec-number-max o)))

(defgeneric validate (spec-object value)
  (:documentation
   "Return a valid CL object, represented by either VALUE or some function of
SPEC-OBJECT, depending on type constraints."))

(defmethod validate ((s spec-boolean) (v spec-boolean))
  (spec-value v))

(defmethod validate ((s spec-number) (v spec-number))
  (let ((min (spec-number-min s))
        (max (spec-number-max s))
        (val (spec-value v)))
  (cond ((and (null min) (null max))
         val)
        ((and min max)
         (if (<= min val max) val (or val (alexandria:clamp val min max))))
        ((and (null min) max)
         (if (<= val max) val (or val max)))
        ((and min (null max))
         (if (<= min val) val (or val min))))))

(defmethod validate ((s spec-string) (v spec-string))
  (spec-value v))

(defmethod validate ((s spec-value) v)
  (declare (ignore v))
  (spec-value s))

(defun first-quoted-string (quoted-string)
  "Returns a substring of QUOTED-STRING beginning at the start and ending
at either the first unescaped #\" or the end of string.

It is assumed that the opening quotation mark has already been consumed." 
  (assert (> (length quoted-string) 0))
  (loop :with escaped := nil
        :for c :across (subseq quoted-string 1)
        :until (and (char= c #\") (not escaped))
        :do (when (char= c #\\) (setq escaped (not escaped)))
        :collect c :into s
        :finally (return (format nil "~{~a~}" s))))

(defun first-word (words)
  "Return the first string token before a space in the string WORDS."
  (assert (> (length words) 0))
  (first (tokenize-words words)))

(defun rest-words (words)
  "Return a list of string tokens that were separated by spaces in the string
WORDS."
  (assert (> (length words) 0))
  (rest (tokenize-words words)))

(defun constraint-part (c)
  "Returns a substring of string C beginning after the first character and
ending either before the first #\] or the end of string.  This represents the
constraint portion of a value string.

It is assumed that the first character is a #\[ and that a constraint is
denoted by the input string."
  (string-trim (list #\Space #\Tab) (subseq c 1 (position #\] c))))

(defun value-part (c)
  "Returns a substring of string C beginning with the first token after a #\].
If no #\] is found, the empty string is returned."
  (string-trim (list #\Space #\Tab)
               (subseq c (1+ (or (position #\] c) (1- (length c)))))))

(defun parse-number (s &optional (default nil))
  "Return the CL number represented by string S if valid, otherwise return
DEFAULT."
  (handler-case (org.mapcar.parse-number:parse-number s)
    (error (c)
      (declare (ignore c))
      default)
    (org.mapcar.parse-number::invalid-number (c)
      (declare (ignore c))
      default)))

(defun number-like-p (s)
  "Return T if string S represents a valid CL number, NIL otherwise."
  (when (parse-number s) t))

(defun parse-constrained-spec-type (s)
  "Returns a spec-value object of the constrained type denoted by the string S,
initialized with any default or constraining values."
  (let* ((constraint (constraint-part s))
         (typename (first-word constraint))
         (args (rest-words constraint))
         (val (value-part s)))
    (alexandria:switch
     (typename :test #'string=)
     ("boolean"
      (parse-typed-value (if (string= "" val) "false" val)))
     ("number"
      (parse-typed-value (if (string= "" val) "0" val)))
     ("range"
      (let* ((min (parse-number (first args)))
             (max (parse-number (second args)))
             (max (if (and min max) (max min max) max))
             (val (parse-number val))
             (val (or val min max 0)))
        (make-instance 'spec-number :value val :min min :max max)))
     ("min"
      (make-instance 'spec-number
                     :value (parse-number val)
                     :min (parse-number (first args))))
     ("max"
      (make-instance 'spec-number
                     :value (parse-number val)
                     :max (parse-number (first args)))))))

(defun parse-typed-value (s &optional (allow-constraint nil))
  "Returns a duck-typed specialized SPEC-VALUE object denoted by the string S.

ALLOW-CONSTRAINT determines whether strings beginning with #\[ are to be parsed
as constrained types.  (This is useful for a top-level specifying value, not
perhaps for an ordinary value.)"
  (cond ((string= s "")
         (make-instance 'spec-string :value ""))
        ((string= (subseq s 0 1) "\"")
         (make-instance 'spec-string :value (first-quoted-string s)))
        ((string= (string-downcase s) "true")
         (make-instance 'spec-boolean :value t))
        ((string= (string-downcase s) "false")
         (make-instance 'spec-boolean :value nil))
        ((and (string= (subseq s 0 1) "[") allow-constraint)
         (parse-constrained-spec-type s))
        ((number-like-p s)
         (make-instance 'spec-number :value (parse-number s)))
        (t
         (make-instance 'spec-string :value s))))

(defun make-validator (spec-object)
  "Returns a closure of one argument VAL, that validates the value VAL against
the type and constraints of specifier SPEC-OBJECT."
  (lambda (val)
    (if (null val)
        (spec-value spec-object)
        (funcall #'validate spec-object (parse-typed-value val nil)))))

(defun parse-specifier-value (spec-string)
  "Returns a validator closure of the specifier and default value represented by
SPEC-STRING."
  (make-validator (parse-typed-value spec-string t)))

(defun validate-value (spec-string value-string)
  "Returns a valid CL value according to the types and constraints of the
specifier represented by SPEC-STRING and value represented by VALUE-STRING, as
determined by the VALIDATE generic function."
  (funcall (parse-specifier-value spec-string) value-string))

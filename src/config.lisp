(in-package :nz.geek.abhishek.py-configvalidator)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defmacro with-config-errors ((section-name option-name &key (spec nil))
                              &body body)
  (let ((c (gensym)))
    `(handler-case
         (progn ,@body)
       ,@(when section-name
          `((py-configparser:no-section-error (,c)
              (declare (ignore ,c))
              (error ',(if spec 'no-spec-section-error 'no-conf-section-error)
                     :section-name ,section-name))))
       ,@(when option-name
           `((py-configparser:no-option-error (,c)
              (declare (ignore ,c))
              (error ',(if spec 'no-spec-option-error 'no-conf-option-error)
                     :section-name ,section-name
                     :option-name ,option-name)))))))

(defclass validated-config ()
  ((%spec :initarg :spec :initform nil :reader vc-spec)
   (%conf :initarg :conf :initform nil :reader vc-conf)))

(defun get-spec-option (vc section-name option-name
                        &key (expand t) (key #'identity))
  (let ((spec-section (funcall key section-name)))
    (with-config-errors (spec-section option-name :spec t)
      (py-configparser:get-option
       (vc-spec vc) spec-section option-name :expand expand))))

(defun get-conf-option (vc section-name option-name
                        &key (expand t) (default nil default-supplied-p))
  (with-config-errors (section-name option-name)
    (handler-case
        (py-configparser:get-option
         (vc-conf vc) section-name option-name :expand expand)
      (py-configparser:no-section-error (c)
        (declare (ignore c))
        (if default-supplied-p
            default
            (error 'py-configparser:no-section-error)))
      (py-configparser:no-option-error (c)
        (declare (ignore c))
        (if default-supplied-p
            default
            (error 'py-configparser:no-option-error))))))

(defun get-option (vc section-name option-name
                   &key (expand t) (key #'identity))
  (let* ((spec-option (get-spec-option vc section-name option-name
                                       :expand expand
                                       :key key))
         (conf-option (get-conf-option vc section-name option-name
                                       :expand expand
                                       :default spec-option)))
    (validate-value spec-option conf-option)))

(defun spec-items (vc section-name &key (key #'identity) (expand t))
  (let ((spec-section (funcall key section-name)))
    (with-config-errors (spec-section nil :spec t)
      (py-configparser:items (vc-spec vc) spec-section :expand expand))))

(defun conf-items (vc section-name &key (expand t))
  (with-config-errors (section-name nil)
    (py-configparser:items (vc-conf vc) section-name :expand expand)))

(defun items (vc section-name &key (expand t) (key #'identity))
  (let* ((spec-items (spec-items vc section-name :key key :expand expand))
         (conf-items (conf-items vc section-name :expand expand))
         (valid-spec
          (loop :for (k . v) :in spec-items
                :collect (cons k (validate-value v nil))))
         (valid-config
          (loop :for (k . v) :in conf-items
                :for spec := (get-spec-option vc section-name k
                                 :key key :expand nil)
                :collect (cons k (validate-value spec v)))))
    (union valid-spec valid-config :key #'car :test #'string=)))

(defun sections (vc)
  "Returns a list of names of defined conf sections."
  (py-configparser:sections (vc-conf vc)))

(defun print-config (config stream)
  (let ((s (or stream (make-string-output-stream))))
    (loop :for section :in (py-configparser:sections config)
          :do (progn
                (format s "[~a]~%" section)
                (loop :for (opt . val) :in (py-configparser:items config section)
                      :do (format s "  ~a: ~a~%" opt val))))
    (or stream (get-output-stream-string s))))

(defmethod print-object ((o validated-config) s)
  (format s "====SPEC====~%~%~a~%====CONF====~%~%~a~%"
          (print-config (vc-spec o) nil)
          (print-config (vc-conf o) nil)))

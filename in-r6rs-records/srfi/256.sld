(library (srfi :256)
  (export define-record-type)
  (import (rename (rnrs)
                  (define-record-type r6rs:define-record-type)))

  (define *cheat-key* (list '*cheat-key*))
  (define-syntax define-record-type
    (lambda (stx)
      (define (convert-field-spec fstx)
        (syntax-case fstx ()
          ((tag accessor)
           #'(immutable tag accessor))
          ((tag accessor mutator)
           #'(mutable tag accessor mutator))))
      (syntax-case stx ()
        ((_ name (constructor field-tag1 ...) predicate
            (field-tag2 . accessor-info) ...)
         (and (identifier? #'name)
              (for-all bound-identifier=? #'(field-tag1 ...) #'(field-tag2 ...)))
         (with-syntax (((field-spec ...)
                        (map convert-field-spec
                             #'((field-tag2 . accessor-info) ...))))
           #'(r6rs:define-record-type
              (name constructor predicate)
              (fields field-spec ...))))
        ((_ (name parent-name) (constructor _ field-tag1 ...) predicate
            (field-tag2 . accessor-info) ...)
         (with-syntax (((field-spec ...)
                        (map convert-field-spec
                             #'((field-tag2 . accessor-info) ...)))
                       ((constructor-arg ...)
                         (generate-temporaries #'(field-tag1 ...))))
           #'(r6rs:define-record-type
              (name constructor predicate)
              (fields field-spec ...)
              (parent parent-name)
              (protocol
               (lambda (n)
                 (letrec
                     ((cheat
                       (case-lambda
                         ((superinstance constructor-arg ...)
                          (assert (eqv? (record-rtd superinstance)
                                        (record-type-descriptor parent-name)))
                          (cheat *cheat-key* superinstance constructor-arg ...))
                         ((key superinstance constructor-arg ...)
                          (assert (eq? key *cheat-key*))
                          (let* ((parent-rtd (record-type-descriptor parent-name))
                                 (n-fields (vector-length (record-type-field-names parent-rtd))))
                            (let loop ((idx n-fields)
                                       (args '()))
                              (if (= idx 0)
                                  (if (record-type-parent parent-rtd)
                                      ((apply n key superinstance args)
                                       constructor-arg ...)
                                      ((apply n args) constructor-arg ...))
                                  (loop (- idx 1)
                                        (cons ((record-accessor parent-rtd (- idx 1)) superinstance)
                                              args)))))))))
                   cheat))))))))))
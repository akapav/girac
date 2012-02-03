#lang racket

(require "names.rkt")

;;
(struct gir:class (name c:symbol-prefix c:type parent abstract? ctor mtds))
(struct gir:method (name c:identifier retval params))
(struct gir:return-value (ownership c:type))
(struct gir:parameter (c:name ownership c:type))

;;
(define (class-all-methods cls)
  (if (gir:class-abstract? cls)
      (gir:class-mtds cls)
      (cons (gir:class-ctor cls) (gir:class-mtds cls))))

(define (mtd-argument-types mtd)
  (cons (gir:return-value-c:type (gir:method-retval mtd))
	(map gir:parameter-c:type (gir:method-params mtd))))

(define (class-types cls)
  (list->set
   (apply append (map mtd-argument-types (class-all-methods cls)))))
	       
(define (class-typedefs cls-types used-types)
  (let ((new-pointer-types
	 (filter pointer-type?
		 (set->list (set-subtract cls-types used-types)))))
    (values
     (map (lambda (type)
	    (cons (raw-type-name type) (pointer-type-name type)))
	  new-pointer-types)
     new-pointer-types)))

(define (raw-fun-decl mtd)
  `(,(raw-function-name
      (or (gir:method-name mtd) (gir:method-c:identifier mtd)))
    ,(gir:method-c:identifier mtd)
    ,(raw-type-name (gir:return-value-c:type (gir:method-retval mtd)))
    ,(map (lambda (param)
	    (raw-type-name (gir:parameter-c:type param)))
	  (gir:method-params mtd))))

(define (class-raw-funs cls)
  (map raw-fun-decl (class-all-methods cls)))

(define builtin-types
  (list->set '("int" "int*" "gint" "gint" "char" "char*" "gchar" "gchar*")))

;; vala example
(define tc
  (gir:class "TestClass"         ;;name
	     #f                  ;;c:symbol-prefix
	     "TestClass"         ;;c:type
	     "Gobject.Object"    ;;parent (name)
	     #f
	     (gir:method #f "testclass_new"
			 (gir:return-value 'full "TestClass*")
			 (list (gir:parameter "foo" 'none "gint"))) ;;ctor
	     (list
	      (gir:method "method_1" "testclass_method_1"
			  (gir:return-value 'full "gint")
			  '())
	      (gir:method "method_2" "testclass_method_2"
			  (gir:return-value 'full "gint")
			  (list
			   (gir:parameter "oth" 'none "TestClass*"))))))

;; Lg example
(define comp
  (gir:class "Component"
	     "component"
	     "LgComponent"
	     "Gobject.Object"
	     #t
	     #f
	     (list
	      (gir:method "to_string" "lg_component_to_string"
			  (gir:return-value 'none "int")
			  (list (gir:parameter #f 'none "gchar*"))))))

(define btn
  (gir:class "Button"
	     "button"
	     "LgButton"
	     "Component"
	     #f
	     (gir:method #f "lg_button_new"
			 (gir:return-value 'full "LgButton*")
			 '())
	     '()))

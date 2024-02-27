;;; -*- Mode: Lisp -*-

;;;; Fuso Valentina 899972
;;;; Consonni Andrea 900116

;;; ool.lisp

;;; definizione di una variabile globale denominata *classes-specs* 
;;; inizializzata con un' hash-tabel
(defparameter *classes-specs* (make-hash-table))

;;; add-class-spec: aggiunge <name> e <class-spec> nell' hash-tabel
;;; *classes-specs*
(defun add-class-spec (name class-spec)
  (setf (gethash name *classes-specs*) class-spec))

;;; class-spec: recupera e ritorna le specifiche di una classe 
;;; dalla hash-tabel utilizzando <name> come chiave.
(defun class-spec (name)
  (gethash name *classes-specs*))

;;; def-class: definisce la struttura di una classe.
;;; Il valore ritornato e' <class-name> se la classe viene correttamente 
;;; aggiunta nell' hash-table altrimenti viene segnalato un errore. 
(defun def-class (class-name parents &rest parts)
  (let ((superclasses-fields (get-parents-spec
			      (get-all-parents class-name parents)))
	(parts-fields (merge-sublists (search-fields parts))))

    (cond ((null class-name) 
           (error "Class-name is null"))

          ((not(atom class-name)) 
           (error "Class-name invalid"))  

          ((not (null(class-spec class-name))) 
           (error "Class-name has already been defined"))
          
          ((not (listp parents)) 
           (error "You didn't enter the parents as a list"))

          ((not (is-classes parents))
           (error "You didn't enter a valid class in the parents list"))

          ((equal (check-subtype superclasses-fields parts-fields) nil)
           (error "the type of a field cannot be broader
than that of a superclass"))
	  
          (T (add-class-spec class-name (parts-check parents parts)))))
  class-name)


;;; parts-check: verifica la corretta definizione di ogni part 
;;; specificata in <parts>. Si restituisce un errore se cio'
;;; non viene garantito, altrimenti il valore ritornato e' una
;;; lista che contiene la lista di parents e la lista di parts. 
(defun parts-check (parents &rest parts)
  (append (list parents) (mapcar #'part-structure (car parts))))


;;; part-structure: verifica la corretta struttura di una part.
;;; Ogni elemento della lista dei fields viene controllato dalla
;;; funzione field-structure, stessa cosa avviene per la lista 
;;; dei metodi controllati dalla funzione structure-methods. 
;;; Se almeno un elemento di queste liste ha un' errata definizione 
;;; viene restituito un errore, in caso contrario, viene restituita 
;;; una part.
(defun part-structure (part)
  (cond ((not(listp part)) 
         (error "fields and methods must be specified as a list"))
        (T 
         (if (equal (car part) 'fields)
             (append (list 'fields) (mapcar #'field-structure (cdr part)))  
             (if (equal (car part) 'methods)
		 (append (list 'methods)
			 (mapcar #'structure-methods (cdr part)))
		 (error "specify fields or methods at the beginning
of the list"))))))

;;; field-structure: verifica la corretta definizione di un <field>  
;;; Quest'ultimo deve essere una lista e puo' essere
;;; definito in 3 modi differenti:
;;; 1. field-name   nil     T  
;;; 2. field-name   value   T
;;; 3. field-name   value   field-type
;;; Se la definizione risulta corretta viene restituita una lista
;;; contenente il nome del campo,il valore associato al campo
;;; e il tipo, altrimenti viene segnalato un errore.
(defun field-structure (field)
  (let ((field-name (car field))
	(value (cadr field))
	(field-type (caddr field)))
    (cond ((and (listp field)          
		(not (null field-name))
		(symbolp field-name))        
           (cond 
					; caso 1
             ((and (null value)    ;;value null, nil come default
                   (null field-type))  ;;field-type null, T come default
              (list field-name nil T))  
					; caso 2
             ((and (not (null value)) ;;value non nullo
                   (null field-type))   ;;field-type null, T come default
              (list field-name value T))
					; caso 3
             ((and (not (null value))   ;;value non nullo
                   (not (null field-type))  ;;field-type non nullo
                   (type-check field-type)
                   (check-value value field-type))
              (if (consp (eval value))
		  (list field-name (eval value) field-type)
		  (list field-name value field-type)))
					;se la definizione non rispecchia
					;nessuno dei 3 tipi
             (T (error "the value and type of a field are inconsistent"))))
          (T (error "You didn't enter a valid field.
check the field name")))))


;;; type-check: verifica che <field-type> sia un tipo valido,
;;; ovvero un tipo numerico Common Lisp, una stringa 
;;; oppure il nome di una classe definita con def-class.
(defun type-check (type)
  (or (is-class type)
      (equal type 'string)
      (subtypep type 'number)))


;;; check-value: verifica che <value> e <field-type> siano consistenti.
;;; Viene restituito T se <value> e' del tipo specificato, NIL altrimenti.
(defun check-value (value type)
  (or (and (is-class type) (is-instance (eval value) type))
      (typep value type)))


;;; structure-methods: verifica la definizione di un metodo.
;;; Se questa risulta corretta viene restituita una cons-cell 
;;; con il nome del metodo come car e una #<anonymous interpreted function>
;;; come cdr restituita dalla funzione process-method. Se la 
;;; definizione risulta errata viene segnalato un errore.
(defun structure-methods (method)
  (let ((method-name (car method))
	(arglist (cadr method))
	(method-body (caddr method)))
    (cond ((and(listp method)
               (not (null method-name)) 
               (symbolp method-name)
               (listp arglist)) 
	   (list method-name arglist method-body))
	  (T (error "You didn't enter a valid method")))
    (cons method-name (process-method method-name (cdr method)))))


;;; get-all-parents: restituisce una lista contenente tutte 
;;; le superclassi di <class-name>
(defun get-all-parents (class-name &optional direct-parents)
  (if (symbolp class-name)
      (let ((parents (get-parents (car direct-parents))))
	(remove-duplicates (append direct-parents (list (car parents))
				   (mapcan #'get-all-parents parents)
				   (cdr parents))))
      '()))

;;; get-parents-spec: restituisce una lista contenente i 
;;; campi delle classi specificate in <class-list>
(defun get-parents-spec (class-list) 
  (merge-sublists(mapcar #'extract-fields class-list)))         

;;; search-fields: ricerca i campi all' interno di <parts> 
;;; che è una lista di liste. Viene restituita una lista 
;;; contenente tutti i campi specificati in parts togliendo
;;; la parola 'fields.
(defun search-fields (parts)
  (cond
    ((null parts) nil)
    ((and (listp (car parts)) (eql (caar parts) 'fields))
     (cons (cdar parts) (search-fields (cdr parts))))
    (t (search-fields (cdr parts)))))

;;; intersect-parents: restituisce una lista di field-name che
;;; sono presenti sia in <superclasses-fields> che in <class-fields>
(defun intersect-parents (superclasses-fields class-fields)
  (let ((name-f-superclasses (extract-first-elements superclasses-fields))
        (name-f-class (extract-first-elements class-fields)))
    (intersection name-f-superclasses name-f-class :test #'equal)))

;;; is-superclass: restituisce nil se <type-f-part> è 
;;; superclasse di <type-f-class>, T altrimenti.
(defun is-superclass (type-f-part type-f-class)
  (cond ((member type-f-part (recursive-parents type-f-class)) nil)
        (T t)))

;;; check-subtype: restituisce T se tutti i campi di <parts-fields> 
;;; hanno un tipo che è un sottotipo del campo corrispondente in
;;; <superclasses-field>, nil altrimenti. 
(defun check-subtype (superclasses-fields parts-fields)
  (labels ((check (attribute)
             (let* ((type-f-class (caddr (assoc attribute
						superclasses-fields :test
						#'equal)))
                    (value-f-part (cadr (assoc attribute
					       parts-fields :test #'equal)))
                    (type-f-part (caddr (assoc attribute
					       parts-fields :test
					       #'equal))))
               (if (not(is-class type-f-part))
                   (subtypep (type-of value-f-part) type-f-class)
                   (cond ((equal (is-superclass type-f-part
						type-f-class)
				 nil)
			  nil)
                         (T t))))))
    (every #'check (intersect-parents
		    superclasses-fields parts-fields))))


;;; is-class: verificare che <class-name> sia una classe valida ovvero
;;; che sia una classe presente in hash-table. La funzione restituisce
;;; T se questo si verifica, nil altrimenti. 
(defun is-class (class-name)
  (cond ((null class-name) nil) 
	((class-spec class-name)t)
	(T nil)))


;;; is-classes: restituisce T se ogni elemento di <list> 
;;; è una classe valida, nil altrimenti.
(defun is-classes(list)
  (every #'is-class list))


;;; make: consente la creazione di una nuova istanza di <class-name>
;;; Se nessun errore viene segnalato, la funzione ritorna la nuova 
;;; istanza di <class-name> rappresentata come una lista contenente 
;;; la parola oolinst il nome della classe di cui si sta creando 
;;; l' istanza e le varie coppie field-name e value.
(defun make (class-name &rest field-name-value)
  (cond
    ((null class-name) 
     (error "class-name is null"))

    ((not(is-class class-name)) 
     (error "The provided class ~A is not a valid class" class-name))
    
    ((not(check-lista-make field-name-value))  
     (error "you must specify a value for each field"))

    ((not(inheritance-control class-name field-name-value))  
     (error "you cannot create an instance with these fields"))

    ((not(check-type-field class-name field-name-value))
     (error "the value and type of a field are inconsistent or a
     supertype"))
    (T (list 'oolinst class-name field-name-value))))

;;; merge-sublists: unisce tutte le sottoliste di
;;; <main-list> in un'unica lista.       
(defun merge-sublists (main-list)
  (apply #'append main-list))

;;; remove-parents: restituisce una lista dei campi e dei 
;;; metodi di <class-name> prelevandoli dall' hash-table.
(defun remove-parents (class-name)
  (rest(class-spec class-name)))


;;; filter-fields-methods: prende in input <class-name> e una parola
;;; tra 'fields e 'methods. Viene restituita  una lista che
;;; contiene solo gli elementi della lista ritornata da remove-parents
;;; che non iniziano con <fileld-method>. Se la parola specificata 
;;; è 'methods, la lista conterrà solo i campi. Se la parola specificata
;;; è 'fields, la lista conterrà solo i metodi.
(defun filter-fields-methods (class-name field-method)
  (let ((list (remove-parents class-name)))
    (remove-if (lambda (sublist)
		 (and (consp sublist)
                      (eq (first sublist) field-method)))
               list)))

;;; extract-fields: restituisce una lista contenente i campi
;;; di <class-name> togliendo la parola 'FIELDS    
(defun extract-fields (class-name)
  (let ((lista (merge-sublists(filter-fields-methods class-name 'methods))))
    (remove 'fields lista)))


;;; extract-first-elements: preleva i primi elementi delle sottoliste 
;;; contenute in <list>. 
(defun extract-first-elements (list)
  (if list
      (cons (caar list) (extract-first-elements (cdr list)))
      '()))


;;; extract-field-name: permette di associare direttamente 
;;; la funzione "extract-first-elements" al parametro 
;;; <class-name>, restituendo una lista contenente i 
;;; field-name specificati durante la definizione di class-name.
(defun extract-field-name (class-name)
  (let((lista-field (extract-fields class-name)))
    (extract-first-elements lista-field)))


;;; extract-fieldname-instance: estrae gli elementi in posizione dispari
;;; in una lista. Questa funzione viene utilizzata per prelevare 
;;; i field-name dalla lista dei campi specificati dall'istanza.
(defun extract-fieldname-instance (list)
  (if (endp list) nil
      (cons (car list) (extract-fieldname-instance (cddr list)))))


;;; get-parents: restituisce la lista dei parents diretti di 
;;; <class-name> estraendola dall'hash-table.
(defun get-parents (class-name) 
  (car(class-spec class-name)))


;;; recursive-parents: restituisce una lista contenente tutte le
;;; superclassi di <class-name> ; l'ordine con cui vengono visitate
;;; le varie superclassi coincide con l' ordine di una visita
;;; in profondita' del grafo
(defun recursive-parents (class-name)
  (if (is-class class-name)
      (let ((parents (get-parents class-name)))
	(remove-duplicates (append (list (car parents))
				   (mapcan #'recursive-parents parents)
				   (cdr parents))))
      '()))


;;; search-class-list: restituisce una lista il cui primo elemento e'
;;; <class-name> seguito da tutte le sue superclassi. 
;;; Questo elenco facilita la corretta ricerca dei campi 
;;; all'interno del grafo delle superclassi.
(defun search-class-list (class-name)
  (let ((parents-list (recursive-parents class-name)))
    (append (list class-name) parents-list)))


;;; get-parents-field: estende la funzione extract-field-name 
;;; per permetterne l'applicazione a una lista di classi. 
;;; Viene restituita una lista contenente i field-name specificati
;;; nelle classi presenti in <class-list>.
(defun get-parents-field (class-list) 
  (mapcar #'extract-field-name class-list))


;;; check-complete: verifica che tutti gli elementi
;;; nella lista <instance-field> sono presenti in 
;;; una qualsiasi delle sottoliste di <superclasses-field>,
;;; se questo viene garantito la funzione restituisce T,
;;; altrimenti nil.
(defun check-complete (superclasses-field instance-field)
  (every (lambda (element)
           (some (lambda (sublist) (member element sublist :test #'equal))
                 superclasses-field)) instance-field))


;;; inheritance-control: verifica che i campi definiti dall'istanza
;;; siano stati definiti anche nella classe dell'istanza oppure nel 
;;; grafo delle sue superclassi.
(defun inheritance-control (class-name instance-fields)
  (let ((search-list (get-parents-field (search-class-list class-name)))
	(instance-field-name (extract-fieldname-instance instance-fields)))
    (check-complete search-list instance-field-name)))


;;; check-lista-make: verifica la struttura delle coppie field-name
;;; e value specificate nell'istanza. Viene restituito  T se la
;;; struttura risulta corretta oppure se <field-name-value> è nullo.
;;; In tutti gli altri casi viene restituito nil.
(defun check-lista-make (field-name-value)
  (if (null field-name-value)t  
      (and (consp field-name-value)
           (symbolp (car field-name-value))
           (not (null (cadr field-name-value))) 
           (check-lista-make (cddr field-name-value)))))  

;;; intersection-field: restituisce una lista di field-name 
;;; che sono presenti sia in <superclasses-fields> che in 
;;; <instance-fields>
(defun intersection-fields (superclasses-fields instance-fields)
  (let ((fields-class (mapcar #'car superclasses-fields))
        (fields-instance
	 (remove-duplicates
	  (mapcan #'(lambda (elem) 
		      (if (evenp (position elem instance-fields :test
					   #'equal))
			  (list elem) nil))
		  instance-fields)
	  :test #'equal)))
    (intersection fields-class fields-instance :test #'equal)))


;;; check-type-list: verifica che il tipo dei valori dei campi
;;; specificati da un' istanza siano sottotipi dei campi
;;; corrispondenti definiti nella classe dell'istanza o nelle
;;; sue superclassi. 
(defun check-type-list (superclasses-fields instance-fields)
  (labels ((check (attribute)
             (let* ((field-type
		     (caddr(assoc attribute superclasses-fields :test
				  #'equal)))
                    (value-instance
		     (cadr (member attribute instance-fields :test
				   #'equal))))
               (if (not(symbolp value-instance))
                   (subtypep (type-of value-instance) field-type)))))
    (every #'check (intersection-fields superclasses-fields
					instance-fields))))

;;; check-type-field: permette di associare direttamente la funzione
;;; "check-type-list" al parametro <class-name>. Viene restituito
;;; T se tutti i campi di <instance-fields> hanno un tipo che è un
;;; sottotipo di quelli della classe o delle super-classi, nil altrimenti.
(defun check-type-field (class-name instance-list)
  (cond ((inheritance-control class-name instance-list)
	 (check-type-list (get-parents-spec (search-class-list class-name))
			  instance-list))
	(T nil)))

;;; is-instance: restituisce T se l'oggetto passatogli e'
;;; l'istanza di una classe. Se class-name e' T allora value può
;;; essere un' istanza qualunque,altrimenti deve essere un' istanza
;;; di una classe che ha class-name come superclasse.
(defun is-instance (value &optional (class-name T)) 
  (cond ((null value) 
	 (error "value is null"))
        ((and(not(equal class-name 'T))
             (not (is-class class-name))) 
         (error "The provided class ~A is not a valid class" class-name))
        ((and (equal (car value) 'OOLINST) 
              (equal class-name 'T))
	 T) 
        ((equal (cadr value) class-name) T) 
        ((member class-name (recursive-parents (cadr value))) T)))


;;; field: restituisce il valore associato al campo
;;; <field-name> nell'istanza specificata. Se il campo 
;;; non è presente nell'istanza, la ricerca prosegue
;;; nella classe o nelle sue super-classi. Se il campo 
;;; non esiste nella classe dell'istanza (ossia, se non è ereditato),
;;; viene generato un errore.
(defun field (instance field-name)
  (cond ((not(is-instance instance))
         (error "The provided instance ~A is not valid" instance))   
        (T (let ((field-classes 
                  (merge-sublists(get-parents-spec(search-class-list
						   (cadr instance))))))
             (cond ((null(member field-name field-classes :test #'equal))
                    (error "~A is not present in any of
the instance's superclasses" field-name))
                   ((not (null (member field-name
				       (merge-sublists(cddr instance)) :test
				       #'equal)))		    
                    (cadr(member field-name
				 (merge-sublists(cddr instance)) :test
				 #'equal)))		   
                   (T (cadr(member field-name field-classes :test
				   #'equal))))))))


;;; field*: estrae il valore da una classe percorrendo una catena di
;;; attributi. Il valore ritornato e' il valore associato all' ultimo
;;; elemento di <field-names> nell' ultima istanza (tale valore potrebbe
;;; anche essere ereditato dalla classe o da uno dei suoi antenati).
;;; Se uno degli elementi di <field-names> non esiste nella
;;; classe dell' istanza, la funzione segnala un errore.
(defun field* (instance &rest field-names)
  (cond ((null instance)
         (error "instance is null"))

        ((not field-names)
         (error "No field name specified"))

        ((not (is-instance instance))
         (error "You didn't enter a valid instance"))

        (t
         (let ((field-result (field instance (car field-names))))
           (if (cdr field-names)
               (apply #'field* field-result (cdr field-names))
               field-result)))))


;;; extract-methods: restituisce una lista contenente i 
;;; metodi di <class-name> togliendo la parola 'METHODS    
(defun extract-methods (class-name)
  (let ((list (merge-sublists(filter-fields-methods class-name 'fields))))
    (remove 'methods list)))


;;; get-parents-methods: restituisce una lista contenente i
;;; metodi delle classi specificate in <class-list>.
(defun get-parents-methods (class-list) 
  (merge-sublists(mapcar #'extract-methods class-list)))   

;;; extract-value-method: estrae il valore del metodo dalla classe
;;; dell' istanza se presente, altrimenti cerca nel grafo delle
;;; superclassi. Il valore ritornato e' il valore associato a
;;; method-name ovvero una anonymous interpreted function restituita
;;; dalla funzione process-method. Se method-name non esiste nella
;;; classe dell' istanza o nelle superclassi viene segnalato un errore. 
(defun extract-value-method (instance method-name)
  (cond ((not(is-instance instance))
         (error "The provided instance ~A is not valid" instance))
        (T (let ((method-class (get-parents-methods
				(search-class-list (cadr instance)))))
             (cond ((null (assoc method-name method-class :test #'equal))
                    (error "Method ~A is not present in any
instance's superclasses"  method-name))
                   (T (cdr (assoc method-name method-class
				  :test #'equal))))))))


;;; process-method: gestisce la manipolazione dei metodi
;;; Utilizza fdefinition e setf per associare una funzione lambda 
;;; <method-name>. La funzione lambda ha come parametri <this> e <args>
;;; e utilizza apply per chiamare la funzione extract-value-method con 
;;; this e args. Chiama rewrite-method-code per ottenere la specifica
;;; del metodo riscritta e poi la valuta utilizzando eval
(defun process-method (method-name method-spec)
  (setf (fdefinition method-name) 
        (lambda (this &rest args) 
          (apply (extract-value-method this method-name)
		 (append (list this) args))))
  (eval(rewrite-method-code method-spec)))

;;; rewrite-method-code: prende in input una S-expression composta
;;; dalla lista di argomenti e dal corpo del metodo e la riscrive in
;;; maniera tale da ricevere in input anche un parametro this.
(defun rewrite-method-code (method-spec) 
  (cons 'lambda (cons (cons 'this (car method-spec)) (cdr method-spec))))

;;; end-of-file -- ool.lisp
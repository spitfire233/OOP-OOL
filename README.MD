# Autori

Fuso Valentina 899972  
Consonni Andrea 900116

# Introduzione
Il lavoro svolto ha lo scopo di sviluppare un’estensione di Common Lisp  
denominata OOΛ, orientata agli oggetti e caratterizzata dalla presenza di un  
sistema di eredità multipla. La struttura dati utilizzata per immagazzinare le  
classi e le loro specifiche è un hash-table, per permettere un accesso rapido e  
agevole alle caratteristiche di una classe mediante l'uso del suo stesso nome  
come chiave. Questa struttura dati viene assegnata a una variabile globale  
denominata *classes-spec*.  

Vengono fornite 2 funzioni:  

- ```(add-class-spec  <name> <class-spec>)``` che aggiunge il nome e le  
  specifiche di una classe in \*classes-specs\*
- ```(class-spec <name>)``` utilizzata per recuperare le specifiche di una  
  classe dalla hash-table utilizzando il nome della classe come chiave
  
---
## Definizione di una classe

```
(def-class <class-name> <parents> <parts>*) 
```  

Definisce la struttura di una classe, con la verifica preliminare che  
<class-name> sia un atomo non nullo. L'implementazione impone una limitazione  
che impedisce la creazione di una classe con lo stesso nome di una classe  
precedentemente definita, assicurando in tal modo l'unicità dei nomi delle  
classi. In seguito, si verifica che ```<parents>``` costituisca una lista di  
classi valide e successivamente si esegue il controllo dei sottotipi. In  
particolare, si assicura che il tipo di un campo in una sottoclasse non possa  
essere più ampio rispetto a quello definito nelle superclassi corrispondenti. Se  
tutti questi controlli vengono superati con successo, la classe viene aggiunta  
all'hash-table utilizzando la funzione:   

```
(add-class-spec <class-name> <parts-check parents parts>). 
```  

Il valore ritornato dalla funzione def-class è ```<class-name>``` se la classe  
viene  correttamente aggiunta nell’hash-table altrimenti un errore.  

---

```
(parts-check (<parents> <&rest parts>)
```

È una funzione che verifica la corretta definizione di ogni part specificata in  
parts. Si restituisce un errore se ciò non viene garantito, altrimenti il valore  
ritornato è una lista che contiene la lista di parents e la lista di parts.   
Ogni elemento della lista ```<parts>``` viene controllato dalla funzione  
“part-structure”. 

---

```
(part-structure <part>)
```

Verifica la struttura di una part. Quest'ultima deve essere una lista di  
lunghezza variabile contenente la parola fields come primo elemento, seguita  
dalla lista dei campi oppure la parola methods come primo elemento, seguita  
dalla lista dei metodi. Ogni elemento della lista dei fields viene controllato  
dalla funzione field-structure ```<field>```, stessa cosa avviene per la lista  
dei metodi controllati dalla funzione structure-methods ```<method>```.   
Se almeno un elemento di queste liste ha un’errata definizione viene restituito  
un errore, in caso contrario, viene restituita una lista ovvero una part.  

---

```
(field-structure <field>)
```

Verifica la corretta definizione di un field.  
Esso deve essere rappresentato come una lista e puo' essere definito in 3 modi  
differenti. Nel caso più semplice ```<field>``` è solo un simbolo il cui valore  
iniziale è NIL e il tipo è T, nel secondo caso ```<field>``` dispone di un  
field-name un valore e un tipo di default T, infine nel terzo caso ```<field>```   
dispone di un field-name, value e un field-type.  
Per gestire il terzo caso, sono state implementate due funzioni aggiuntive:

```
(type-check <field-type>)
```

Per verificare che <field-type> sia un tipo valido, ovvero un tipo numerico  
Common Lisp, una stringa oppure il nome di una classe definita con def-class  

```
(check-value <value> <field-type>)
```

Per verificare che ```<value>``` e ``` <field-type>``` siano consistenti.  
Viene restituito T se il <value> e' del tipo specificato, NIL, altrimenti.  

Se la definizione risulta corretta viene restituita una lista contenente il nome  
del campo, il valore associato al campo e il tipo, altrimenti viene segnalato un  
errore.  

---

```
(structure-methods <method>)
```

Verifica la corretta definizione di un metodo.   
Esso deve essere rappresentato come una lista, il nome del metodo deve essere un  
simbolo non nullo e gli argomenti devono essere definiti in una lista.  
Se la definizione non dovesse risultare corretta viene segnalato un errore,  
altrimenti viene restituita una cons-cell che ha come car il nome del metodo e  
come cdr una anonymous interpreted function restituita dalla funzione  
process-method.

---

Per garantire che il tipo di un campo in una sottoclasse non sia più ampio  
rispetto a quello definito nelle super-classi corrispondenti, vengono definite 6  
funzioni aggiuntive:  

```
(get-all-parents <class-name> <direct-parents>)
```

Restituisce una lista contenente tutte le superclassi di ```<class-name>```

```
(get-parents-spec <class-list>) 
```

Restituisce una lista contenente i campi delle classi specificate in  
```<class-list>```

```
(search-fields <parts>)
```

Ricerca i campi all' interno di ```<parts>``` che è una lista di liste. Viene  
restituita una lista contenente tutti i campi specificati in parts togliendo la  
parola 'fields.  

```
(intersect-parents <superclasses-fields> <class-fields>)
```

Funzione che restituisce una lista di field-name che sono presenti sia in   
```<superclasses-fields>``` che in ```<class-fields>```  

```
(is-superclass <type-f-part> <type-f-class>)
```

Restituisce nil se ```<type-f-part>``` è superclasse di <type-f-class>, T  
altrimenti.

```
(check-subtype <superclasses-fields> <parts-fields>)
```

È responsabile della verifica del sottotipo dei campi. Questa funzione viene  
richiamata in def-class con il primo parametro <superclasses-fields>  
rappresentante una lista di campi delle super-classi di class-name (parametro di  
def-class) ovvero il valore restituito dalla composizione di funzioni  

```
(get-parents-spec (get-all-parents <class-name> <parents>). 
```

Il secondo parametro di check-subtype è <parts-fields> rappresentante una lista  
contenente tutti i campi specificati in parts (parametro di def-class). 

Check-subtype verifica, tramite la funzione subtypep, che il tipo di un valore  
di un campo ```(type-of value-f-part) ``` specificato in parts sia sottotipo  
dello stesso campo presente in una delle super-classi. Questa verifica viene  
fatta solo peri campi presenti sia in ```<parts-fields>``` che in  
```<superclasses-fields>```.  
Nel contesto di una super-classe con un campo come (university "Berkeley"  
string) e una sottoclasse con un campo corrispondente (university "UNIMIB"),   
si assicura che il valore del campo, ad esempio "UNIMIB", sia conforme al tipo   
specificato nella super-classe se questo non dovesse essere specificato nella  
sotto-classe. In questo modo, se si tentasse di assegnare un valore di tipo  
diverso, come ad esempio un intero, al campo university, il codice segnalerà un  
errore,  garantendo la coerenza dei tipi all'interno della gerarchia di classi.  
La funzione restituisce true se tutti i campi di <parts-fields> hanno un tipo  
che è un sottotipo di quelli della super-classe, nil altrimenti.   
Infatti in def-class, viene generato un errore solo quando la funzione  
```check-subtype``` restituisce nil. Questo si traduce nell'errore "the type of  
a field cannot be broader than that of a superclass".

---

Infine vengono implementate 2 funzioni:

```
(is-class <class-name>)
```

Responsabile di verificare che <class-name> sia una classe valida ovvero che sia  
una classe presente in hash-table. La funzione restituisce T se questo si  
verifica, nil altrimenti.

```
(is-classes <list>)
```

Restituisce T se ogni elemento di ```<list>``` è una classe valida, nil  
altrimenti. Questa funzione viene utilizzata in def-class per verificare che la  
lista dei parents contenga classi valide.  

---

## Creazione di istanze

```
(make <class-name> <&rest field-name-value>)
```

Consente la creazione di una nuova istanza di <class-name>. L'implementazione  
include vincoli che impediscono la creazione di un'istanza nel caso in cui  
<class-name> sia nullo o non rappresenti una classe valida. Errori verranno  
segnalati nei seguenti casi:

  * Se ad un ```<field-name>``` non viene associato alcun valore.
  * Se si tenta di definire un campo per un'istanza, ma tale campo non è  
    definito né nella classe dell'istanza né nelle sue super-classi.  
  * Se il tipo di un valore di un campo ```(type-of value-instance)``` definito  
    da un'istanza non coincide o non è un sottotipo del campo corrispondente  
    presente in una delle super-classi.
	
Se nessun errore viene rilevato durante questi controlli, la funzione ritorna la  
nuova istanza di <class-name> rappresentata come una lista contenente la parola  
oolinst il nome della classe di cui si sta creando l'istanza e le varie coppie   
field-name e value.
 
---

Per implementare i controlli della funzione make sono state definite diverse  
funzioni ausiliarie di seguito elencate:

```
(merge-sublists <main-list>)
```

Unisce tutte le sottoliste di <main-list>  in un'unica lista.

```
(remove-parents <class-name>)
```

Restituisce una lista dei campi e dei metodi di ```<class-name>``` prelevandoli   
dall' hash-table. Si utilizza la funzione rest per escludere il primo elemento  
della lista presente in hash-table che è la lista di parents.  

```
(filter-fields-methods <class-name> <field-method>)
```

La funzione prende in input il nome di una classe e una parola tra 'fields e  
'methods. Viene restituita una lista che contiene solo gli elementi della lista  
ritornata da remove-parents che non iniziano con ```<fileld-method>```.   
Se la parola specificata è 'methods, la lista conterrà solo i campi.   
Se la parola specificata è 'fields, la lista conterrà solo i metodi.  

```
(extract-fields <class-name>)
```

Restituisce una lista contenente i campi di ```<class-name>``` togliendo la  
parola FIELDS. Appiattisce la lista ritornata dalla funzione  
filter-fields-methods specificando ‘methods come parametro e rimuove la parola   
‘fields da questa. 

```
(extract-first-elements <list>)
```

Preleva i primi elementi delle sottoliste contenute in``` <list>```.   
Questa funzione è stata progettata per estrarre il nome del campo, il quale  
coincide sempre con il primo elemento della lista rappresentante quel campo all'  
interno dell'intera lista dei campi.  

```
(extract-field-name <class-name>)
```

Permette di associare direttamente la funzione "extract-first-elements" al  
parametro ``` <class-name>```, restituendo una lista contenente i field-name  
specificati durante la definizione di class-name.  

```
(extract-odd-elements <list> )
```

Ha il compito di estrarre gli elementi in posizione dispari in una lista. Questa  
funzione viene utilizzata per prelevare i field-name dalla lista dei campi  
specificati dall'istanza.

```
(get-parents <class-name>)
```

Restituisce la lista dei parents diretti di ```<class-name>``` estraendola  
dall'hash-table. Utilizza la funzione car per prelevare solo il primo elemento   
della lista presente in hash-table che è appunto la lista dei parents diretti di  
```<class-name>```.   

```
(recursive-parents <class-name>)
```

Restituisce una lista contenente tutte le superclassi di ```<class-name>```;  
l'ordine con cui vengono visitate le varie superclassi coincide con l' ordine di  
una visita in profondita'del grafo. Si utilizza la funzione mapcan per applicare  
la  funzione recursive-parents a ciascuna superclasse ottenuta e appiattire i  
risultati in un'unica lista. La funzione remove-duplicates rimuove eventuali  
duplicati nella lista risultante.  

```
(search-class-list <class-name>)
```

Restituisce una lista il cui primo elemento è ```<class-name>``` seguito da  
tutte le sue super-classi. Questo elenco facilita la corretta ricerca dei campi  
all'interno del grafo delle super-classi.  

```
(get-parents-field <class-list>)
```

Estende la funzione extract-field-name per permetterne l'applicazione a una  
lista di classi. Viene restituita una lista contenente i field-name specificati  
nelle classi presenti in ```<class-list>```.  

```
(check-complete <superclasses-field> <instance-field>)
```

Verifica che tutti gli elementi nella lista <instance-field> sono presenti in  
una qualsiasi delle sottoliste di ```<superclasses-field>```, se questo viene  
garantito la funzione restituisce T, altrimenti nil. Si utilizza la funzione  
every  che restituisce T se la funzione lambda specificata è vera per ogni  
elemento della lista instance-field. La funzione lambda prende un elemento  
element da instance-field. Viene utilizzata la funzione some che restituisce T  
se la funzione lambda specificata è vera per almeno uno degli elementi della  
lista superclasses-field. La funzione lambda interna a some confronta ogni  
sottolista di superclasses-field con l'elemento corrente utilizzando member con  
il test di uguaglianza :test #'equal. Questo test specifica che la funzione  
equal deve essere utilizzata per confrontare gli elementi quando si esegue la  
ricerca con member. La funzione equal confronta gli elementi in modo ricorsivo,   
considerandoli uguali se hanno la stessa struttura e contenuto.

```
(inheritance-control <class-name> <instance-fields>)
```

Verifica che i campi definiti dall'istanza siano stati definiti anche nella  
classe dell'istanza oppure nel grafo delle sue super-classi. Se ciò non avviene,  
la funzione chiamata all'interno della make, genera un errore con il messaggio  
"you cannot create an instance with these fields". Per la verifica utilizza la  
funzione check-complete dichiarata in precedenza con 2 parametri:

  * search-list che contiene tutti i field-name di class-name e delle sue  
    superclassi.
  * Instance-field-name che contiene tutti i field-name specificati durante la  
    creazione dell’istanza

```
(check-lista-make <field-name-value>)
```

Verifica la correttezza delle coppie field-name e value specificate  
nell'istanza. In particolare si controlla che la chiave sia un simbolo e che il   
valore non sia nullo. La funzione restituisce T se queste 2 condizioni sono  
soddisfatte oppure se ```<field-name-value>``` è nullo perché è possibile  
definire un’istanza senza ridefinire alcun campo. In tutti gli altri casi viene  
restituito nil.

```
(intersection-fields <superclasses-fields> <instance-fields>)
```

restituisce una lista di field-name che sono presenti sia in  
```<superclasses-fields>``` che in ```<instance-fields>```. 

  * fields-class: è una lista contenete i primi elementi di ogni sottolista in   
    superclasses-fields ovvero i field-name, estratti con la funzione mapcar  
    #'car.  
  * fields-instance: è una lista che contiene i field-name presenti  
    nell'istanza. La funzione lambda verifica se l'indice di un elemento in  
    instance-fields è pari (utilizzando evenp) e se lo è, aggiunge l'elemento  
    alla lista. La lista dei campi nella funzione make è composta da coppie  
    "field-name value", quindi è garantito che il nome del campo (field-name) si  
    troverà sempre nelle posizioni degli indici pari nella lista.  

```
(check-type-list <superclasses-fields> <instance-fields>)
```

Verifica che il tipo dei valori dei campi specificati da un' istanza siano  
sottotipi dei campi corrispondenti definiti nella classe dell'istanza o nelle  
sue super-classi.  

  * field-type: contiene il terzo elemento della sottolista associata alla  
    chiave attribute nella lista superclasses-fields. Quindi per l’appunto il  
    tipo di un campo.
  * value-instance: contiene il secondo elemento della sottolista associata alla  
    chiave attribute nella lista instance-fields. Quindi il valore associato a  
    un campo nella lista dei campi specificata durante la creazione  
    dell’istanza.
	
```
(check-type-field <class-name> <instance-list>)
```

Permette di associare direttamente la funzione " check-type-list " al parametro  
<class-name>. La funzione restituisce true se tutti i campi di  
```<instance-fields>``` hanno un tipo che è un sottotipo di quelli della classe  
o delle super-classi, nil altrimenti. Nella funzione make, viene generato un  
errore quando la funzione ` check-type-field ` restituisce `nil`. Questo si  
traduce nell'errore " the value and type of a field are inconsistent or a  
supertype ".

---

## Ricerca dei campi

Per la ricerca dei campi vengono implementate 3 funzioni:

```
(is-instance <value> <&optional (class-name T)>)
```

Restituisce T se ```<value>``` e’ un'istanza di una classe. Se il parametro  
opzionale ``` <class-name>``` è T, allora <value> può essere un'istanza di  
qualsiasi classe. Altrimenti,```<value>``` deve essere un'istanza di una classe  
che ha class-name come superclasse. La funzione genera errori se ```<value>``` è  
un valore nullo, e se viene specificata una classe non valida.  

```
(field <instance> <field-name>)
```

Restituisce il valore associato al campo ```<field-name>``` nell'istanza  
specificata. Se il campo non è presente nell'istanza, la ricerca prosegue nella  
classe o nelle sue super-classi. Se il campo non esiste nella classe  
dell'istanza (ossia, se non è ereditato), viene generato un errore con il  
messaggio “<field-name> is not present in any of the instance's superclasses”.   
Viene segnalato errore anche quando ```<instance>``` non è un’istanza valida.  

```
(field* <instance> <&rest field-names>)
```

Estrae il valore da una classe percorrendo una catena di attributi. Il valore  
ritornato e' il valore associato all' ultimo elemento di ```<field-names>```
nell' ultima istanza (tale valore potrebbe anche essere ereditato dalla classe o  
da una delle sue superclassi). Se uno degli elementi di field-names non esiste  
nella classe dell'istanza, la funzione segnala un errore.  

---

## Manipolazione dei metodi

Per la manipolazione dei metodi vengono implementate 5 funzioni: 

```
(extract-methods <class-name>)
```

Restituisce una lista contenente i metodi di ```<class-name>``` togliendo la  
parola ‘METHODS

```
(get-parents-methods <class-list>)
```

Restituisce una lista contenente i metodi delle classi specificate in  
 ```<class-list>```. La funzione viene impiegata per accedere ai metodi delle  
 super-classi di ```<class-name>``` al fine di gestire l'ereditarietà dei  
 metodi.
 
```
(extract-value-method <instance> <method-name>)
```

Restituisce il valore associato a ```<method-name>``` nella classe dell'istanza  
specificata. Se il metodo non è presente nella classe, la ricerca prosegue nelle  
sue super-classi. Se il campo non esiste nella classe dell'istanza (o se non è  
ereditato), viene generato un errore con il messaggio   
“<method-name> is not present in any instance's superclasses".   

```
(process-method <method-name> <method-spec>)
```

Gestisce manipolazione dei metodi. Utilizza fdefinition e setf per associare una  
funzione lambda al nome del metodo. Questa funzione lambda ha come parametri  
this e altri argomenti del metodo e utilizza apply per chiamare la funzione  
extract-value-method con this e args. Chiama “rewrite-method-code” per ottenere  
la specifica del metodo riscritta e poi la valuta utilizzando ‘eval.

```
(rewrite-method-code <method-spec>)
```

Prende in input una S-expression composta dalla lista di argomenti e dal corpo  
del metodo e la riscrive in maniera tale da ricevere in input anche un parametro  
this.  


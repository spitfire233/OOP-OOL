# Introduzione

Il lavoro svolo ha lo scopo di sviluppare un'estensione di Prolog denominata  
OOΠ, orientata agli oggetti e caratterizzata dalla presenza di un sistema di  
ereditarieta' multipla.

---

## Predicati dinamici e scelte implementative

I seguenti predicati sono stati dichiarati dinamici per permetterne  
l'asserzione, all'interno della base della conoscenza di Prolog, anche a  
runtime. Essi, inoltre, rappresentano le scelte implementative fatte per  
rappresentare i vari termini del linguaggio:

```
:- dynamic class/3
```

Questo predicato dinamico rappresenta una classe del linguaggio. Una classe  
e' quindi rappresentata da un termine con la seguente sintassi:  

```
class(Class_name, Parents, Parts)
```

Con:

  * ```<Class_name>``` Il nome della classe  
  * ```<Parents>``` La lista delle superclassi "dirette" della classe  
  * ```<Parts>``` La lista dei campi e dei metodi della classe.
  
```:- dynamic inst_ref/2 ```

Questo predicato dinamico rappresenta un riferimento ad un'istanza di una  
classe del linguaggio. Un riferimento e' quindi rappresentato da un termine  
con la seguente sintassi:

``` inst_ref(<Instance_name>, instance(Class, Inst_fields))```

Con:

  * ```<Instance_name>``` Il nome associato all'istanza
  * ```<Class>``` La classe dell'istanza
  * ```<Inst_fields>``` La lista dei campi della classe "sovrascritti
    localmente" dall'istanza
	
```:- dynamic type/1```

Questo predicato dinamico rappresenta un tipo. Poiche' in prolog non esiste  
un sistema di tipi esplicito, si e' deciso di implementare questo predicato  
in modo da rendere le operazioni sui tipi piu' semplici. Un tipo e'  
rappresentato da un termine con la seguente sintassi:  

```type(<Name>)```

Con ```<Name>``` il nome del tipo.

``` :- dynamic subtype/2```

Questo predicato dinamico rappresenta una relazione di sottotipo. Poiche' in  
Prolog non esiste una relazione esplicita tra i vari tipi del linguaggio, si  
e' deciso di implementare questo predicato in modo da rendere le operazioni e  
i controlli sui tipi piu' semplici. Una relazione di sottotipo e'  
rappresentata da un termine con la seguente sintassi:

``` subtype(<Super_type>, <Sub_type>)```

Con ```<Super_type>``` il super-tipo e ```<Sub_type>``` il sottotipo

---
## Primitive del linguaggio

## def_class/2

``` def_class(Class_name, Parents) ```

Questo predicato effettua una chiamata a **def_class/3** assumendo che la  
sua lista ```<Parts>``` sia vuota.

## def_class/3

```def_class(Class_name, Parents, Parts)```

Questo predicato si occupa del caricamento, sulla base della conoscenza  
del sistema Prolog, della definizione di una classe. Il predicato si  
appoggia su altri predicati per effettuare tutti i controlli necessari  
a garantire la correttezza della definizione. In particolare:

```<Class_name>``` deve essere un simbolo  
``` <Parents>``` deve essere una lista di classi (esistenti) da cui  
	deriva la classe
``` <Parts>``` deve essere una lista di campi correttamente definiti.  

I campi di una classe devono avere la seguente sintassi:  

```field(Field_name, Value)``` oppure  
```field(Field_name, Value, Type)``` oppure  
```method(Method_name, Arglist, Body)```  
	
Con:

* ```<Field_name>``` e ```<Method_name>``` degli atomi
* ``` <Value> ``` Un qualsiasi valore, conforme a ```<Type>```  
	  nel caso il campo sia tipizzato
* ```<Args>``` una lista che rappresenta gli argomenti del metodo
* ```<Body>``` una qualsiasi congiunzione di termini e predicati  
	 Prolog
	 
Infine, ogni volta che si asserisce una nuova classe, si asserisce anche un  
nuovo tipo e anche dei nuovi sottotipi, dipendentemente se la classe e'  
derivata o meno.

## make/2

```make(Instance_name, Class_name)```

Il predicato chiama a sua volta il predicato make/3 assumendo che la  
lista <Istance_fields> sia vuota.

## make/3

```make(Instance_name, Class_name, Instance_fields)```

Il predicato si occupa della definizione di istanze di classe e di  
riferimenti ad esse all'interno della base di conoscenza di Prolog. Il  
predicato si appoggia su altri predicati per effettuare tutti i  
controlli necessari a garantire la correttezza della definizione  
dell'istanza. Il predicato distingue diversi casi:

  * ```<Instance_name>``` e' un simbolo o una variabile istanziata con un  
	   simbolo: Si distinguono 2 sottocasi:  
	   
	   1) ```<Instance_name>``` e' gia' presente nella base della conoscenza;  
		    quindi si procede alla ridefinizione dell'istanza ad esso  
		    associata; la ridefinizione e' ammessa solo se la nuova istanza e'  
 		    istanza della stessa classe o di una sottoclasse della classe  
		    originale.  
			
       2) ```<Instance_name>``` non e' gia' presente nella base della  
          conoscenza, quindi si procede alla definizione di un nuovo  
          riferimento all'istanza descritta da ```<Class_name>``` e  
          ```<Instance_fields>```.
  * ```<Instance_name>``` e' una variabile logica; quindi viene unificata  
	con il termine che rappresenta l'istanza appena creata.
  * ```<Instance_name>``` e' un termine che unifica con la rappresentazione  
    interna di un'istanza.
	
## is_class/1

``` is_class(Class_name) ```

Il predicato controlla l'esistenza di una classe denominata  
```<Class_name>```. Se esiste, il predicato ha successo, altrimenti  
   fallisce.
   
## is_instance/1:

``` is_instance(Value)```

Il predicato controlla se ```<Value>``` e' un'instanza di una classe   
qualsiasi. Se e' un'istanza, il predicato ha successo, altrimenti  
fallisce. Si distinguono due casi in cui il predicato ha successo:  

  * ```<Value>``` e' una reference ad un'istanza; questo caso e' stato  
	implementato per permettere le chiamate a metodi e ad altri predicati  
	anche solo con il nome di una reference ad un'istanza  
  * ```<Value>``` e' un'istanza di classe  
  
## is_instance/2:

```is_instance(Value, Class)```

Il predicato controlla se ```<Value>``` e' un'instanza di una classe  
```<Class>``` o di una sua sottoclasse. Se e' un'istanza, il predicato ha  
successo, altrimenti fallisce. Si distinguono due casi in cui il predicato  
ha successo:  


  * ```<Value>``` e' una reference ad un'istanza di ```<Class_name>```;   
    o di una sua sottoclasse; questo caso e' stato implementato per  
    permettere le chiamate a metodi e ad altri predicati anche solo con il  
    nome di una reference ad un'istanza  
  * ```<Value>``` e' un'istanza della classe ```<Class_name>``` o di una  
	sua sottoclasse
	
## inst/2:

```inst(Instance_name, Instance)```

Il predicato recupera un'istanza dalla base di conoscenza Prolog con  
nome associato ```<Istance_name>``` e la unifica con ```<Instance>```

## field/3:

```field(Inst_name, Field_name, Result)```

Il predicato si occupa di ritornare il valore di un campo ```<Field_name>```  
definito nell'istanza ```<Instance>``` o nella classe dell'istanza. Il  
risultato viene unificato con ```<Result>```. Si distinguono i seguenti  
casi:  

   * ```<Instance>``` e' il nome della reference ad un'instanza, questo caso  
     e' stato implementato per consentire la chiamata a field anche con il  
     solo nome associato all'istanza
   * Il campo e' stato sovrascritto nell'istanza, quindi ```<Result>``` verra'  
     unificato con il valore presente nell'istanza
   * Il campo deve essere ricercato nelle classi; quindi ```<Result>```  
     verra' unificato con il valore presente nelle classe dell'istanza o  
     nelle sue superclassi

## fieldx/3:

```fieldx(Instance, Fields, Result)```

Il predicato estrae il valore da una classe percorrendo una catena di  
attributi. In particolare, tutti gli attributi intermedi dovranno essere a  
loro volta istanze per permettere che il predicato possa estrarre il valore  
dell'ultimo attributo, il quale verra' unificato con ```<Result>```

---

## Predicati di controllo

Questi predicati sono stati implementati per effettuare i  
controlli necessari a garantire la correttezza della base della  
conoscenza quando vengono usate le primitive del linguaggio.  

## parents\_exist/1

```parents_exist(Parents)```

Il predicato controlla ricorsivamente se tutti gli elementi della  
lista ```<Parents>``` passata sono nomi di classi presenti nel sistema  
Prolog. Il predicato ha successo se tutti gli elementi della lista sono nomi  
di classi; altrimenti fallisce.  

## parse\_class\_fields/3

Il predicato si occupa di effettuare il parsing della lista  
```<Parts>``` di una classe. Il predicato distingue diversi casi, su cui  
effettua diversi controlli:

  * Il termine considerato e' del tipo:  
    ```field(Field_name, Value)```  
	In questo caso, il predicato si occupa di controllare se esiste un  
	altro campo di nome ```<Field_name>``` nelle superclassi e di ereditarne  
    il tipo ```<Type>```. Dopodiche', il predicato controlla se <Value> e'  
	conforme con ```<Type>```  
  * Il termine considerato e' del tipo:  
    ```field(Field_name, Value, Type).```  
	In questo caso, il predicato si occupa di controllare solo se il  
    valore ```<Value>``` e' conforma a ```<Type>``` e che quest'utlimo sia  
    conforme ai tipi nelle superclassi.

## inherited\_type\_check/3

```inhertied_type_check(Field_name, Type, Parents)```

Il predicato controlla che il tipo ```<Type>``` sia uguale o un sottotipo  
del tipo dichiarato per ```<Field_name>``` nelle superclassi ```<Parents>```

## conflict_check/2

```conflict_check(Template, List)```

Il predicato controlla che non vi siano altri elementi del tipo  
```<Template>``` all'interno della lista ```<List>```

## pasrse\_field\_value/2

``` parse_field_value(Field, New_field)```

Il predicato effettua il parsing di ```<Value>``` di un termine del  
tipo: ```field(Field_name, Value)``` oppure  
      ```field(Field_name, Value, Type)```  

In particolare, il predicato controlla che ```<Value>``` sia di tipo  
```<Type>``` e unifica ```<New_field>``` con un un nuovo predicato del tipo:  

 ```field(Field_name, Inst_name)``` oppure  
 ```field(Field_name, Inst_name, Type)```  
 
 se ```<Value>``` e' del tipo   
 ```make(Inst_name, Class_name)``` oppure  
 ```make(Inst_name, Class_name, Inst_fields)```
 
 Il predicato fallisce se ```<Value>``` non e' di tipo ```<Type>```
 
 
## type_check/2

``` type_check(Type_1, Type_2)```

Il predicato effettua un controllo sui tipi passati. Esso ha successo  
sse i due tipi sono identici oppure se ```<Type_2>``` e' sottotipo di  
```<Type_1>```. Il predicato supporta tutti i tipi espressi dai termini  
```type/1``` e ```subtype/2```.

## value\_type\_check/2

``` value_type_check(Value, Type)```

Il predicato controlla che ```<Value>``` sia un valore di tipo ```<Type>```  
oppure che sia un valore di un sottotipo di ```<Type>``` . Si distinguono  
due casi particolari:  
1) Qualsiasi ```<Value>``` e' di tipo any  
2) ```<nil>``` rappresenta un valore "null" per una reference ad un'istanza  
   di una classe.
   
## parse\_instance\_fields/3:

``` parse_instance_fields(Fields, Class_name, New_fields)```

Il predicato effettua ricorsivamente il parsing dei campi dichiarati in  
un'istanza del tipo:  
```<Field_name> = <Value>```  
In particolare, il predicato controlla che ```<Field_name>``` sia presente  
nella classe ```<Class_name>``` o nelle sue superclassi, che ```<Value>```  
sia conforme al tipo dichiarato per ```<Field_name>``` ed esegue, dopo aver  
fatto gli adeguati controlli, l'eventuale ```make``` e sostituisce <Value>  
con ```<Inst_name>```; in caso non vi sia una ```make```, ```<Value>```  
rimane inalterato. I campi controllati vengono messi nella lista  
```<New_fields>```.

## is_reference/1:

```is_reference(<Value)```

Il predicato controlla se ```<Value>``` e' il nome di un riferimento ad  
un'istanza; in questo caso ha sucesso, altrimenti fallisce.  

## is_reference/2:

```is_reference(Value, Class)```

Il predicato controlla se ```<Value>``` e' il nome di un riferimento ad  
un'istanza di una certa classe o di una sottoclasse di quella classe; in  
questo caso il predicato ha successo, altrimenti fallisce.  

---

## Altri predicati

Questi predicati sono stati implementati per "astrarre" delle  
operazioni che i vari predicati implementati svolgono  
ripetutamente per evitare l'implementazione di codice duplicate o  
per  "spostare" queste operazioni in modo da migliorare la  
leggibilita' del codice.  

## separate_parts/3

```separate_parts(Parts. Fields, Methods)```

Il predicato separa i termini del tipo:   
```field(Field_name, Value)```  
```field(Field_name, Value, Type)```  
dai termini del tipo:  
```method(Method_name, Arglist, Body)```  
E li inserisce rispettivamente nelle liste ```<Fields>``` e ```<Methods>```

## filter_list/3:

```filter_list(Template, List, Result)```

Il predicato filtra ricorsivamente ```<List>```, restituendo una  
lista ```<Result>``` di tutti gli elementi che unificano con   
```<Template>```

## eliminate_duplicates/2

``` eliminate_duplicates(List, New_list)```

Il predicato elimina ricorsivamente i duplicati all'interno di una  
lista e unifica il 2 argomento con la lista priva di duplicati.  

## inherit_type/3

```inherit_type(Field, Parents, Typed_field)```

Il predicato si occupa di convertire i termini di tipo:  
```field(Field_name, Value)``` in   
```field(Field_name, Value, Type)```  
ereditando, se presente,``` <Type>``` dal campo delle superclassi con nome  
```<Field_name>```. Il risultato viene unificato con ```<Typed_field>```

## get_field/3:

```get_field(Field_name, Parts, Field)```

Il predicato estrae un determinato campo con nome ```<Field_Name>```  
dalla lista ```<Parts>``` di una classe. Il predicato ha successo se il  
campo viene trovato, altrimenti fallisce. Se il predicato ha successo,  
```<Field>``` viene unificato con il campo.

## field_value/2:

```field_value(Field, Value)```

Il predicato si occupa di estrarre dai termini del tipo:  
```field(Field_name, Value)```  
```field(Field_name, Value, Type)```  
Il campo <Value>

## classes\_dfs\_next/2:

```classes_dfs_next(Parents, New_order)```

Il predicato ritorna il prossimo ordine di attraversamento in  
profondita' del grafo delle superclassi nella lista ```<New_order>```

## load_methods/3:

```load_methods(Methods, Class_name, Parents)```

Il predicato si occupa di caricare ricorsivamente nella base della  
conoscenza di Prolog i metodi presenti all'interno della lista  
```<Methods>```; ```<Class_name>``` e' il nome della classe nella quale e'  
contenuto il metodo, mentre ```<Parents>``` sono i suoi genitori diretti;  
essi vengono passati al fine di controllare e implementare l'override dei  
metodi.

## get_predicates/2:

```get_predicates(Compound, Predicates)```

Il predicato scompone il termine ```<Compound> in una lista di
predicati ```<Predicates>```.

## arg\_to\_var/4:

```arg_to_var(Predicates, Arg, Var, Parsed_predicates)```

Il predicato si occupa di sostitutire il termine ```<Arg>``` nei predicati  
della lista ```<Predicates>``` con la variabile logica ```<Var>```.   
Il risultato viene unificato con la lista ```<Parsed_predicates>```. Il  
predicato tratta anche i casi nel quale il predicato analizzato e' formato a  
sua volta da un termine "compound".

## construct_compound/3

```construct_compound(Terms, Compound)```

Il predicato effettua la costruzione di un unico termine compound  
formato dai vari predicati all'interno della lista.  

## replace\_in\_list/4:

```replace_in_list(List, Item, Replacement, Result)```

Il predicato sostituisce tutte le occorrenze di ```<Item>``` nella lista  
```<List>``` con ```<Replacement>```. Il risultato e' una lista unificata in  
```<Result>```.

## term\_list\_to\_atom/3:

```term_list_to_atom(Terms, Result) :- term_list_to_atom(Terms, [], Result)```

```term_list_to_atom(Terms, Atm_results, Result)```

Il predicato concatena in un solo atomo tutti i termini presenti nella  
lista. Il risultato viene unficato con ```<Result>```. Il predicato si  
avvale di una lista ```<Buffer>```  dove vengono immessi i risultati  
parziali.

## load_method/4

```load_method(Method, Method_name, Arglist, Parents)```

Il predicato asserisce ```<Method>``` in modo che rispetti l'ereditarieta' in  
profondita' del linguaggio anche per l'override dei metodi. Tutti gli altri  
parametri passati sono necessari affinche' il metodo venga asserito nella  
posizione corretta.

## field\_type\_in_class/3:

```field_type_in_class(Class, Field_name, Result)```

Il predicato unifica ```<Result>``` con il valore di ```<Type>``` di un  
campo all'interno di una classe. Se ```<Type>``` non e' presente, lo cerca  
nelle superclassi.

## field_type/2:

```field_type(Field, Type)```

Il predicato unifica ```<Type>``` con il valore del tipo ```<Field>```  
passato.

## field\_value\_in\_class/3:

```field_value_in_class(Class, Field_name, Result)```

Il predicato unifica ```<Result>``` con il valore di ```<Field_value>```  
all'interno di ```<Class>```. Se ```<Field_value>``` non e' presente, lo  
cerca nelle superclassi.

---

## Sistema dei tipi

Questi termini indicano che l'argomento e' un tipo. Essi sono stati  
implementati per permettere al programma di avere e mantenere una  
dichiarazione esplicita di tutti i tipi presenti. Ogni qualvolta  
si dichiara una nuova classe, si dichiara anche un nuovo tipo.  

## type/1:

```type(Type)```

Questi termini indicano che l'argomento e' un tipo. Essi sono stati  
implementati per permettere al programma di avere e mantenere una  
dichiarazione esplicita di tutti i tipi presenti. Ogni qualvolta  
si dichiara una nuova classe, si dichiara anche un nuovo tipo.

## subtype/2:

```subtype(Supertype, Subtype)```

Questi termini sono stati implementati per permettere al programma  
di mantnere una chiara gerarchia dei sottotipi. Essi rappresentano  
%una relazione di sottotipo tra il 2 argomento (```<Subtype>```) e il 1  
argomento (```<Supertype>```). La relazione e' transitiva ed essa e'  
implementata tramite il predicato **is_subtype/2**. Ogni qualvolsta si  
dichiara una nuova sottoclasse di una o piu' classi, si asserisce  
un nuovo sottotipo di tutte le sue classi genitori.  

## assert_subtypes/2:

```assert_subtypes(Supertypes, Subtype)```

Il predicato asserisce ricorsivamente nuovi sottotipi della forma:  
```subtype(<Supertype>, <Subtype>)``` da una lista di supertipi  
```<Supertypes>```.


## get\_value\_type/2:

```get_value_type(Type, Value)```

Il predicato ritorna il tipo di un certo valore. Sono supportati  
tutti i tipi espressi da **type/1**.


---

## Errori conosciuti

L'ovverride dei metodi avviene solamente con l'ultimo appena definito e non  
in profondita'.

---


  





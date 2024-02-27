%%% -*- Mode: Prolog -*-

%%% oop.pl

%%% PREDICATI DINAMICI:
%%% I seguenti predicati/termini sono stati dichiarati dinamici per
%%% permetterne l'asserzione, all'interno della base della conoscenza
%%% di Prolog, anche a runtime.

:- dynamic class/3.

:- dynamic inst_ref/2.

:- dynamic type/1.

:- dynamic subtype/2.

%%% PRIMITIVE DEL LINGUAGGIO:

%%% def_class

%%% def_class/2:
%%% Questo predicato chiama a sua volta il predicato def_class/3
%%% assumendo che la lista <Parts> sia vuota.

def_class(Class_name, Parents) :-
    def_class(Class_name, Parents, []).

%%% def_class/3:
%%% Questo predicato si occupa del caricamento, sulla base della
%%% conoscenza del sistema Prolog, della definizione di una
%%% classe. Una classe è definita dal termine:
%%% class(Class_name, Parents, Type).
%%% Il predicato si appoggia su altri predicati per effettuare tutti i
%%% controlli necessari a garantire la correttezza della definizione
%%% della classe.

def_class(Class_name, Parents, Parts) :-
    atom(Class_name),
    findall(Class_name, is_class(Class_name), []),
    conflict_check(Class_name, Parents),
    parents_exist(Parents),
    is_list(Parts),
    separate_parts(Parts, Fields, Methods),
    parse_class_fields(Fields, Parents, New_fields),
    load_methods(Methods, Class_name, Parents),
    append(New_fields, Methods, New_parts),
    assertz(class(Class_name, Parents, New_parts)),
    assertz(type(Class_name)),
    assert_subtypes(Parents, Class_name), !.

%%% make

%%% make/2:
%%% Il predicato chiama a sua volta il predicato make/3 assumendo che la
%%% lista <Istance_fields> sia vuota.

make(Istance_name, Class_name) :-
    make(Istance_name, Class_name, []).

%%% make/3:
%%% Il predicato si occupa della definizione di istanze di classe e di
%%% riferimenti ad esse all'interno della base di conoscenza di Prolog. Il
%%% predicato si appoggia su altri predicati per effettuare tutti i
%%% controlli necessari a garantire la correttezza della definizione
%%% dell'istanza. Il predicato distingue diversi casi:

%%% Caso 1: <Instance_name> è un simbolo o una variabile istanziata con un
%%% simbolo: Si distinguono 2 sottocasi:

%%% 1) <Instance_name> è già presente nella base della conoscenza; quindi si
%%% procede alla ridefinizione dell'istanza ad esso associata; la
%%% ridefinizione è ammessa solo se la nuova istanza è istanza della stessa
%%% classe o di una sottoclasse della classe originale:

make(Instance_name, Class_name, Instance_fields) :-
    atom(Instance_name),
    inst(Instance_name, instance(Inst_class, _)), !,
    is_class(Class_name),
    type_check(Inst_class, Class_name),
    is_list(Instance_fields),
    parse_instance_fields(Instance_fields, Class_name, New_fields),
    retract(inst_ref(Instance_name, _)),
    Redifined_instance =.. [inst_ref, Instance_name, instance(Class_name,
							      New_fields)],
    assertz(Redifined_instance), !.

%%% 2) <Instance_name> non è già presente nella base della conoscenza,
%%% quindi si procede alla definizione di un nuovo riferimento all'istanza
%%% descritta da <Class_name> e <Instance_fields>.

make(Instance_name, Class_name, Instance_fields) :-
    atom(Instance_name),
    findall(Instance_name, is_reference(Instance_name), []), !,
    is_class(Class_name),
    is_list(Instance_fields),
    parse_instance_fields(Instance_fields, Class_name, New_fields),
    assertz(inst_ref(Instance_name, instance(Class_name, New_fields))), !.

%%% Caso 2: <Instance_name> è una variabile logica

make(Instance_name, Class_name, Instance_fields) :-
    var(Instance_name), !,
    is_class(Class_name),
    parse_instance_fields(Instance_fields, Class_name, New_fields),
    Instance_name = instance(Class_name, New_fields), !.

%%% Caso 3: <Instance_name> è un termine che unifica con la rappresentazione
%%% interna di un'istanza.

make(Instance_name, Class_name, Instance_fields) :-
    compound(Instance_name),
    is_class(Class_name),
    is_list(Instance_fields),
    parse_instance_fields(Instance_fields, Class_name, New_fields),
    Instance_name =.. [instance, Class_name, New_fields], !.

%%% is_class

%%% is_class/1:
%%% Il predicato controlla l'esistenza di una classe denominata
%%% <Class_name>. Se esiste, il predicato ha successo, altrimenti fallisce.

is_class(Class_name) :-
    atom(Class_name),
    class(Class_name, _, _), !.

%%% is_instance

%%% is_instance/1:
%%% Il predicato controlla se <Value> è un'instanza di una classe
%%% qualsiasi. Se è un'istanza, il predicato ha successo, altrimenti
%%% fallisce. Si distinguono due casi:

%%% Caso 1: <Value> è una reference ad un'istanza

is_istance(Value) :- is_reference(Value), !.

%%% Caso 2: <Value> è un'istanza di classe

is_instance(instance(_, _)) :- !.

%%% is_instance/2:
%%% Il predicato controlla se <Value> è un'istanza di una classe che ha come
%%% classe o come superclasse <Class_name>. Si distinguono due casi:

%%% Caso 1: <Value> è una reference ad un'istanza:

is_instance(Value, Class_name) :-
    is_reference(Value, Class_name), !,
    inst_ref(Value, instance(Inst_class, _)),
    is_class(Class_name),
    is_class(Inst_class),
    type_check(Class_name, Inst_class), !.

%%% Caso 2: <Value> è un'istanza di classe:

is_instance(instance(Inst_class, _), Class_name) :-
    is_class(Class_name),
    is_class(Inst_class),
    type_check(Class_name, Inst_class), !.

%%% inst

%%% inst/2:
%%% Il predicato recupera un'istanza dalla base di conoscenza Prolog con
%%% nome <Istance_name> e la unifica con <Instance>

inst(Instance_name, Instance) :-
    is_reference(Instance_name),
    inst_ref(Instance_name, Instance).

%%% field

%%% field/3:
%%% Il predicato si occupa di ritornare il valore di un campo <Field_name>
%%% definito nell'istanza <Instance> o nella classe dell'istanza. Il
%%% risultato viene unificato con Result. Si distinguono i seguenti casi:

%%% Caso 1: <Instance> è il nome della reference ad un'instanza

field(Inst_name, Field_name, Result) :-
    atom(Inst_name),
    inst(Inst_name, Istance),
    field(Istance, Field_name, Result).

%%% Caso 2: Il campo è stato sovrascritto nell'istanza:

field(instance(_, Instance_fields), Field_name, Result) :-
    atom(Field_name),
    filter_list(Field_name = Result, Instance_fields,
		[Field_name = Result]),
    !.

%%% Caso 3: Il campo deve essere ricercato nelle classi:

field(instance(Class, _), Field_name, Result) :-
    field_value_in_class(Class, Field_name, Result),
    !.

%%% fieldx

%%% fieldx/3:
%%% Il predicato estrae il valore da una classe percorrendo una catena di
%%% attributi.

fieldx(Instance, [Field_name], Result) :-
    field(Instance, Field_name, Result), !.

fieldx(Instance, [Field_name|Field_names], Result) :-
    field(Instance, Field_name, Par_result),
    fieldx(Par_result, Field_names, Result).

%%% PREDICATI DI CONTROLLO:
%%% Questi predicati sono stati implementati per effettuare i
%%% controlli necessari a garantire la correttezza della base della
%%% conoscenza quando vengono usate le primitive del linguaggio.

%%% parents_exist

%%% parents_exist/1:
%%% Il predicato controlla ricorsivamente se tutti gli elementi della
%%% lista passata sono nomi di classi presenti nel sistema Prolog. Il
%%% predicato ha successo se tutti gli elementi della lista sono nomi
%%% di classi; altrimenti fallisce.

%%% Caso base:

parents_exist([]).

%%% Passo ricorsivo:

parents_exist([Parent|Parents]) :-
    is_class(Parent),
    parents_exist(Parents).


%%% parse_class_fields

%%% parse_class_fields/3:
%%% Il predicato si occupa di effettuare il parsing della lista
%%% <Parts> di una classe. Il predicato distingue diversi casi, su cui
%%% effettua diversi controlli:

%%% 1) Il termine considerato è del tipo:
%%% field(Field_name, Value). 
%%% In questo caso, il predicato si occupa di controllare se esiste un
%%% altro campo di nome <Field_name> nelle superclassi e di ereditarne
%%% il tipo <Type>. Dopodiché, il predicato controlla se <Value> è
%%% conforme con <Type>

%%% 2) Il termine considerato è del tipo:
%%% field(Field_name, Value, Type).
%%% In questo caso, il predicato si occupa di controllare solo se il
%%% valore <Value> è conforma a <Type> e che quest'utlimo sia conforma ai
%%% tipi nelle superclassi.

%%% Caso base

parse_class_fields([], _, []).

%%% Passi ricorsivi:

%%% Caso 1: Campo non tipizzato

parse_class_fields([Part|Parts], Parents, [New_part|New_parts]) :-
    functor(Part, field, 2), !,
    arg(1, Part, Field_name),
    atom(Field_name),
    conflict_check(field(Field_name, _), Parts),
    conflict_check(field(Field_name, _, _), Parts),
    inherit_type(Part, Parents, Field_with_type),
    parse_field_value(Field_with_type, New_part),
    parse_class_fields(Parts, Parents, New_parts).

%%% Caso 2: Campo tipizzato

parse_class_fields([Part|Parts], Parents, [New_part|New_parts]) :-
    functor(Part, field, 3), !,
    arg(1, Part, Field_name),
    atom(Field_name),
    conflict_check(field(Field_name, _), Parts),
    conflict_check(field(Field_name, _, _), Parts),
    arg(3, Part, Type),
    inherited_type_check(Field_name, Type, Parents),
    parse_field_value(Part, New_part),
    parse_class_fields(Parts, Parents, New_parts).

%%% inherited_type_check

%%% inherited_type_check/3:
%%% Il predicato controlla che il tipo <Type> sia uguale o un sottotipo del
%%% tipo dichiarato per <Field_name> nelle superclassi <Parents>

%%% Casi base:

inherited_type_check(_, _, []).

inherited_type_check(Field_name, Type, [Parent|_]) :-
    is_class(Parent),
    class(Parent, _, Parts),
    get_field(Field_name, Parts, Field), !,
    field_type(Field, Superclass_type),
    type_check(Superclass_type, Type).

%%% Passo ricorsivo:

inherited_type_check(Field_name, Type, Parents) :-
    classes_dfs_next(Parents, Updated_parents),
    inherited_type_check(Field_name, Type, Updated_parents).

%%% conficlt_check

%%% conflict_check/2:
%%% Il predicato controlla che non vi siano altri elementi del tipo
%%% <Template> all'interno della lista <List>

conflict_check(Template, List) :-
    nonvar(Template),
    is_list(List),
    member(Template, List), fail.

conflict_check(Template, List) :-
    nonvar(Template),
    is_list(List).


%%% parse_field_value

%%% parse_field_value/2:
%%% Il predicato effettua il parsing di <Value> di un termine del
%%% tipo: field(Field_name, Value) oppure
%%%       field(Field_name, Value, Type)
%%% In particolare, il predicato controlla che <Value> sia di tipo
%%% <Type> e unifica <New_field> con un un nuovo predicato del tipo:
%%% field(Field_name, Inst_name) oppure
%%% field(Field_name, Inst_name, Type)
%%% se <Value> è del tipo make(Inst_name, Class_name). Il predicato
%%% fallisce se il <Value> non è di tipo <Type>

%%% Casi field(Field_name, Value)

parse_field_value(Field, field(Field_name, Inst_name)) :-
    Field =.. [field, Field_name, Make], 
    Make =.. [make, Inst_name, _|_], !,
    call(Make).

parse_field_value(Field, Field) :- functor(Field, field, 2), !.

%%% Casi field(Field_name, Value, Type)

parse_field_value(Field, field(Field_name, Inst_name, Class)) :-
    Field =.. [field, Field_name, Make, Class], 
    Make =.. [make, Inst_name, Inst_class|_], !,
    type_check(Class, Inst_class),
    call(Make).

parse_field_value(Field, field(Field_name, Value, Type)) :-
    Field =.. [field, Field_name, Value, Type], !,
    value_type_check(Value, Type).


%%% type_check

%%% type_check/2:
%%% Il predicato effettua un controllo sui tipi passati. Esso ha successo
%%% sse i due tipi sono identici oppure se <Type_2> è sottotipo di
%%% <Type_1>. Il predicato supporta tutti i tipi espressi dai termini
%%% type/1 e subtype/2.

type_check(Type, Type) :- type(Type).

type_check(Type_1, Type_2) :-
    type(Type_1),
    type(Type_2),
    is_subtype(Type_1, Type_2).

%%% is_subtype

%%% is_subtype/2:
%%% Controlla ricorsivamente che il tipo <Subtype> sia un sottotipo di     
%%% <Supertye>. Il predicato implementa la transitività della relazione
%%% espressa dai termini subtype/2.

%%% Caso base:

is_subtype(Supertype, Subtype) :- subtype(Supertype, Subtype), !.

%%% Passo ricorsivo:

is_subtype(Supertype, Subtype) :-
    subtype(Supertype, Other_subtype), !,
    is_subtype(Other_subtype, Subtype).


%%% value_type_check

%%% value_type_check/2:
%%% Il predicato controlla che <Value> sia un valore di tipo <Type> oppure
%%% che sia un valore di un sottotipo di <Type> . Si distinguono due casi
%%% particolari:
%%% 1) Qualsiasi <Value> è di tipo any
%%% 2) <nil> rappresenta un valore "null" per un'istanza di una classe

value_type_check(_, any) :- !.

value_type_check(nil, Class) :- is_class(Class), !.

value_type_check(Value, Type) :- is_of_type(Type, Value), !.

value_type_check(Value, Type) :-
    get_value_type(Type_value, Value),
    type_check(Type, Type_value), !.


%%% parse_instance_fields

%%% parse_instance_fields/3:
%%% Il predicato effettua ricorsivamente il parsing dei campi dichiarati in
%%% un'istanza del tipo:
%%% <Field_name> = <Value>
%%% In particolare, il predicato controlla che <Field_name> sia presente
%%% nella classe <Class_name> o nelle sue superclassi, che <Value> sia
%%% conforme al tipo dichiarato per <Field_name> ed esegue, dopo aver fatto 
%%% gli adeguati controlli, l'eventuale make(Inst_name, Inst_Class) e
%%% sostituisce <Value> con <Inst_name>

%%% caso base:

parse_instance_fields([], _, []).

parse_instance_fields([Field|Fields], Class_name, [New_field|New_fields]) :-
    compound_name_arguments(Field, =, [Field_name, Make]),
    is_class(Class_name),
    field_type_in_class(Class_name, Field_name, Type),
    Make =.. [make, Inst_name, Inst_class|_],
    type_check(Type, Inst_class),
    call(Make),
    compound_name_arguments(New_field, =, [Field_name, Inst_name]),
    parse_instance_fields(Fields, Class_name, New_fields).

parse_instance_fields([Field|Fields], Class_name, [Field|New_fields]) :-
    compound_name_arguments(Field, =, [Field_name, Field_value]),
    is_class(Class_name),
    field_type_in_class(Class_name, Field_name, Type),
    value_type_check(Field_value, Type),
    parse_instance_fields(Fields, Class_name, New_fields).


%%% is_reference

%%% is_reference/1:
%%% Il predicato controlla se <Value> è il nome di un riferimento ad
%%% un'istanza; in questo caso ha sucesso, altrimenti fallisce.

is_reference(Value) :- inst_ref(Value, instance(_, _)).

%%% is_reference/2:
%%% Il predicato controlla se <Value> è il nome di un riferimento ad
%%% un'istanza di una certa classe o di una sottoclasse di quella classe; in
%%% questo caso il predicato ha successo, altrimenti fallisce.

is_reference(Value, Class) :-
    is_reference(Value),
    inst_ref(Value, instance(Inst_class, _)),
    is_class(Class),
    is_class(Inst_class),
    type_check(Class, Inst_class).


%%% ALTRI PREDICATI:
%%% Questi predicati sono stati implementati per "astrarre" delle
%%% operazioni che i vari predicati implementati svolgono
%%% ripetutamente per evitare l'implementazione di codice duplicate o
%%% per  "spostare" queste operazioni in modo da migliorare la
%%% leggibilità del codice.

%%% separate_parts

%%% separate_parts/3:
%%% Il predicato separa i termini del tipo: field(Field_name, Value)
%%%                                         field(Field_name, Value, Type)
%%% dai termini:
%%% method(Method_name, Arglist, Body)

separate_parts(Parts, Fields, Methods) :-
    filter_list(field(_, _), Parts, Field_2),
    filter_list(field(_, _, _), Parts, Field_3),
    filter_list(method(_, _, _), Parts, Methods),
    append(Field_2, Field_3, Fields),
    length(Parts, Length_of_parts),
    length(Fields, Length_of_fields),
    length(Methods, Length_of_methods),
    Length_of_parts is Length_of_fields + Length_of_methods.


%%% filter_list

%%% filter_list/3:
%%% Il predicato filtra ricorsivamente una lista, restituendo una
%%% lista <Result> di tutti gli elementi che unificano con <Template>

%%% Caso base:

filter_list(_, [], []).

%%% Passi ricorsivi:

filter_list(Template, [X|Xs], [X|Results]) :-
    unifiable(Template, X, _), !,
    filter_list(Template, Xs, Results).

filter_list(Template, [_|Xs], Result) :-
    filter_list(Template, Xs, Result).

%%% eliminate_duplicates

%%% eliminate_duplicates/2:
%%% Il predicato elimina ricorsivamente i duplicati all'interno di una
%%% lista e unifica il 2° argomento con la lista priva di duplicati.

%%% Caso base:

eliminate_duplicates([], []).

%%% Passi ricorsivi:

eliminate_duplicates([X|Xs], Result) :-
    member(X, Xs), !,
    eliminate_duplicates(Xs, Result).

eliminate_duplicates([X|Xs], [X|Rest]) :-
    eliminate_duplicates(Xs, Rest).

%%% inherit_type/3:
%%% Il predicato si occupa di convertire i termini di tipi:
%%% field(Field_name, Value) in field(Field_name, Value, Type)
%%% ereditando, se presente, <Type> da un campo delle superclassi. Il
%%% predicato unifica il nuovo termine con <New_field>

%%% Casi base:

inherit_type(Field, [], Field) :- !.

inherit_type(Field, [Parent|_], field(Field_name, Value, Type)) :-
    functor(Field, field, 2),
    arg(1, Field, Field_name),
    field_value(Field, Value),
    is_class(Parent),
    class(Parent, _, Parts),
    get_field(Field_name, Parts, Parents_field), !,
    field_type(Parents_field, Type).

%%% Passo ricorsivo:

inherit_type(Field, Parents, New_field) :-
    classes_dfs_next(Parents, Updated_parents),
    inherit_type(Field, Updated_parents, New_field).

%%% get_field

%%% get_field/3:
%%% Il predicato estrae un determinato campo con nome <Field_Name>
%%% dalla lista <Parts> di una classe. Il predicato ha successo se il
%%% campo viene trovato, altrimenti fallisce

get_field(Field_name, Parts, Field) :-
    filter_list(field(Field_name, _), Parts, Field_2),
    filter_list(field(Field_name, _, _), Parts, Field_3),
    append(Field_2, Field_3, Field_list),
    Field_list \= [], !,
    nth1(1, Field_list, Field).

%%% field_value

%%% field_value/2:
%%% Il predicato si occupa di estrarre dai termini di tipo:
%%% field(Field_name, Value)
%%% field(Field_name, Value, Type),
%%% Il campo <Value>

field_value(Field, Value) :-
    functor(Field, field, Arity),
    between(2, 3, Arity),
    arg(2, Field, Value).


%%% classes_dfs_next

%%% classes_dfs_next/2:
%%% Il predicato ritorna il prossimo ordine di attraversamento in
%%% profondità del grafo delle superclassi

classes_dfs_next([], []).

classes_dfs_next([Parent], Next_order) :-
    is_class(Parent),
    class(Parent, Next_order, _), !.

classes_dfs_next([Parents|Parents], Next_order) :-
    is_class(Parent),
    class(Parent, Superclasses, _), !,
    append(Superclasses, Parents, Next_order_with_duplicates),
    eliminate_duplicates(Next_order_with_duplicates, Next_order).


%%% load_methods

%%% load_methods/3:
%%% Il predicato si occupa di caricare ricorsivamente nella base della
%%% conoscenza di Prolog i metodi presenti all'interno della lista
%%% <Methods>.

%%% Caso base:

load_methods([], _, _).

load_methods([method(Method_name, Args, Body)|Rest], Class_name, Parents):-
    atom(Method_name),
    is_list(Args),
    compound(Body),
    conflict_check(method(Method_name, Args, _), Rest),
    Method_body =.. [',', is_instance(This, Class_name), Body],
    append([Method_name, This], Args, Method_head_list),
    Method_head =.. Method_head_list,
    get_predicates(Method_body, Method_predicates),
    append([Method_head], Method_predicates, Predicates),
    arg_to_var(Predicates, this, This, [Parsed_head|Parsed_predicates]),
    term_list_to_atom(Parsed_predicates, Atm_parsed_body),
    atom_concat(Atm_parsed_body, ', !', Atm_parsed_body_cut),
    term_to_atom(Parsed_head, Atm_parsed_head),
    atom_concat(Atm_parsed_head, ':-', Atm_rule_head),
    atom_concat(Atm_rule_head, Atm_parsed_body_cut, Atm_method),
    term_to_atom(Method, Atm_method),
    load_method(Method, Method_name, Args, Parents),
    load_methods(Rest, Class_name, Parents).

%%% get_predicates

%%% get_predicates/2:
%%% Il predicato scompone il termine <Compound> in una lista di
%%% predicati.

get_predicates(Compound, [Predicate|Predicates]) :-
    compound(Compound),
    compound_name_arguments(Compound, ',', [Predicate, Rest]),
    get_predicates(Rest, Predicates), !.


get_predicates(Compound, [Compound]) :-
    compound(Compound), !.

%%% arg_to_var

%%% arg_to_var/4:
%%% Il predicato si occupa di sostitutire il termine <Arg> nei predicati
%%% della lista <Predicates> con la variabile logica <Var>. Il risultato   
%%% viene unificato con la lista <Parsed_predicates>, si distinguono due
%%% casi: 

%%% Caso base:

arg_to_var([], _, _, []).

%%% Passo ricorsivo:

%%% Caso 1: Il predicato è a sua volta composta da un termine "compound"
%%% formato da più predicati.

arg_to_var([Comp_predicate|Predicates], Arg, Var,
	   [Parsed_predicate|Rest]) :-
    compound(Comp_predicate),
    compound_name_arguments(Comp_predicate, Name, [Sub_pred, Compound]),
    compound(Compound),
    get_predicates(Compound, Compound_pred_list),
    arg_to_var([Sub_pred], Arg, Var, [Parsed_sub_pred]),
    arg_to_var(Compound_pred_list, Arg, Var, Parsed_compound_list),
    construct_compound(Parsed_compound_list, Parsed_compound),
    compound_name_arguments(Parsed_predicate, Name, [Parsed_sub_pred,
                                                     Parsed_compound]),
    arg_to_var(Predicates, Arg, Var, Rest).

arg_to_var([Predicate|Predicates], Arg, Var, [Parsed_predicate|Rest]) :-
    compound(Predicate),
    compound_name_arguments(Predicate, Name, Args),
    replace_in_list(Args, Arg, Var, Parsed_args),
    compound_name_arguments(Parsed_predicate, Name, Parsed_args),
    arg_to_var(Predicates, Arg, Var, Rest).


%%% constuct_compound:

%%% construct_compound/3:
%%% Il predicato effettua la costruzione di un unico termine compound
%%% formato dai vari predicati all'interno della lista.

%%% Casi base:

construct_compound([Term], Term) :- !.

%%% Passi ricorsivi

construct_compound([Term_1, Term_2, Term_3|Rest], Result) :-
    compound_name_arguments(Partial_compound_2, ',', [Term_1, Term_2]),
    compound_name_arguments(Partial_compound_3, ','
			    ,[Partial_compound_2, Term_3]) ,
    construct_compound([Partial_compound_3|Rest], Result), !.

construct_compound([Term_1, Term_2|Rest], Result) :-
    compound_name_arguments(Partial_compound_2, ',', [Term_1, Term_2]),
    construct_compound([Partial_compound_2|Rest], Result), !.

%%% replace_in_list 

%%% replace_in_list/4:
%%% Il predicato sostituisce tutte le occorrenze di <Item> nella lista
%%% <List> con <Replacement>. Il risultato è unificato con la lista
%%% <Result>.

%%% Caso base:

replace_in_list([], _, _, []).

%%% Passi ricorsivi:

replace_in_list([Element|Elements], Item, Repl, [Element|Rest]) :-
    Item \=@= Element,
    replace_in_list(Elements, Item, Repl, Rest).

replace_in_list([Item|Items], Item, Repl, [Repl|Rest]) :-
    replace_in_list(Items, Item, Repl, Rest).

%%% term_list_to_atom

%%% term_list_to_atom/2:
%%% Il predicato effettua la chiamata a term_list_to_atom/3 con una lista
%%% vuota. Essa servirà da buffer per il predicato chiamato.

term_list_to_atom(Terms, Result) :-
    term_list_to_atom(Terms, [], Result).

%%% term_list_to_atom/3:
%%% Il predicato concatena in un solo atomo tutti i termini presenti nella
%%% lista  Il risultato viene unficato con <Result>. Il predicato si avvale
%%% di una lista <Buffer>  dove vengono immessi i risultati parziali.

term_list_to_atom([Term|Terms], Atm_results, Result) :-
    term_to_atom(Term, Atm_term_no_comma),
    atom_concat(Atm_term_no_comma, ',', Atm_term),
    append(Atm_results, [Atm_term], Atm_par_results),
    term_list_to_atom(Terms, Atm_par_results, Result).

term_list_to_atom([Term], Atm_terms, Result) :-
    term_to_atom(Term, Atm_term),
    atomic_list_concat(Atm_terms, Par_result),
    atom_concat(Par_result, Atm_term, Result).


%%% load_method

%%% load_method/4:
%%% Il predicato asserisce <Method> in modo che rispetti l'ereditarietà in
%%% profondità del linguaggio anche per l'override dei metodi.

%%% Casi base:

load_method(Method, _, _, []) :- asserta(Method).

load_method(Method, Method_name, Arglist, [Parent|_]) :-
    is_class(Parent),
    class(Parent, _, Parts),
    member(method(Method_name, Arglist, _), Parts),
    asserta(Method).

load_method(Method, _, _, Parents) :-
    classes_dfs_next(Parents, []),
    assertz(Method).

%%% Passo ricorsivo:

load_method(Method, Method_name, Arglist, Parents) :-
    classes_dfs_next(Parents, Updated_parents),
    load_method(Method, Method_name, Arglist, Updated_parents).

%%% field_type_in_class/3_
%%% Il predicato unifica <Result> con il valore di <Type> di un campo
%%% all'interno di una classe. Se <Type> non è presente, lo cerca nelle
%%% superclassi.

%%% Caso "trampolino":

field_type_in_class(Class, Field_name, Result) :-
    is_class(Class),
    field_type_in_class([Class], Field_name, Result).


%%% Casi base:

field_type_in_class([], _, _) :- !, fail.

field_type_in_class([Class|_], Field_name, Result) :-
    is_class(Class),
    class(Class, _, Parts),
    get_field(Field_name, Parts, Field), !,
    field_type(Field, Result).

%%% Passo ricorsivo:

field_type_in_class(Classes, Field_name, Result) :-
    classes_dfs_next(Classes, Updated_classes),
    field_type_in_class(Updated_classes, Field_name, Result).


%%% field_type

%%% field_type/2
%%% Il predicato unifica <Type> con il valore del tipo <Field>
%%% passato.

field_type(Field, any) :- functor(Field, field, 2), !.

field_type(Field, Type) :-
    functor(Field, field, 3), !,
    arg(3, Field, Type).

%%% field_value_in_class/3:
%%% Il predicato unifica <Result> con il valore di <Field_value>
%%% all'interno di <Class>. Se <Field_value> non è presente, lo cerca
%%% nelle superclassi

%%% NOTA: Probabilmente c'è un modo migliore per farlo; tampone per il
%%% test

field_value_in_class([], _, _) :- !, fail.

field_value_in_class(Class, Field_name, Result) :-
    is_class(Class),
    field_value_in_class([Class], Field_name, Result).

field_value_in_class([Class|_], Field_name, Result) :-
    is_class(Class),
    class(Class, _, Parts),
    get_field(Field_name, Parts, Field), !,
    field_value(Field, Result).

field_value_in_class(Classes, Field_name, Result) :-
    classes_dfs_next(Classes, Updated_classes),
    field_value_in_class(Updated_classes, Field_name, Result).


%%% SISTEMA DEI TIPI:
%%% I seguenti termini e predicati sono stati implementati per effettuare
%%% la gestione dei tipi.

%%% type

%%% type/1:
%%% Questi termini indicano che l'argomento è un tipo. Essi sono stati
%%% implementati per permettere al programma di avere e mantenere una
%%% dichiarazione esplicita di tutti i tipi presenti. Ogni qualvolta
%%% si dichiara una nuova classe, si dichiara anche un nuovo tipo.

%%% Tipo any: Questo tipo è equivalente al tipo T di Lisp. Esso è
%%% supertipo di tutti i tipi.

type(any).

%%% Tipi numerici: Questi tipi corrispondono ai tipi numerici presenti
%%% in Prolog:

type(number).
type(float).
type(rational).
type(integer).

%%% Tipi Prolog: Questi tipi corrispondono agli tipi presenti nel
%%% sistema Prolog

type(atom).
type(string).
type(atom).
type(compound).

%%% subtype

%%% subtype/2:
%%% Questi termini sono stati implementati per permettere al programma
%%% di mantnere una chiara gerarchia dei sottotipi. Essi rappresentano
%%% una relazione di sottotipo tra il 2° argomento (<Subtype>) e il 1°
%%% argomento (<Supertype>). La relazione è transitiva ed essa è
%%% implementata tramite il predicato is_subtype/2. Ogni qualvolsta si
%%% dichiara una nuova sottoclasse di una o più classi, si asserisce
%%% un nuovo sottotipo di tutte le sue classi genitori.

%%% Sottotipi di any: any ha come sottotipi tutti i tipi presenti:

subtype(any, _).

%%% Sottotipi numerici:

subtype(number, float).

subtype(float, rational).

subtype(rational, integer).

%%% assert_subtypes/2:
%%% Il predicato asserisce ricorsivamente nuovi sottotipi della forma:
%%% subtype(<Supertype>, <Subtype>) da una lista di supertipi
%%% <Supertypes>.

%%% Caso base:

assert_subtypes([], _).

%%% Passo ricorsivo:

assert_subtypes([Supertype|Supertypes], Subtype) :-
    assertz(subtype(Supertype, Subtype)),
    assert_subtypes(Supertypes, Subtype).

%%% get_value_type

%%% get_value_type/2:
%%% Il predicato ritorna il tipo di un certo valore. Sono supportati
%%% tutti i tipi espressi da type/1.

%%% NOTA: magari rimpiazzare con get_type

get_value_type(integer, Value) :- integer(Value).

get_value_type(float, Value) :- float(Value).

get_value_type(rational, Value) :- rational(Value).

get_value_type(number, Value) :- number(Value).

get_value_type(atom, Value) :- atom(Value).

get_value_type(string, Value) :- string(Value).

get_value_type(compound, Value) :- compound(Value).

get_value_type(Class, Value) :- is_instance(Value, Class).

get_value_type(Class, Value) :-
    is_reference(Value),
    inst_ref(Value, instance(Class, _)).

%%% end-of-file -- oop.pl

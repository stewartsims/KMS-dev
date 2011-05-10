/* 
Understand
Generate
Answer questions
Search

The Cypher™ beta release is the AI software program available which generates the RDF graph and SPARQL/SeRQL query representation of a plain language input, allowing users to speak plain language to update and query semantic databases. With robust definition languages, Cypher's grammar and lexicon can quickly and easily be extended to process highly complex sentences and phrases of any natural language, and can cover any vocabulary. Equipped with Cypher, programmers can now begin building next generation semantic web applications that harness what is already the most widely used tool known to man - natural language.


Examples:
Anaphora resolution
s(S,[john, sleeps],[]), s(S,[paul,runs],[]), s(S,[he,walks],[]).


*/
/* TODO
Check if RDF can be generated from OpenMind data.
Generate integrity constrains from NL?
Explore the notion of question answering by collaborative generation.
How does that relate to prediction/autocompletion?
Can this drive a dialogue?
What kind of role can askables play?
Perhaps look at David Pooles prover/abducer one more time?
Think more about the distributive case. Probably means that we want to use server-pushed events for monitoring the database!
*/

:- module(grammar, 
    [  combined/2,
       answer/2
    ]).

:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdf_turtle_write')).

%:- dynamic rdf/3.
:- dynamic inconsistent/0.

rdf_db:ns(ex,'http://ex#').
rdf_db:ns(ind,'http://ind#').
rdf_db:ns(wn,'http://www.cogsci.princeton.edu/~wn#').
rdf_db:ns(rdf,'http://www.w3.org/1999/02/22-rdf-syntax-ns#').


:- rdf_assert(ind:e1, ex:pred, ex:is_on).
:- rdf_assert(ind:e1, ex:arg1, ind:x1).
:- rdf_assert(ind:e1, ex:arg2, ind:x2).
   
:- rdf_assert(ind:x1, rdf:type, ex:cat).
   
:- rdf_assert(ind:x2, rdf:type, ex:mat).
   
:- rdf_assert(ind:x6, ex:named, literal(mary)).
   
:- rdf_assert(ind:x7, ex:named, literal(john)).
   
:- rdf_assert(ind:e3, ex:pred, ex:loves).
:- rdf_assert(ind:e3, ex:arg1, ind:x7).
:- rdf_assert(ind:e3, ex:arg2, ind:x6).
   
:- rdf_assert(ind:e2, ex:pred, ex:saw).
:- rdf_assert(ind:e2, ex:arg1, ind:x6).
:- rdf_assert(ind:e2, ex:arg2, ind:x4).
   
:- rdf_assert(ind:x4, rdf:type, ex:man).
:- rdf_assert(ind:x4, ex:with, ind:x5).
   
:- rdf_assert(ind:x5, rdf:type, ex:dog).



inconsistent :- 
    rdf(State1, ex:pred, ex:sleeps),
    rdf(State1, ex:arg1, Agent),
    rdf(State2, ex:pred, ex:runs),
    rdf(State2, ex:arg1, Agent).



s(E) --> np(X), vp(X^E^A), {abduce(A)}. 

np(X) --> pron(X^A), {abduce(A)}.
np(X) --> pn(X^A), {abduce(A)}.
np(X) --> det, n(X^A), {abduce(A)}. 
np(X) --> det, n(X^A), {abduce(A)}, pp(X). 

pp(X) --> p(X^Y^A), {abduce(A)}, np(Y).

vp(A) --> v1(A).
vp(A) --> v2(X^A), np(X).

det --> [the].
det --> [a]. 

pron(X^rdf(X, rdf:type, ex:male)) --> [he].
pron(X^rdf(X, rdf:type, ex:female)) --> [she].

pn(X^(rdf(X, ex:named, literal(john)), rdf(X, rdf:type, ex:male))) --> [john].
pn(X^(rdf(X, ex:named, literal(paul)), rdf(X, rdf:type, ex:male))) --> [paul].
pn(X^(rdf(X, ex:named, literal(mary)), rdf(X, rdf:type, ex:female))) --> [mary].

n(X^rdf(X, rdf:type, ex:cat)) --> [cat].
n(X^rdf(X, rdf:type, ex:mat)) --> [mat].
n(X^rdf(X, rdf:type, ex:man)) --> [man].
n(X^rdf(X, rdf:type, ex:dog)) --> [dog].


p(X^Y^rdf(X, ex:with, Y)) --> [with]. 

v1(X^E^(rdf(E, ex:pred, ex:sleeps), rdf(E, ex:arg1, X))) --> [sleeps].
v1(X^E^(rdf(E, ex:pred, ex:walks), rdf(E, ex:arg1, X))) --> [walks].
v1(X^E^(rdf(E, ex:pred, ex:runs), rdf(E, ex:arg1, X))) --> [runs].

v2(Y^X^E^(rdf(E, ex:pred, ex:saw),   rdf(E, ex:arg1, X), rdf(E, ex:arg2, Y))) --> [saw].
v2(Y^X^E^(rdf(E, ex:pred, ex:loves), rdf(E, ex:arg1, X), rdf(E, ex:arg2, Y))) --> [loves].
v2(Y^X^E^(rdf(E, ex:pred, ex:found), rdf(E, ex:arg1, X), rdf(E, ex:arg2, Y))) --> [found].
v2(Y^X^E^(rdf(E, ex:pred, ex:is_on), rdf(E, ex:arg1, X), rdf(E, ex:arg2, Y))) --> [is, on].


% A simple prover/abducer


:- dynamic allow_assumptions/0.
allow_assumptions.


abduce(Goal) :-
    rdf_global_term(Goal, ExpandedGoal),
    prove(ExpandedGoal).

prove((A, B)) :- !,
    prove(A), 
    prove(B).
prove(A) :-
    call(A).
prove(A) :-
    allow_assumptions,
    \+ A,               % check that A is not already in the theory
    ensure_ground(A),
    rdf_assume(A),      % add A to the current theory
    \+ inconsistent.    % but backtrack if the theory becomes inconsistent



ensure_ground(rdf(S, P, O)) :-
    var(S), var(P), var(O), !,
    rdf_node(S), rdf_node(P), rdf_node(O).
ensure_ground(rdf(S, P, _O)) :-
    var(S), var(P), !,
    rdf_node(S), rdf_node(P).
ensure_ground(rdf(S, _P, O)) :-
    var(S), var(O), !,
    rdf_node(S), rdf_node(O).    
ensure_ground(rdf(_S, P, O)) :-
    var(P), var(O), !,
    rdf_node(P), rdf_node(O).    
ensure_ground(rdf(S, _P, _O)) :-
    var(S), !,
    rdf_node(S). 
ensure_ground(rdf(_S, P, _O)) :-
    var(P), !,
    rdf_node(P). 
ensure_ground(rdf(_S, _P, O)) :-
    var(O), !,
    rdf_node(O). 
ensure_ground(rdf(_S, _P, _O)).    



rdf_assume(rdf(S, P, O)) :-
    write(assuming-rdf(S, P, O)), nl,
    rdf_assert(S, P, O).
    
rdf_assume(rdf(S, P, O)) :-
    rdf_retractall(S, P, O),
    write(retracting-rdf(S, P, O)), nl, 
    fail.

    

% List the database

list_prolog :-
    forall(rdf(A,B,C),
        (   writeq(rdf(A,B,C)), nl)
        ).

list_turtle :-
    rdf_save_turtle(stream(current_output),[]).

list_xml :-
    rdf_save(stream(current_output),[]).

clear :-
    rdf_retractall(_S, _P, _O).


% Combined
% The input list is first regarded as a "question", and all answers are generated. But as soon as no answers *can* be generated, it is regarded as a statement. The idea here would be to connect this to an autocompletion widget.

combined(Atom, JsonList) :-
    atomic_list_concat(LC, ' ', Atom),
    open_list(LC, L),
    retractall(allow_assumptions),
    (   s(_, L, [])
    ->  atomic_list_concat(L, ' ', JsonList)
    ;   assert(allow_assumptions),
        rdf_transaction(s(S, LC, []))
    ->  JsonList = S
    ;   JsonList = 'Building statement...'
    ).

open_list([], _).
%open_list([_], [_|_]) :- !.
open_list([X|Xs], [X|Ys]) :-
    open_list(Xs, Ys).

sentences_to_json(L, Sent) :-
    atomic_list_concat(L, ' ', Sent).
    

bind_tail(T) :- var(T), !, T = [].
bind_tail([_H|T]) :-
    bind_tail(T).
    
        
% Make a statement
state(L) :-
    rdf_transaction(s(_S, L, [])).

% Answer a question 
% Question answering as collaborative generation 
answer(Atom, Sents) :-
    retractall(allow_assumptions),
    atomic_list_concat(LC, ' ', Atom),
    open_list(LC, L),
    findall(L, s(_S, L, []), Ls),
    Ls \= [],
    maplist(sentences_to_json, Ls, Sents),
    assert(allow_assumptions).
answer(_Atom, ['I don\'t know...']).
    
       
    
% Generate all true sentences
gen(S) :-
    retractall(allow_assumptions),
    forall(
        s(S, L, []), 
        (   atomic_list_concat(L, ' ', Sent), 
            write(Sent), nl
        )
    ),
    assert(allow_assumptions).


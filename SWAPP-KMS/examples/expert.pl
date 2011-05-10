:- module(expert,
	  [ prove_json/2,
	    op(1150, xfx, (<-)),
	    op(950, xfy, (&))
	  ]).

:- use_module(library('http/json')).
:- use_module(library('http/http_json')).
:- use_module(library('http/http_session')).


%%	prove(+Query, -Proof) is nondet.
%
%	A simple expert system meta interpreter

:- meta_predicate prove_json(:, -).

prove_json(M:Goal, JsonProofTree) :-
    prove(Goal, M, Proof),
    proof_to_json(Proof, JsonProofTree).


prove(true, _, true) :- !.
prove((B & Bs), M, and(BT, BTs)) :- !,
    prove(B, M, BT),
    prove(Bs, M, BTs).
prove(H, M, if(H, BT)) :-
    clause(M:(H <- B), true),
    prove(B, M, BT).
prove(H, M, if(H, asked)) :-
    clause(M:askable(H), true),
    term_to_atom(H, Atom),
    atomic_list_concat([Atom, ' ?'], Query),
    ask(Query, Answer),
    Answer == yes.



%%	ask(+Query, ?Answer)
%
%	Predicate that poses a query and collects an answer.

ask(Query, Answer) :-
    http_session_data(eq(Queue)),
    thread_send_message(Queue, query(Query)),
    thread_get_message(Queue, answer(Answer)).



%%	proof_to_json(+Proof, -JsonTree) is det.
%
%	Transforming a proof tree to JSON

proof_to_json(if(H,asked),json([type='HTML', html=HAHTML])) :- !,
    term_to_atom(H,HA),
    atomic_list_concat(['<div style="color:blue">',HA,'</div>'], HAHTML).
proof_to_json(if(H,true),json([type='HTML', html=HAHTML])) :- !,
    term_to_atom(H,HA),
    atomic_list_concat(['<div style="color:darkgreen">',HA,'</div>'], HAHTML).
proof_to_json(false,false).
proof_to_json(if(H,B), json([type='Text', label=HA, children=Cs])) :-
    term_to_atom(H,HA),
    proof_to_json_body(B,Cs).

proof_to_json_body(and(A, B),[H|Hs]) :- !,
    proof_to_json(A, H),
    proof_to_json_body(B, Hs).
proof_to_json_body(A, [H]) :-
    proof_to_json(A, H).



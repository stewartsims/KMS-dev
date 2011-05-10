:- module(sessiondb_api, []).


/***************************************************
* load_modules
***************************************************/

% http library modules
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/http_session')).
:- use_module(library('http/http_json')).
:- use_module(library('http/http_client')).

% lib modules
:- use_module('lib/thread_call').
:- use_module('lib/term_to_json').
:- use_module('lib/safecode').
:- use_module('lib/auth').
:- use_module('lib/dispatch').

% semweb library
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_persistency)).


:- use_module(library(settings)).
:- use_module(library(debug)).

/***************************************************
* settings
***************************************************/

:- setting(cache_timeout, nonneg, 60, 'Cache timeout in seconds').
:- setting(solution_timeout, nonneg, 20, 'Solution timeout in seconds').


/***************************************************
* http handlers
***************************************************/

:- http_handler(root('swapp/session/db'), dispatch, []).

/***************************************************
* handle reply
***************************************************/

%%	dispatch_method(+Method, +Request)

dispatch_method(Method, Request) :-
    memberchk(Method, [put, post]), !,
    http_session_id(SessionId),
    % We need to flush the cache for outstanding GETs since
    % otherwise we get a problem with active clauses (it seems..)
    psc_flush_soft(SessionId),
    setup_call_cleanup(new_memory_file(Handle),
		       consult_code(Request, Handle),
		       free_memory_file(Handle)).
dispatch_method(delete, Request) :-
    % We need to flush the cache for outstanding GETs since
    % otherwise we get a problem with active clauses (it seems..)
    http_session_id(SessionId),
    psc_flush_soft(SessionId),
    http_parameters(Request,
		    [ template(TemplateAtom, [default('')])
		    ]),
    (   TemplateAtom \== ''
    ->  atom_to_term(TemplateAtom, Templates, _),
        (   is_list(Templates)
        ->  forall(member(Template, Templates),
		   (   not_qualified(Template),
		       retract(SessionId:Template)))
        ;   not_qualified(Templates),
	    forall(retract(SessionId:Templates), true)
        )
    ;   forall(current_predicate(SessionId:PI),
	       abolish(SessionId:PI))
    ),
	reply_json(json([success= @true, message=deleted]), [width(0)]).
dispatch_method(get, Request) :-
    http_parameters(Request,
        [ query(GoalAtom, []),
          cursor(Cursor, [nonneg, default(0)]),
          limit(Limit, [integer, default(1)]),
          output(Output, [oneof([json,'json-s']), default(json)])
  		]),
  	must_be(positive_integer, Limit),
  	I is Cursor + 1,
    J is Cursor + Limit,
  	http_session_id(SessionId),
    text_to_goal(GoalAtom, Goal, SessionId, Bindings),
    rdf_db:rdf_global_term(Goal, ExpandedGoal),
    setting(sessiondb_api:cache_timeout, CacheTimeout),
    setting(sessiondb_api:solution_timeout, SolutionTimeout),
    thread_call(SessionId, SessionId:ExpandedGoal, I-J, Bindings, Result,
        [ abort_thread_after(CacheTimeout),
          time_limit(SolutionTimeout)
        ]),
    (   Output = 'json'
    ->  term_to_json(Result, Bindings, Json)
    ;   Output = 'json-s'
    ->  mk_shallow(Result, ShallowResult),
        term_to_json(ShallowResult, Bindings, Json)
    ),
    reply_json(Json, [width(0)]).


% this exist also in sessiondb_api - share somehow!
mk_shallow(Result, ShallowResult) :-
    maplist(swap1, Result, ShallowResult).

swap1(bindings=Bindings, bindings=Bindings2) :- !,
    maplist(swap2, Bindings, Bindings2).
swap1(Pair, Pair).

swap2(Bindings, Bindings2) :-
    maplist(swap3, Bindings, Bindings2).

swap3(N=V, N=A) :-
    term_to_atom(V,A).


text_to_goal(GoalAtom, Goal, SessionId, Bindings) :-
	atom_to_term(GoalAtom, Goal, Bindings),
	(   safe_goal(SessionId:Goal)
	->  true
	;   permission_error(execute, goal, Goal)
	).


%%	consult_code(+Request, +MemFile)

consult_code(Request, Handle) :-
	setup_call_cleanup(open_memory_file(Handle, write, Output),
			   http_read_data(Request, _, [to(stream(Output))]),
			   close(Output)),
	setup_call_cleanup(open_memory_file(Handle, read, Stream),
			   consult_stream(Request, Stream),
			   close(Stream)),
	reply_json(json([success= @true, message=updated]), [width(0)]).

consult_stream(Request, Stream) :-
	http_session_id(SessionId),
	(   memberchk(method(put), Request)
	->  forall(current_predicate(SessionId:PI),
		   abolish(SessionId:PI))
	;   true
	),
	repeat,
        read_term(Stream, Term, [module(SessionId)]),
        (   Term == end_of_file
        ->  !
	;   expand_term(Term, ExpandedTerm),
	    consult_term(ExpandedTerm, SessionId),
	    fail
	).

consult_term(Var, _) :-
	var(Var), !,
	instantiation_error(Var).
consult_term([], _) :- !.
consult_term([H|T], Module) :- !,
	consult_term(H, Module),
	consult_term(T, Module).
consult_term((:- Directive), Module) :- !,
	expand_goal(Directive, Goal),
	(   safe_goal(Module:Goal)
	->  Module:Goal
	;   permission_error(execute, goal, Goal)
	).
consult_term(Clause, Module) :-
	not_qualified(Clause),
	assert(Module:Clause).

not_qualified(Clause) :-
	Clause = _:_, !,
	permission_error(assert, clause, Clause).
not_qualified(_).

%:- debug(test).

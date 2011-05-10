:- module(rdfdb_api, []).


/***************************************************
* load_modules
***************************************************/

% http library modules
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/http_session')).
:- use_module(library('http/http_json')).
:- use_module(library('http/http_client')).

% semweb library
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_persistency)).

% lib modules
:- use_module('lib/thread_call').
:- use_module('lib/term_to_json').
:- use_module('lib/safecode').
:- use_module('lib/auth').
:- use_module('lib/dispatch').


:- use_module(library(settings)).
:- use_module(library(debug)).


/***************************************************
* settings
***************************************************/

:- setting(cache_timeout, nonneg, 60, 'Cache timeout in seconds').
:- setting(solution_timeout, nonneg, 20, 'Solution timeout in seconds').
:- setting(persistent_db, atom, 'SWAPP-RDF', 'Directory for persistent store').

/***************************************************
* initialisation
***************************************************/

:- multifile swapp:startup/0.

swapp:startup :-
	(   setting(persistent_db, Dir),
	    Dir \== ''
	->  assert(swapp:loading, Ref),
	    call_cleanup(rdf_attach_db(Dir, []),
			 erase(Ref))
	;   true
	).

/***************************************************
* http handlers
***************************************************/

:- http_handler(root('swapp/rdf/db'), dispatch, []).


/***************************************************
* handle reply
***************************************************/

dispatch_method(Method, Request) :-
    memberchk(Method, [put, post]), !,
    http_session_id(SessionID),
    psc_flush_soft(SessionID),
    new_memory_file(Handle),
    open_memory_file(Handle, write, Output),
    http_read_data(Request, _Data, [to(stream(Output))]),
    close(Output),
    (   Method == put
    ->  rdf_db:rdf_retractall(_, _, _)
    ;   true
    ),
    open_memory_file(Handle, read, Stream),
    member(content_type(ContType), Request),
    (   ContType == 'application/x-turtle; charset=UTF-8'
    ->  rdf_db:rdf_load(Stream, [format(turtle), register_namespaces(true)])
    ;   ContType == 'text/rdf+xml; charset=UTF-8'
    ->  rdf_db:rdf_load(Stream, [format(xml), register_namespaces(true)])
    ),
    free_memory_file(Handle),
    reply_json(json([success= @true, message=updated]), [width(0)]).
dispatch_method(delete, Request) :-
    http_session_id(SessionID),
    psc_flush_soft(SessionID),
    http_parameters(Request,
        [ template(TemplateAtom, [default('')])
        ]),
    (   TemplateAtom \= ''
    ->  atom_to_term(TemplateAtom, Templates, _),
        (   is_list(Templates)
        ->  forall(member(rdf_db:rdf(S, P, O), Templates),
                rdf_db:retractall(S, P, O))
        ;   (   rdf_db:rdf(S, P, O) = Templates
            ->  rdf_db:rdf_retractall(S, P, O)
            ;   true
            )
        )
    ;    rdf_db:rdf_retractall(_, _, _)
    ),
    reply_json(json([success= @true, message=deleted]), [width(0)]).
dispatch_method(get, Request) :-
    http_parameters(Request,
        [ query(GoalAtom, []),
          cursor(Cursor, [integer, default(0)]),
          limit(Limit, [integer, default(1)]),
          output(Output, [oneof([json,'json-s']), default(json)])
  		]),
  	must_be(nonneg, Cursor), % SWI bug?: for some reason, nonneg and positive_integer doesn't work above...
  	must_be(positive_integer, Limit),
  	I is Cursor + 1,
    J is Cursor + Limit,
  	http_session_id(SessionID),
  	text_to_goal(GoalAtom, Goal, Bindings),
    rdf_db:rdf_global_term(Goal, ExpandedGoal),
    setting(rdfdb_api:cache_timeout, CacheTimeout),
    setting(rdfdb_api:solution_timeout, SolutionTimeout),
    thread_call(SessionID, ExpandedGoal, I-J, Bindings, Result,
        [ abort_thread_after(CacheTimeout),
          time_limit(SolutionTimeout)
        ]),
    (   Output = 'json'
    ->  term_to_json(Result, Json)
    ;   Output = 'json-s'
    ->  mk_shallow(Result, ShallowResult),
        term_to_json(ShallowResult, Json)
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




text_to_goal(GoalAtom, Goal, Bindings) :-
	atom_to_term(GoalAtom, Goal, Bindings),
	(   safe_goal(Goal)
	->  true
	;   permission_error(execute, goal, Goal)
	).


:- debug(rdfdb_api).

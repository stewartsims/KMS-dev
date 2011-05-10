:- module(admin, []).
/*
 * Designed for administration access of specific modules (e.g. ka_manager to modify KAs)
 */

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

%KMS modules
:- use_module(logger).

/***************************************************
* settings
***************************************************/

:- setting(cache_timeout, nonneg, 60, 'Cache timeout in seconds').
:- setting(solution_timeout, nonneg, 20, 'Solution timeout in seconds').

%set the modules allowed to be queried here
:- dynamic admin_modules/2.

:- assert(admin_modules(ka_manager,'adminpassword')).

/***************************************************
* http handlers
***************************************************/

:- http_handler(root('swapp/admin'), dispatch, []).

/***************************************************
* handle reply
***************************************************/

%%	dispatch_method(+Method, +Request)

dispatch_method(get, Request) :-
    http_parameters(Request,
        [ query(GoalAtom, []),
	    module(Module, []),
	    password(Password, []),
          cursor(Cursor, [nonneg, default(0)]),
          limit(Limit, [integer, default(1)]),
          output(Output, [oneof([json,'json-s']), default(json)])
  		]),
    admin_modules(Module,Password),
  	must_be(positive_integer, Limit),
  	I is Cursor + 1,
    J is Cursor + Limit,
  	http_session_id(SessionId),
    text_to_goal(GoalAtom, Goal, Module, Bindings),
    rdf_db:rdf_global_term(Goal, ExpandedGoal),
    setting(admin:cache_timeout, CacheTimeout),
    setting(admin:solution_timeout, SolutionTimeout),
    thread_call(SessionId, Module:ExpandedGoal, I-J, Bindings, Result,
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

text_to_goal(GoalAtom, Goal, _, Bindings) :-
	atom_to_term(GoalAtom, Goal, Bindings),
	(   true
	->  true
	;   permission_error(execute, goal, Goal)
	).

:- module(sessioneq_api, []).


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
:- use_module('../lib/term_to_json').
:- use_module('../lib/auth').
:- use_module('../lib/swapp_predicates').
:- use_module('../lib/dispatch').


:- use_module(library(settings)).
:- use_module(library(debug)).


/***************************************************
* http handlers
***************************************************/

:- http_handler(root('swapp/session/eq'), dispatch,
		[ spawn(cheapthreads),
		  time_limit(30)
		]).


/***************************************************
* handle reply
***************************************************/

%%	dispatch_method(+Method, +Request)
%
%	Handling of PUT, POST, DELETE and GET on /swapp/session/eq.

dispatch_method(put, _Request) :-
    swapp_session_eq_create,
    reply_json(json([success= @true, message='session event queue created']), [width(0)]).
dispatch_method(post, Request) :-
    http_read_data(Request, Data, [to(atom)]),
    swapp_session_eq_id(Queue),
    atom_to_term(Data, Term, _Bindings),
    (   is_list(Term)
    ->  forall(member(T, Term), thread_send_message(Queue, T))
    ;   thread_send_message(Queue, Term)
    ),
    reply_json(json([success= @true, message='event(s) sent']), [width(0)]).
dispatch_method(delete, _Request) :-
    swapp_session_eq_destroy,
	reply_json(json([success= @true, message='session event queue destroyed']), [width(0)]).
dispatch_method(get, Request) :-
    http_parameters(Request,
        [ template(TemplateAtom, [default('_')]),
          method(Method, [oneof(['long-polling','short-polling']), default('long-polling')]),
          output(Output, [oneof([json,'json-s']), default(json)])
  		]),
    swapp_session_eq_id(Queue),
    atom_to_term(TemplateAtom, Template, _Bindings),
    copy_term(Template, Event),
    (   Method == 'long-polling'
    ->  thread_get_message(Queue, Event),
        Events = [Event|Events0]
    ;   Method == 'short-polling'
    ->  Events = Events0
    ),
    get_rest_of_events(Queue, Template, Events0),
    (   Output = 'json'
    ->  term_to_json(Events, Json)
    ),
    reply_json(json([success= @true, message='event(s) received', events=Json]), [width(0)]).


get_rest_of_events(Queue, Template, [Event|Events]) :-
    copy_term(Template, Event),
    thread_peek_message(Queue, Event), !,
    thread_get_message(Queue, Event),
    get_rest_of_events(Queue, Template, Events).
get_rest_of_events(_Queue, _Template, []).





:- listen(http_session(end(_SessionId, _Peer)),
	  stop_client).

stop_client :-
	(   http_session_data(eq(Queue))
	->  message_queue_destroy(Queue)
	;   true
	).

:- debug(queue).


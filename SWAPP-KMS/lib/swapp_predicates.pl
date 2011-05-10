:- module(swapp_predicates,
    [ swapp_session_eq_create/0,
      swapp_session_eq_destroy/0,
      swapp_session_eq_id/1,
      swapp_current_session_eq/1,
      swapp_session_eq_send/2,
      swapp_session_eq_send_to_all/1
    ]).
:- use_module(library('http/http_session')).


:- dynamic
	safe_queue/1.

swapp_session_eq_create :-
	(   http_session_data(eq(Queue0)),
	    retract(safe_queue(Queue0))
	->  http_session_retract(eq(Queue0)),
	    message_queue_destroy(Queue0)
	;   true
	),
	assert(safe_queue(Queue)),
	message_queue_create(Queue),
	http_session_assert(eq(Queue)).


swapp_session_eq_destroy :-
	http_session_data(eq(Queue)), !,
	retract(safe_queue(Queue)),
	message_queue_destroy(Queue),
	http_session_retract(eq(Queue)).
swapp_session_eq_destroy :-
	existence_error(queue, session).


swapp_session_eq_id(Queue) :-
	http_session_data(eq(Queue)),
	safe_queue(Queue), !.
swapp_session_eq_id(_Queue) :-
	existence_error(queue, session).

swapp_current_session_eq(Queue) :-
	http_current_session(_SessionID, eq(Queue)),
	safe_queue(Queue).

swapp_session_eq_send(Queue, Event) :-
	safe_queue(Queue),
	thread_send_message(Queue, Event).

swapp_session_eq_send_to_all(Event) :-
	forall(swapp_current_session_eq(Queue),
	       swapp_session_eq_send(Queue, Event)).



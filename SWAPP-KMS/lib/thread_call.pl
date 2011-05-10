:- module(psc,
	  [ thread_call/6,
	    psc_flush_soft/1
	  ]).

:- use_module(library(error)).
:- use_module(library(thread_pool)).
:- use_module(library(option)).
			   

/** <module> Prolog State Caching (PSC)

This module is a demonstration of   handling  a conversation with Prolog
through a web-interface where state for the   conversation  is kept in a
non-deterministic Prolog predicate.

The design associates a thread that runs the conversation (and keeps the
state) with each HTTP session. This thread is created by first/1 and can
end due to next/1, stop/1  or  timeout   of  the  session. The latter is
signalled through the library(broadcast).  See   the  directive  calling
listen/2.

@author	Torbjörn Lager
@author	Jan Wielemaker

*/


%%	thread_call(+CacheID, +Goal, +I, +Bindings, -Result, +Options) is det.
%
%   Implements a Prolog solver with a the following properties:
%	Generate the Bindings of the I:th solution to Goal.
%   Caches the Prolog state in between calls. The cache will either be used or flushed, depending on the next.
%   Deterministic


thread_call(CacheId, Query, Index, Bindings, Result, Options) :-  
    (   is_index(Index)
    ->  empty_queue,
      	thread_self(Me),
      	(   session_data(CacheId, previous_request(Query0, Index0)),
      	    Query =@= Query0, greater_than(Index, Index0),
      	    try(thread_send_message(CacheId, command(Me, next(Index)))),
      	    New = true
      	->  true
      	;   psc_flush_soft(CacheId),
      	    (   option(thread_pool(Pool), Options)
      	    ->  thread_create_in_pool(Pool,
                    solve_2(Query, Bindings, Index, Me), _,
      	                [ alias(CacheId)
      		            ])
      	    ;   thread_create(
                    solve_2(Query, Bindings, Index, Me), _,
      	                [ alias(CacheId)
      		            ])
      		),  
      		New = false
      	),
      	option(time_limit(SearchTimeout), Options, 0),
      	get_result(CacheId, SearchTimeout, Result0),
      	result_cache(Result0, New),
      	result_to_pairlist(Result0, Result),
      	session_retractall(CacheId, previous_request(_, _)),
      	session_assert(CacheId, previous_request(Query, Index)),
      	(   option(abort_thread_after(CacheTimeout), Options)
      	->  psc_expire(CacheId, CacheTimeout)
      	;   true
      	)
    ;   Result = [error= index_error, message='Index error', cache= @false, time=0]
    ).


is_index(I-J) :- !,
    integer(I),
    integer(J),
    I > 0,
    J >= I.
is_index(I) :-
    integer(I),
    I > 0.


greater_than(I-_, _-J0) :- !, I > J0.
greater_than(I, _-J0) :- !, I > J0.
greater_than(I-_, J0) :- !, I > J0.
greater_than(I, I0) :- I > I0.
	

get_result(CacheId, SearchTimeout, Result) :-
    (   SearchTimeout == 0
    ->  thread_get_message(Result)
    ;   catch(
            call_with_time_limit(
                SearchTimeout,
                thread_get_message(Result)
            ),
            Error,
            (   psc_flush(CacheId), 
                Result = result(error, Error, false, _, SearchTimeout) 
            )
        )
    ).


result_to_pairlist(result(error, E, _N, C, T), PL) :- !,
    extract_error(E, Error),
    message_to_string(E, Msg),
    PL = [error=Error, message=Msg, cache= @C, time=T].
result_to_pairlist(result(true, B, N, C, T), PL) :- !,
    PL = [success= @true, message=yes, bindings=B, more= @N, cache= @C, time=T].
result_to_pairlist(result(false, _B, _N, C, T), PL) :-
    PL = [success= @false, message=no, cache= @C, time=T].


extract_error(error(Type,_), Error) :- !,
    functor(Type, Error, _).
extract_error(Error, Error).


result_type( result(_,_,T,_,_), T).
result_cache(  result(_,_,_,T,_), T).
result_time( result(_,_,_,_,T), T).


:- dynamic s_data/2.

session_data(CacheId, Clause) :-
    s_data(CacheId, Clause).

session_retract(CacheId, Clause) :-
    retract(s_data(CacheId, Clause)).

session_retractall(CacheId, Clause) :-
    retractall(s_data(CacheId, Clause)).

session_assert(CacheId, Clause) :-
    assert(s_data(CacheId, Clause)).



%%	empty_queue
%
%	Empty the thread's message queue. Just to be sure we are not hit
%	by stuff left from another session.

empty_queue :-
	thread_peek_message(_), !,
	thread_get_message(_),
	empty_queue.
empty_queue.



%%	psc_expire(+ThreadID, +Integer)
%
%	Start running the timer for expiring the Prolog cache.	

psc_expire(CacheId, CacheTimeout) :-    
    (   session_data(CacheId, tid(TID0))
    ->  remove_alarm(TID0),
        session_retract(CacheId, tid(TID0))
    ;   true
    ),
    alarm(CacheTimeout, psc_flush_soft(CacheId), TID, []),
  	session_assert(CacheId, tid(TID)).
  	


%%	try(+Goal)
%
%	Succeeds iff Goal succeds without raising an exception

try(Goal) :-
    catch(Goal, E, true),
    var(E).
    
    
%%	psc_flush_soft(+ThreadID)
%
%	Stop a running thread. Make sure it is stopped before continuing.	

psc_flush_soft(ThreadID) :-
    thread_self(Me),
    catch(thread_send_message(ThreadID, command(Me, stop)),_,true),
    catch(thread_join(ThreadID, _Status), _, true).


psc_flush(ThreadID) :-
    catch(thread_signal(ThreadID, abort), _, true),
    catch(thread_join(ThreadID, _Status), _, true).
    
    

%%	solve_2(:Goal, +Bindings, +ThreadID) is det.
%
%	Generate for Goal and send them  to   the  client. For the first
%	call, it is started by first/1  and   sends  the  result back to
%	first/1. If Goal is re-satisfied, the   call  comes from next/1.
%	Note that next/1 can be  executed   in  a  different thread, and
%	therefore next/1 passes the thread with the command-term.
%
%	We use non-backtrackable mutable state   to  preserve the client
%	thread-id while backtracking over  Goal.   This  can be achieved
%	using a predicate declared using   thread_local/1  or, as below,
%	using non-backtrackable destructive assignment on a structure.

solve_2(Goal, Bindings, Index, ThreadID) :-
	thread_self(Me),
	thread_statistics(Me, cputime, T0a),
	State = client(ThreadID, T0a),
	solve_3(Goal, Bindings, Index, Result),
	State = client(Client, T0),
	thread_statistics(Me, cputime, T1),
	Time is T1 - T0,
	result_time(Result, Time),
	nb_setarg(2, State, T1),
	debug(swapp, 'Sending: ~q', [Result]),
	thread_send_message(Client, Result),
	result_type(Result, Type),
	(   Type == false
	->  true
	;   Type == true
	->  debug(swapp, 'Waiting...', []),
	    thread_get_message(command(From, Command)),
	    debug(swapp, 'Command: ~q', [Command]),
	    nb_setarg(1, State, From),
	    (   Command = next(I1-J1)
	    ->  nb_setval(i, I1),
	        nb_setval(j, J1),
	        fail
        ;   Command = next(I1)
	    ->  nb_setval(i, I1),
	        fail
	    ;   Command == stop,
	        debug(swapp, 'Stopped: ~q', [])
	    )
	;   true
	).
	



%%	solve_3(:Goal, +Bindings, -Result) is nondet.
%
%	Solve Goal. This predicate catches   errors  and detects whether
%	Goal succeeded deterministically.


solve_3(Goal, Bindings, Index, Result) :-
	call_cleanup(catch(solve_4(Bindings, Goal, Index, BindingsList), E, true), Det=true),
	(   var(E)
	->  (   Det == true
	    ->	Sol = false
	    ;	Sol = true
	    ),
	    Result = result(true, BindingsList, Sol, _, _)
	;   Result = result(error, E, false, _, _)
	).
solve_3(_Goal, _Bindings, _Index, result(false, [], false, _, _)).


% prolog_state_caching: solve_4([], (repeat,fail), 1, 2, BindingsList)

solve_4(Bindings, Goal, I-J, BindingsList) :- !,
    nb_setval(i, I),
    nb_setval(j, J),
    nb_setval(c, 0),
    (   call(Bindings^Goal),
        nb_getval(c, C1),
        C2 is C1 + 1,
        nb_setval(c, C2),
        nb_getval(i, I1),
        nb_getval(j, J1),
        C2 >= I1,
        recordz(sols, Bindings),
        C2 == J1
    ;   true
    ),
    findall(B, (recorded(sols, B, Ref), erase(Ref)), BindingsList),
    BindingsList \= [].
solve_4(Bindings, Goal, I, Bindings) :- 
    nb_setval(i, I),
    nb_setval(c, 0),
    call(Bindings^Goal),
    nb_getval(c, C1),
    C2 is C1 + 1,
    nb_setval(c, C2),
    nb_getval(i, I1),
    C2 == I1.
    
    
    


    

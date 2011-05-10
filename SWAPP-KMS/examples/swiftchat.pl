:- module(swiftchat,[
    init/1,
    enter/2,
    heartbeat/0,
    add_to_chat/2
    ]).

:- use_module(library('http/json')).
:- use_module(library('http/http_json')).
:- use_module(library('http/http_session')).
:- use_module(library(porter_stem)).
:- use_module(library(debug)).

:- use_module('../lib/swapp_predicates').

:- dynamic nick/2.

nick(none, 'Eliza').

init(Status) :-
    http_session_id(SessionId),
    (   nick(SessionId, NickName)
    ->  % Client is already present
        findall(N, nick(_, N), Ns),
        Status = status(1, NickName, Ns)
    ;   % Inform client that it needs to enter the chat room
        Status = status(0)
    ).


enter(NickName, Participants) :-
    findall(N, nick(_, N), Participants),
    http_session_id(SessionId),
    assert(nick(SessionId, NickName)),
    swapp_session_eq_send_to_all(entered(NickName)).


add_to_chat(NickName, Text) :-
    (   swapp_current_session_eq(Queue),
	    swapp_session_eq_send(Queue, msg(NickName, Text)),
	    % Eliza only talks if she and you are on your own
	    findall(., nick(_, _), [_,_]),
	    downcase_atom(Text, LowerCaseText),
	    tokenize_atom(LowerCaseText, Stimuli),
	    eliza(Stimuli, Response),
	    atomic_list_concat(Response, ' ', Message),
	    swapp_session_eq_send(Queue, msg('Eliza', Message)),
	    fail % if not earlier, backtrack here!
	;   true
	).


%%	heartbeat
%
%	Removes the nickname associated with the current session from the room
%   after 25 seconds unless it is called again (and again and again...)

heartbeat :-
    swapp_heartbeat(25, stop_client).


%%	swapp_heartbeat(+Time, :Goal)
%
%	Runs Goal after Time seconds unless swapp_heartbeat/2 is called again (and again and again...)
%   Typically used to perform some kind of cleanup after a client has unloaded a page
%   Should perhaps be moved to swapp_predicates ??

swapp_heartbeat(Time, Goal) :-
    (   http_session_retract(current_heartbeat_alarm(ID0))
    ->  remove_alarm(ID0)
    ;   true
    ),
    alarm(Time, Goal, Id, [remove(true)]),
    http_session_assert(current_heartbeat_alarm(Id)).



stop_client :-
    http_session_id(SessionId),
    stop_client(SessionId).

stop_client(SessionId) :-
	retract(nick(SessionId, NickName)),
	swapp_session_eq_send_to_all(left(NickName)).



:- listen(http_session(end(SessionId, _Peer)),
	  stop_client(SessionId)).




%%	eliza(+Stimuli, Response) is det.
%
%	@param	Stimuli is a list of atoms (words).
%	@author Richard A. O'Keefe (The Craft of Prolog) (Modified by Torbjörn Lager)

eliza(Stimuli, Response) :-
    template(InternalStimuli, InternalResponse),
    match(InternalStimuli, Stimuli),
    match(InternalResponse, Response),
    !.

template([w(Greeting)], [s([hello, there, NickName,'.',how,are,you,'?'])]) :-
    member(Greeting, [hello, hi, hey]),
    http_session_id(SessionId),
    nick(SessionId, NickName).
template([w(yes)], [s([so,you,think,so])]).
template([w(no)], [s(['don\'t',be,negative])]).
template([s([why,?])], [s([i,am,the,one,who,asks,questions,here,!])]).
template([s([i,am]),s(X)], [s([why,are,you]),s(X),w('?')]).
template([w(i),s(X),w(my),s(Y)], [w(your),s(Y),s(['?',why,do,you]),s(X),w(it),w('?')]).
template([w(i),s(X),w(you)], [s([why,do,you]),s(X),w(me),w('?')]).
template([s(_X),w(FamilyMember),s(_Y)], [s([tell,me,more,about,your,family])]) :-
    member(FamilyMember, [mother, father, brother, sister]).
template([s(_),w(Word),w(Word),s(_)], [s([Word, Word, '.', 'You',stutter,'!'])]).
template([s(_X)], [s([oh,NickName,please,tell,me,more,'...'])]) :-
    http_session_id(SessionId),
    nick(SessionId, NickName).


match([],[]).
match([Item|Items],[Word|Words]) :-
    match(Item, Items, Word, Words).

match(w(Word), Items, Word, Words) :-
    match(Items, Words).
match(s([Word|Seg]), Items, Word, Words0) :-
    append(Seg, Words1, Words0),
    match(Items, Words1).




:- module(auth,
	  [ authorized/2
	  ]).
	  
:- use_module(library('http/http_authenticate')).

:- use_module(user_db).

:- dynamic deny/5.

authorized(Method, Request) :-
	memberchk(path(Path), Request),
    (   http_authenticate(basic(passwords), Request, Fields)
    ->  memberchk(user(User), Fields)
    ;   logged_on(User)
    ->  true
    ;   User = anonymous 
    ),
    (   user(User, UserProperties),
	    memberchk(roles(Roles), UserProperties),
	    member(Role, Roles),
        (   allow(Role, User, Method, Path, Request)
        ->  (   deny(Role, User, Method, Path, Request)
            ->  throw(http_reply(authorise(basic, 'secure')))
            ;   Done = true
            )
        ;   throw(http_reply(authorise(basic, 'secure')))
    	),
    	Done == true, !
    ;   % Deny is default
        throw(http_reply(authorise(basic, 'secure'))) 
    ).


allow(admin, _, post, '/swapp/admin/users', _).
allow(admin, _, delete, '/swapp/admin/users', _).
allow(_, _, get, '/swapp/admin/users', _).

allow(admin, _, post, '/swapp/admin/settings', _).
allow(_, _, get, '/swapp/admin/settings', _).

allow(_, _, get, '/swapp/admin/statistics', _).

allow(_, _, _, '/swapp/rdf/db', _).
allow(_, _, _, '/swapp/session/db', _).
allow(_, _, _, '/swapp/session/eq', _).

%For KMS
allow(_, _, _, '/swapp/kms/db', _).
allow(_, _, _, '/swapp/admin', _).

% Deny if user is using a Safari browser
% deny(_, _, _, _, Request) :-
%     memberchk(user_agent(UserAgent), Request),
%     sub_atom(UserAgent, _, _, _, 'Safari').

% Deny if too late in the night (server-side)
% deny(_, _, _, _, Request) :-
%     get_time(TimeStamp), 
%     stamp_date_time(TimeStamp, DateTime, local),
%     date_time_value(hour, DateTime, Hour),
%     Hour >= 22.

% Deny if too much content   
% deny(_, _, Method, _, Request) :-
%     memberchk(Method, [put, post]),
%     memberchk(content_length(Bytes), Request),
%     Bytes > 1000.

% Deny if no quota   
% :- dynamic quota/2.    
% deny(_, User, Method, Path, _) :-
%     (   quota(User, N)
%     ->  (   N > 0
%         ->  N1 is N - 1,
%             retractall(quota(User, _)),
%             assert(quota(User, N1)),
%             fail
%         ;   true
%         )
%     ;   assert(quota(User, 4)),
%         fail
%     ).

:- debug(authorization).

:- module(user_db,
	  [ set_user_database/1,	% +File

	    user/2,			    % +Name, +Properties

	    user_add/2,			% +Name, +Properties
	    user_del/1,			% +Name,

	    user_property/2,		% ?Name, ?Property

	    login/1,			% +User
	    logout/1,			% +User
	    current_user/1,		% ?User
	    logged_on/1  		% -User
	  ]).

:- use_module(library('http/http_session')).
:- use_module(library('http/http_wrapper')).
:- use_module(library(debug)).

:- use_module(db).



/** <module> User administration

Core user administration. The  user  administration   is  based  on  the
following:

	* A persistent fact user/2
	* A dynamic fact logged_in/3
	* Session management

@author	Jan Wielemaker, modified by Torbjörn Lager
*/

:- dynamic
	logged_in/3,			% Session, User, Time
	user/2.


		 /*******************************
		 *	  USER DATABASE		*
		 *******************************/

:- db_term user(_Name, _UserOptions).

%%	set_user_database(+File) is det.
%
%	Load user/2 from File.  Changes are fully synchronous.

set_user_database(File) :-
	db_attach(File, [sync(close)]).

%%	user_add(+Name, +Properties) is det.
%
%	Add a new user with given properties.

user_add(Name, Options) :-
	must_be(atom, Name),
	db_assert(user(Name, Options)).

%%	user_del(+Name)
%
%	Delete named user from user-database.

user_del(Name) :-
	must_be(atom, Name),
	(   user(Name, _)
	->  db_retractall(user(Name, _))
	;   existence_error(user, Name)
	).
	
		 /*******************************
		 *	     USER QUERY         *
		 *******************************/

%%	current_user(?User)
%	
%	True if User is a registered user.

current_user(User) :-
	user(User, _).

%%	user_property(?User, ?Property) is nondet.
%%	user_property(+User, +Property) is semidet.
%	
%	True if Property is a defined property on User.  In addition to
%	properties explicitely stored with users, we define:
%	
%		* session(SessionID)
%		* connection(LoginTime, Idle)

user_property(User, Property) :-
	nonvar(User), nonvar(Property), !,
	uprop(Property, User), !.
user_property(User, Property) :-
	uprop(Property, User).

uprop(session(SessionID), User) :-
	(   nonvar(SessionID)		% speedup
	->  !
	;   true
	),
	logged_in(SessionID, User, _).
uprop(connection(LoginTime, Idle), User) :-
	logged_in(SessionID, User, LoginTime),
	http_current_session(SessionID, idle(Idle)).
uprop(Prop, User) :-
	user(User, Properties),
	member(Prop, Properties).


		 /*******************************
		 *	 LOGIN                      *
		 *******************************/

%%	logged_on(-User) is det.
%
%	True if User is the name of the currently logged in user.


logged_on(User) :-
	http_session_id(SessionID),
	user_property(User, session(SessionID)).



%%	login(+User:atom) is det.
%	
%	Accept user as a user that has logged on into the current
%	session.

login(User) :-
	must_be(atom, User),
	get_time(Time),
	http_session_id(Session),
	retractall(logged_in(_, Session, _)),
	assert(logged_in(Session, User, Time)),
	debug(login, 'Login user ~w on session ~w', [User, Session]).


%%	logout(+User) is det.
%	
%	Logout the specified user

logout(User) :-
	must_be(atom, User),
	retractall(logged_in(_Session, User, _Time)),
	debug(login, 'Logout user ~w', [User]).
	
:- debug(authorization).
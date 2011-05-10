:- module(users_api, []).


/***************************************************
* load_modules
***************************************************/

% http library modules
:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/http_json')).
:- use_module(library('http/http_client')).
:- use_module(library('http/http_authenticate')).
:- use_module(library('http/http_session')).

:- use_module('../lib/user_db').
:- use_module('../lib/auth').
:- use_module('../lib/update_passwd').
:- use_module('../lib/term_to_json').
:- use_module('../lib/dispatch').
:- use_module(library(settings)).
:- use_module(library(debug)).



/***************************************************
* http handlers
***************************************************/

:- http_handler(root('swapp/admin/users'), dispatch, []).

/***************************************************
* handle reply
***************************************************/

%%	dispatch_method(+Method, +Request)

dispatch_method(post, Request) :-
    http_parameters(Request,
        [ user(Name, []),
          password(Password, [])
  		]),
    http_read_data(Request, OptionsAtom, [to(atom)]),
    catch(atom_to_term(OptionsAtom, Options, _Bindings), E, true),
    (   var(E)
    ->  (   \+ current_user(Name)
        ->  user_add(Name, Options),
            add_passwd('passwords', Name, Password),
            reply_json(json([ok= @true]), [width(0)])
        ;   reply_json(json([error='Existing user']), [width(0)])
        )
    ;   reply_json(json([error='Malformed option list']), [width(0)])
    ).
dispatch_method(delete, Request) :-
    http_parameters(Request,
        [ user(Name, [])
  		]),
    catch((user_del(Name), remove_passwd('passwords', Name)), E, true),
    (   var(E)
    ->  reply_json(json([ok= @true]), [width(0)])
    ;   message_to_string(E, Msg),
	    reply_json(json([error=Msg]), [width(0)])
	).
dispatch_method(get, Request) :-
    http_parameters(Request,
        [ user(User, [default('_')])
  		]),
  	(   User == '_'
  	->  list_users(_, List)
    ;   list_users(User, List)
  	),
    reply_json(json(List), [width(0)]).


list_users(User, List) :-
    findall(User=Properties, (
            current_user(User),
            findall(Property, (
                user_property(User, Prop),
                term_to_atom(Prop, Property)
            ), Properties)
    ), List).

:- set_user_database('user.db').

%:- debug(users).



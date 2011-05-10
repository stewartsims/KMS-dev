:- module(login_api, []).


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

:- use_module(library(settings)).
:- use_module(library(debug)).

:- use_module('../lib/user_db').
    

/***************************************************
* http handlers
***************************************************/

:- http_handler(root('swapp/login'), dispatch, []). 


/***************************************************
* handle reply
***************************************************/

dispatch(Request) :-
    memberchk(method(Method0), Request),
    % method overriding
    http_parameters(Request,
        [ 'http_method'(Method, [default(Method0)])
  	    ]),
  	dispatch_method(Method, Request).
  	
  	
%%	dispatch_method(+Method, +Request)	
%
%	Handling of POST and DELETE on /swapp/admin/login.

dispatch_method(post, Request) :-
    http_authenticate(basic(passwords), Request, Fields),
    memberchk(user(User), Fields),
    login(User),
    reply_json(json([ok= @true, msg=User]), [width(0)]).
dispatch_method(delete, _Request) :-
    logged_on(User),
    logout(User),
    reply_json(json([ok= @true, msg=User]), [width(0)]).
	
	
    
    
    


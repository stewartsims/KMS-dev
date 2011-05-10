:- module(swapp_dispatch,
	  [ dispatch/1
	  ]).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(auth).

:- meta_predicate
	dispatch(:).

%%	dispatch(+Request)
%
%

dispatch(Module:Request) :-
    memberchk(method(Method0), Request),
    % method overriding
    http_parameters(Request,
		    [ 'http_method'(Method, [default(Method0)])
		    ]),
    authorized(Method, Request),
    catch(Module:dispatch_method(Method, Request),
	  Error,
	  return_error(Error)
	 ).


return_error(E) :-
    extract_error(E, Error),
    message_to_string(E, Msg),
    reply_json(json([error=Error, message=Msg]), [width(0)]).

extract_error(error(Type,_), Error) :- !,
    functor(Type, Error, _).
extract_error(Error, Error).

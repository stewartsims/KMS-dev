:- module(settings_api, []).


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
:- use_module('../lib/dispatch').
:- use_module(library(settings)).
:- use_module(library(debug)).



/***************************************************
* http handlers
***************************************************/

:- http_handler(root('swapp/admin/settings'), dispatch, []).


/***************************************************
* handle reply
***************************************************/

dispatch_method(post, Request) :-
    http_parameters(Request,
        [ module(Module, []),
          setting(Setting, [])
  		]),
    http_read_data(Request, Data, [to(atom)]),
    (   setting_property(Module:Setting, type(Type)),
        atom_to_value(Type, Data, Value),
        catch(set_setting(Module:Setting, Value), E, true),
        (   var(E)
        ->  save_settings('settings.db'),
            settings_all(Module:Setting, Json),
            reply_json(Json, [width(0)])
        ;   message_to_string(E, Msg),
            reply_json(json([ok= @false, error=Msg]), [width(0)])
        )
    ;   reply_json(json([ok= @false, error='No such module or setting.']), [width(0)])
    ).
dispatch_method(get, Request) :-
    http_parameters(Request,
        [ module(Module, [default(_)]),
          setting(Setting, [default(_)])
  		]),
  	(   \+ \+ current_setting(Module:Setting)
  	->  settings_all(Module:Setting, Json),
        reply_json(Json, [width(0)])
    ;   reply_json(json([ok= @false, error='No such module or setting.']), [width(0)])
    ).


% This isn't enought!
atom_to_value(Type, Data, Value) :-
    (   Type == number
    ->  atom_to_term(Data, Value, _)
    ;   Type == nonneg
    ->  atom_to_term(Data, Value, _)
    ;   Value = Data
    ).

settings_all(Module:Setting, json(SList)) :-
    findall(Module=SJson, (current_setting(Module:_),settings_to_json(Module:Setting, SJson)), SList).

settings_to_json(Module:Setting, json(SList)) :-
    findall(Setting=SJson, setting_to_json(Module:Setting, SJson), SList).

setting_to_json(Module:Setting, Json) :-
    setting_property(Module:Setting, comment(Comment)),
    setting_property(Module:Setting, type(Type)),
    type_to_json(Type, JsonType),
    setting_property(Module:Setting, default(Default)),
    setting(Module:Setting, Value),
    Json = json([comment=Comment, type=JsonType, default=Default, value=Value]).


% This is likely not complete
type_to_json(oneof(List), List) :- !.
type_to_json(Type, Type).








/*
 * exportKMSdb is a simple module to export the full listing of the KMS database to
 * a file called fullKMSdb
 * The method is exposed through the web service
 */

:- module(export_kms_db, []).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).

:- http_handler(root(export_kms_db), exportKMSdb, []).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

export_kms_db(Request) :-
	http_read_json(Request, JSONIn),
        json_to_prolog(JSONIn, PrologIn),
	%if request body = 'exportKMS' write KMS db to file
	(PrologIn='exportKMS'-> 
		(telling(Stream),tell('../data/fullKMSdb'),listing(kms:_),told,tell(Stream),
		prolog_to_json('{ status: success }',JSONOut),
		reply_json(JSONOut));
	(prolog_to_json('{ status: failure }', JSONOut),
	 reply_json(JSONOut))).	


/*
 * Knowledge Entity Bearer, not yet tested
 */
:- module('ke_bearer', []).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).

:- http_handler(root(bearEntity), bearEntity, []).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

readAllTerms(InputStream,_):- read(InputStream,end_of_file).
readAllTerms(InputStream,OutputTermList) :- 
	read(InputStream,Term),
	append(Term,OutputTermList,OutputTermList),
	readAllTerms(InputStream,OutputTermList).

bearEntity(Request) :-
	http_read_json(Request, JSONIn),
       json_to_prolog(JSONIn, PrologIn),
	%save agent and then open
	atom_concat('../data/KA/',PrologIn,File),
	saveKA(File),
	read_file_to_terms(File, Terms, []),
	prolog_to_json(Terms,JSONOut),
	reply_json(JSONOut).
	%prolog_to_json('{ status: failure }', JSONOut),
	%reply_json(JSONOut)).	

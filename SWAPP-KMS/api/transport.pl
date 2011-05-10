/*
 * Transport is a module to facilitate safe KA communication to the KMS-Transport
 * RESTful web service for external data retrieval
 */

:- module('transport', []).

:- use_module(logger).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/json)).

:- json_object
	transport(transportId:atom, queryPayload:String).

:- json_object
	data_request(method:atom, id:String).

request(TransportId, QueryPayload, Reply) :-
	prolog_to_json(transport(TransportId, QueryPayload), JSON),
	http_post('http://localhost:8080/KMS-Interface/TransportServlet',json(JSON), Reply, []),
	logger:logAtLevel('D',Reply).
	
constructQueryPayload( Method, Id, QueryPayload ) :-
	prolog_to_json(data_request(Method,Id),QueryPayload).

parseArray( JsonArray, PrologList ) :-
	atom_json_term(JsonArray, PrologList, []). 
		
parseResponse( Response, JsonParameters, PrologTerms ) :-
	length( JsonParameters, NumberParameters ),
	parseResponseParametersAndValues( Response, NumberParameters, ResponseParameters, ResponseValues ),
	findParameters( ResponseParameters, ResponseValues, JsonParameters, PrologTerms ).

readResponse( Response, JsonParameters, PrologTerms ) :-
	length( JsonParameters, NumberParameters ),
	readResponseParametersAndValues( Response, NumberParameters, ResponseParameters, ResponseValues ),
	findParameters( ResponseParameters, ResponseValues, JsonParameters, PrologTerms ).
	
parseResponseParametersAndValues( Response, NumberParameters, ResponseParameters, ResponseValues ) :-
	generateNameValueLists( 0, NumberParameters, Pairs, ResponseParameters, ResponseValues ),
	atom_json_term(Response, json(Pairs), []).

readResponseParametersAndValues( Response, NumberParameters, ResponseParameters, ResponseValues ) :-
	generateNameValueLists( 0, NumberParameters, Pairs, ResponseParameters, ResponseValues ),
	Response = json(Pairs).
	
generateNameValueLists( End, End, [], [], [] ) :- !.
generateNameValueLists( End, Count, [Pair | Pairs], [Parameter | Parameters], [Value | Values] ) :-
	Pair = (Parameter = Value),
	CountD is Count-1,
	generateNameValueLists( End, CountD, Pairs, Parameters, Values ).

findParameters( _, _, [], [] ).
findParameters( ResponseParameters, ResponseValues, [JsonParameter | JsonParameters], [PrologTerm | PrologTerms] ) :-
	nth0(Index, ResponseParameters, JsonParameter),
	nth0(Index, ResponseValues, PrologTermPre),
	(
		(PrologTermPre = @true -> PrologTermPre2 = true; PrologTermPre2 = PrologTermPre),
		(PrologTermPre2 = @false -> PrologTermPre3 = false; PrologTermPre3 = PrologTermPre2),
		(PrologTermPre3 = '_' -> PrologTerm = _; PrologTerm = PrologTermPre3)
	),
	findParameters( ResponseParameters, ResponseValues, JsonParameters, PrologTerms ).
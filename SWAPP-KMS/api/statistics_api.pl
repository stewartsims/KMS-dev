:- module(statistics_api, []).


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

:- http_handler(root('swapp/admin/statistics'), dispatch, []).

/***************************************************
* handle reply
***************************************************/

dispatch_method(get, _Request) :-
    server_statistics(ServerJson),
    semweb_statistics(SemWebJson),
    reply_json(json([server=ServerJson, rdfdb=SemWebJson]), [width(0)]).


server_statistics(Json) :-
    gethostname(Host),
    swapp_server_property(port(Port)),
    swapp_server_property(started(StartTime)),
    format_time(atom(StartTimeAtom), '%+', StartTime),
    http_workers(Port, NWorkers),
    findall(ID, http_current_worker(Port, ID), Workers),
    statistics(heapused, Heap),
	Json = json([host=Host,
	             port=Port,
	             starttime=StartTimeAtom,
	             nworkers=NWorkers,
	             workers=Workers,
	             heapused=Heap]).

semweb_statistics(Json) :-
    rdf_statistics(triples(Total)),
    rdf_statistics(core(Core)),
	findall(File=Triples,
		rdf_statistics(triples_by_file(File, Triples)),
		UnsortedPairs),
	findall(IndexAtom=Count,
		(rdf_statistics(lookup(Index, Count)), term_to_atom(Index, IndexAtom)),
		Lookup),
%	findall(S, session(S), Sessions0),
	Json = json([triples=Total,
	             core=Core,
	             triples_by_file=json(UnsortedPairs),
	             lookup_stats=json(Lookup)]).

session(s(Idle, User, SessionID, Peer)) :-
	http_current_session(SessionID, peer(Peer)),
	http_current_session(SessionID, idle(Idle)),
	(   user_property(User, session(SessionID))
	->  true
	;   User = (-)
	).

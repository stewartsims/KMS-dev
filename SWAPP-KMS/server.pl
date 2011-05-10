:- module(swapp,
    [ server/1,
      swapp_load_plugins/0,
      swapp_server_property/1	% -Property
    ]).


:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(settings)).
:- use_module(library(thread_pool)).
:- use_module(library(debug)).

:- use_module('lib/www').

:- use_module('api/login_api').
:- use_module('api/users_api').
:- use_module('api/settings_api').
:- use_module('api/statistics_api').
:- use_module('api/rdfdb_api').
:- use_module('api/sessioneq_api').
:- use_module('api/sessiondb_api').

%KMS modules
:- use_module('api/admin').
:- use_module('api/kms_db').
:- use_module('api/export_kms_db').
:- use_module('api/ke_bearer').
:- use_module('api/safe_file_persistence').
:- use_module('api/ka_manager').
:- use_module('api/logger').
:- use_module('api/kms_populate_ka').
:- use_module('api/transport').
:- logger:setLogLevel('E').
:- kms_db:setDatabaseModuleName(kms).
:- kms_populate_ka:populateKMSwithKAs('data/KA').

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

:- (   user:file_search_path(swapp, _)
   ->  true
   ;   prolog_load_context(directory, Dir),
       asserta(user:file_search_path(swapp, Dir))
   ).

user:file_search_path(www,	     swapp(www)).
user:file_search_path(swapp_plugins, swapp(plugins)).


%%	swapp_load_plugins is det.
%
%	Find  .pl  files  in  all  directories  search  through  by  the
%	file-search-path =swapp_plugins= and load them.

swapp_load_plugins :-
	forall(absolute_file_name(swapp_plugins(.),
				  Dir,
				  [ file_type(directory),
				    file_errors(fail),
				    solutions(all)
				  ]),
	       load_plugin_dir(Dir)).

load_plugin_dir(Dir) :-
	atom_concat(Dir, '/*.pl', Pattern),
	expand_file_name(Pattern, Files),
	load_files(Files, [if(changed)]).


:- multifile
	startup/0.

:- dynamic
	start_time/1,			% Stamp
	loading/0,
	loading_done/2.			% Graphs, Total (not used)


%%	server(?Port) is det.
%
%	Start the SWAPP server on Port.

server(Port) :-
	load_settings('settings.db'),
	forall(startup, true),
	thread_pool_create(cheapthreads, 90, []),
	http_server(swapp_server,
		    [ port(Port),
		      workers(8)
		    ]),
	get_time(Time),
	assert(start_time(Time)),
	print_message(informational, swapp(started(Port))).


swapp_server(_Request) :-
	loading, !,
	rdf_statistics(triples(Triples)),
	(   loading_done(Nth, Total)
	->  Extra = [ '; ~D of ~D graphs.'-[Nth, Total] ]
	;   Extra = [ '.' ]
	),
	HTML = p([ 'This service is currently restoring its ',
		   'persistent database.', br([]),
		   'Loaded ~D triples'-[Triples]
		 | Extra
		 ]),
	throw(http_reply(unavailable(HTML))).
swapp_server(Request) :-
	http_dispatch(Request).


%%	swapp_server_property(?Property)
%
%	Query status and attributes of the server. Defined properties
%	are:
%
%		* port(-Port)
%		Port on which the server is running.
%
%		* start_time(-Time)
%		TimeStamp when the server was started.

swapp_server_property(port(Port)) :-
	http_current_server(swapp_server, Port).
swapp_server_property(started(Time)) :-
	start_time(Time).


%	Declare HTTP locations we serve and how.

:- http_handler(root(www), serve_documents_from(www),
        [ prefix
        ]).

:- http_handler(root(swapp),
		http_redirect(moved, '/www/site/home.html'),
		[]).


:- multifile
	prolog:message//1.

prolog:message(swapp(started(Port))) -->
	[ 'You can access the server at http://localhost:~w/swapp'-[Port] ].

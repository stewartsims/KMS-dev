:- op(1200, xfy, '--->').


:- load_files([ server,
		examples/queens,
		examples/swiftchat,
		examples/prolog,
		examples/haiku,
		examples/grammar,
		examples/parser,
		examples/wordnet,
		library(semweb/rdf_db),
		library(semweb/rdf_zlib_plugin),
		library(http/http_dispatch)
	      ],
	      [ if(not_loaded),
		silent(true)
	      ]).

:- http_handler(/, http_redirect(moved, root(swapp)), []).

load_wordnet :-
	rdf_graph('http://thesauri.cs.vu.nl/wordnet/rdfs/wordnet1a.rdf'), !.
load_wordnet :-
	rdf_load('www/data/wn20assem/wordnet1a.rdf',
		 [ graph('http://thesauri.cs.vu.nl/wordnet/rdfs/wordnet1a.rdf')]),
	rdf_load('www/data/wn20assem/wordnet1a.rdfs',
		 [ graph('http://thesauri.cs.vu.nl/wordnet/rdfs/wordnet1a.rdfs')]).

% swapp:startup/0 can be used to run goals when the swapp server starts.
% Some of the demos depend on WordNet and therefore we load it into the
% persistent RDF store if it isn't there.

:- multifile
	swapp:startup/0.

swapp:startup :-
	load_wordnet.

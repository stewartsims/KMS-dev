:- module(www,
    [ serve_documents_from/2
    ]).

:- use_module(library(http/http_dispatch)).

serve_documents_from(PathAlias, Request) :-
        memberchk(path_info(PathInfo), Request),
	Term =.. [PathAlias, PathInfo],
        http_reply_file(Term, [], Request).

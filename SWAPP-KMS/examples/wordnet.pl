:- module(wordnet, [word/2]).
:- use_module(library(semweb/rdf_db)).

:- rdf_register_ns(wn, 'http://www.cogsci.princeton.edu/~wn#').

word(Word, Json) :-
    setof(json([id=S, pos=Pos, gloss=Gloss, words=Ws]), (
        rdf(S, wn:wordForm, literal(Word)),
        pos(S, Pos),
        rdf(S, wn:g, literal(Gloss)),
        findall(W, rdf(S, wn:wordForm, literal(W)), Ws)
    ), Synsets),
    Json = json([word=Word, synsets=Synsets]).


pos(S, n) :- rdf(S, rdf:type, wn:'Noun').
pos(S, v) :- rdf(S, rdf:type, wn:'Verb').
pos(S, a) :- rdf(S, rdf:type, wn:'Adjective').
pos(S, a) :- rdf(S, rdf:type, wn:'AdjectiveSatellite').
pos(S, r) :- rdf(S, rdf:type, wn:'Adverb').






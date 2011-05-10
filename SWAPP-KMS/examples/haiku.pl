:- module(haiku,
    [ haiku/1
    ]).


rdf_db: ns('ex','http://example.org/stuff/1.0/').


haiku([Line1, Line2, Line3]) :- 
    line1(Line1),
    line2(Line2),
    line3(Line3).

% Prep Det A N
line1(Line) :-
    random_closed_word(prep, W1, P1),
    random_closed_word(det, W2, P2),
    P1 + P2 =< 3,
    random_wordnet_word('http://www.cogsci.princeton.edu/~wn#Adjective', W3, P3), 
    P1 + P2 + P3 =< 4,
    random_wordnet_word('http://www.cogsci.princeton.edu/~wn#Noun', W4, P4),
    P1 + P2 + P3 + P4 =:= 5,
    atomic_list_concat([W1, W2, W3, W4], ' ', Line), !. 

% Det A N Part Vsg3
line2(Line) :-
    random_closed_word(det, W1, P1),
    random_wordnet_word('http://www.cogsci.princeton.edu/~wn#Adjective', W2, P2), 
    P1 + P2 =< 3,
    random_wordnet_word('http://www.cogsci.princeton.edu/~wn#Verb', W30, P30),
    make_ing_form(W30, W3), P3 is P30 + 1,
    P1 + P2 + P3 =< 4,
    random_wordnet_word('http://www.cogsci.princeton.edu/~wn#Noun', W4, P4),
    P1 + P2 + P3 + P4 =< 5,
    random_closed_word(part, W5, P5),
    P1 + P2 + P3 + P4 + P5 =< 6,
    random_wordnet_word('http://www.cogsci.princeton.edu/~wn#Verb', W60, P6),
    make_sg3_form(W60, W6),
    P1 + P2 + P3 + P4 + P5 + P6 =:= 7,
    atomic_list_concat([W1, W2, W3, W4, W5, W6], ' ', Line), !.
    
% Mod Adv
line3(Line) :-
    random_closed_word(mod, W1, P1),
    random_wordnet_word('http://www.cogsci.princeton.edu/~wn#Adverb', W2, P2), 
    P1 + P2  =:= 5,
    atomic_list_concat([W1, W2], ' ', Line), !.
 
    
:- dynamic words_cache/3.

random_wordnet_word(PoS, Word, NSyllables) :-
    (   words_cache(PoS, Words, N)
    ->  true
    ;   findall(W, (
            rdf(S, rdf:type, PoS),
            rdf(S, 'http://www.cogsci.princeton.edu/~wn#wordForm', literal(W))
        ),
        Words),
        length(Words, N),
        assert(words_cache(PoS, Words, N))
    ),
    randseq(20, N, L), 
    member(I, L),
    nth1(I, Words, Word),
    count_syllables(Word, NSyllables).



count_syllables(Word, N) :-
    atom_chars(Word, Chars),
    count_syllables_chars(Chars, N).
    
count_syllables_chars([], 0) :- !.
count_syllables_chars([C,l,e], 1) :-
    consonant(C), !.
count_syllables_chars([C,e], 0) :- 
    consonant(C), !.
count_syllables_chars([C|Cs], N) :- 
    consonant(C), !,
    count_syllables_chars(Cs, N).
count_syllables_chars([_, C|Cs], N) :- 
    vowel(C), !,
    count_syllables_chars(Cs, NN), 
    N is NN + 1.
count_syllables_chars([_|Cs], N) :- 
    count_syllables_chars(Cs, NN), 
    N is NN + 1.


vowel(a).
vowel(e).
vowel(i).
vowel(o).
vowel(u).
vowel(y).

consonant(b).
consonant(c).
consonant(d).
consonant(f).
consonant(g).
consonant(h).
consonant(j).
consonant(k).
consonant(l).
consonant(m).
consonant(n).
consonant(p).
consonant(q).
consonant(r).
consonant(s).
consonant(t).
consonant(v).
consonant(x).
consonant(z).
consonant('_'). % fix


% If the verb ends in e, drop the e and add ing (if not exception).  
make_ing_form(Word, IngForm) :-
    \+ memberchk(Word, [be, see, flee, knee]),
    atom_concat(Prefix, e, Word), !,
    atom_concat(Prefix, ing, IngForm).
% If the verb ends in ie, change ie to y and add ing.  
make_ing_form(Word, IngForm) :-
    atom_concat(Prefix, ie, Word), !,
    atom_concat(Prefix, ying, IngForm).
% For 1 syllable words that end with a consonant-vowel-consonant, double the final letter before adding ing. 
make_ing_form(Word, IngForm) :-
    atom_chars(Word, Chars),
    count_syllables_chars(Chars, 1),
    append(Prefix, [C1, V, C2], Chars),
    consonant(C1), vowel(V), consonant(C2), !,
    append(Prefix, [C1, V, C2, C2, i, n, g], Chars2),
    atom_chars(IngForm, Chars2).
% By default just add ing
make_ing_form(Word, IngForm) :-
    atom_concat(Word, ing, IngForm).
    
    
    
% If the verb ends in y, remove it and add ies.
make_sg3_form(Word, SG3) :-
    atom_concat(Prefix, y, Word), 
    atom_concat(_, C, Prefix),
    consonant(C), !,
    atom_concat(Prefix, ies, SG3).
% If the verb ends in o, ch, s, sh, x or z, add es.
make_sg3_form(Word, SG3) :-
    member(Suffix, [o, ch, s, sh, x, z]),
    atom_concat(_Prefix, Suffix, Word), !,
    atom_concat(Word, es, SG3).
% By default just add s.
make_sg3_form(Word, SG3) :-
    atom_concat(Word, s, SG3).



random_closed_word(prep, W, P) :-
    prepositions(Prepositions),
    length(Prepositions, N),
    randseq(10, N, L),
    member(I, L),
    nth1(I, Prepositions, W-P).
random_closed_word(det, W, P) :-
    determiners(Determiners),
    length(Determiners, N),
    randseq(10, N, L),
    member(I, L),
    nth1(I, Determiners, W-P).
random_closed_word(part, W, P) :-
    particles(Particles),
    length(Particles, N),
    randseq(10, N, L),
    member(I, L),
    nth1(I, Particles, W-P).
random_closed_word(mod, W, P) :-
    modifiers(Modifiers),
    length(Modifiers, N),
    randseq(10, N, L),
    member(I, L),
    nth1(I, Modifiers, W-P).    



prepositions([in-1, on-1, to-1, around-2, besides-2, along-2, aboard-2, above-2, among-2, behind-2, inside-2, outside-2, under-2, without-2]).

determiners([a-1, any-1, each-1, her-1, his-1, my-1, one-1, our-1, some-1, the-1, their-1, this-1, your-1]).

particles([still-1]).

modifiers([extremely-3, heavily-3, awfully-3, seemingly-3, dreadfully-3, alarmingly-4, exceedingly-4]).











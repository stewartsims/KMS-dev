:- module(swapp_prolog,[
    swapp_predicate/1
    ]).
    
swapp_predicate(F/N) :-
    safe_code:safe_primitive(P),
    functor(P, F, N).
swapp_predicate(F/N) :-
    safe_code:safe_meta(P),
    functor(P, F, N).
    
    

 

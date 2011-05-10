:- module(kms_populate_ka,[]).

/*
 * kms_populate_ka provides a mechanism to be called on server startup
 * which loads all the Knowledge Agent modules found in the given directory
 * Additionally loadKA can be called seperately to load a module when required
 */
:- use_module(logger).
:- use_module(kms_db).

populateKMSwithKAs(Path) :- 
	directory_files(Path,Directory),
	delete(Directory,'.',Directory2),
	delete(Directory2,..,Filenames),
	loadKAs(Filenames).

loadKAs([]).
loadKAs([KA|Filenames]) :-
	loadKA(KA),
	loadKAs(Filenames).

loadKA(KA) :-
	kms_db:kms(DbName),
	DbName:KA:consult('data/KA'/KA),
	assert(ka_manager:ka_list(KA)).
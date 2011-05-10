/*
 * Knowledge Agent manager, handles creation, persistence, and removal for individial KAs
 * ka_list is a dynamic predicate which holds all KA identifiers
 * every valid KA has a fact asserted to it called 'ka_token' 
 */

:- module('ka_manager', []).

:- use_module(logger).
:- use_module(kms_db).

:- dynamic(ka_list/1).

%simple predicate that persists the Knowledge Agent to directory ./KA/GivenFileName
saveKA(KAIdentifier) :- 
	exists(KAIdentifier) ->
				(
					kms_db:kms(DbName),
					atom_concat('data/KA/',KAIdentifier,File),
					save_module_safely(DbName:KAIdentifier,File)
				);
	logger:logAtLevel('I','Tried to save KA which doesnt appear to exist'),fail.

%simple predicate that updates the Knowledge Agent at path ./KA/GivenFileName
updateKA(KAIdentifier,DataToAssert) :- 
	exists(KAIdentifier) ->
				(
					kms_db:kms(DbName),
					atom_concat('data/KA/',KAIdentifier,File),
					assert_and_save_safely(KAIdentifier,File,DataToAssert)
				);
	logger:logAtLevel('I','Tried to update KA which doesnt appear to exist'),fail.

newKA(KAIdentifier) :- 
			     exists(KAIdentifier) -> 
				 logger:logAtLevel('I','Tried to create KA using existing KAIdentifier'),fail;
			     (
                         kms_db:kms(DbName),
				 DbName:assert(KAIdentifier:ka_token),
			       assert(ka_list(KAIdentifier))
			     ).

removeKA(KAIdentifier) :- 	exists(KAIdentifier) ->
					(
					   kms_db:kms(DbName),
					   DbName:retract(KAIdentifier:ka_token),
					   retract(ka_list(KAIdentifier))
					);
					logger:logAtLevel('I','Tried to remove KA which doesnt appear to exist'),fail.


exists(KAIdentifier) :- catch(checkExistence(KAIdentifier),
				     E,
				     (
					   E=error(existence_error(_,_),_) -> 
					   false;
					   (logger:logAtLevel('D','Could not check existence of KA')),fail
				     )
				).

checkExistence(KAIdentifier) :- kms_db:kms(DbName),
			     		  DbName:KAIdentifier:ka_token,
					  ka_list(KAIdentifier).
					  %logger:logAtLevel('D','Existence check returned true'),fail.
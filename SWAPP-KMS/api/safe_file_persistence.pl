/*
 * safe_file_persistence
 * This module uses a single message queue in order to facilate safe
 * serial file operations for writing data to files.
 *
 */

:- module(safe_file_persistence,[]).

%:- use_module(logger).

:- dynamic file_queue/1.

setFileQueueName(Name) :- 	
	file_queue(_) -> file_queue(kms(_));
	assert(file_queue(Name)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Usage predicates						%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(QueueName) :-
	setFileQueueName(QueueName),
	begin(QueueName).

restart(QueueName) :-
	message_queue_destroy(QueueName),
	begin(QueueName).

save_module_safely(Module, FileName) :-
	file_queue(QueueName),
	write_file_safely(QueueName, Module, FileName).

assert_and_save_safely(Module,FileName,Data) :- 
	file_queue(QueueName),
	assert(Module:Data),
	catch(write_data_safely(QueueName,FileName,Data),_,
						(
							retract(Module:Data),
							logger:logAtLevel('E',
									  'Error writing data to file using 																				safe_file_persistence:assert_and_save_safely'),
							fail
				    		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Supporting code						 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_data(File, Data) :- 
	open(File, append, Stream),
	write(Stream,Data),
	catch(close(Stream),E,
			      (
				      close(Stream,[force(true)]),
				      throw(E)
			      )
	).

begin(Queue) :- 
	message_queue_create(Queue),
	thread_send_message(Queue, ready).

make_ready(Queue) :- 
	thread_send_message(Queue, ready).

check_ready(Queue) :- 
	thread_get_message(Queue, ready).

write_data_safely(Queue, File, Data) :-
	check_ready(Queue),
	catch(write_data(File, Data), E,
					(
						make_ready(Queue),
						throw(E)
					)
	),
	make_ready(Queue).

write_file(File, Module) :- 
	open(File, write, Stream),
	tell(File),
	listing(Module:_),
	told,
	tell(Stream),
	catch(close(Stream),E,
			      (
				      close(Stream,[force(true)]),
				      throw(E)
			      )
	).

write_file_safely(Queue, File, Module) :- 
	check_ready(Queue),
	write_file(File, Module),
	catch(write_file(File, Module), E,
					(
						make_ready(Queue),
						throw(E)
					)
	),
	make_ready(Queue).

%%% Following is to replace code in ka_manager

%simple predicate that updates the Knowledge Agent at path ./KA/GivenFileName
%updateKA(KAIdentifier,DataToAssert) :- 
%	exists(KAIdentifier) ->
%				(
%					kms_db:kms(DbName),
%					atom_concat('data/KA/',KAIdentifier,File),
%					assert_and_save_safely(KAIdentifier,File,DataToAssert)
%				);
%	logger:logAtLevel('I','Tried to update KA which doesnt appear to exist'),fail.

%simple predicate that persists the Knowledge Agent to path ./KA/GivenFileName
%saveKA(KAIdentifier) :- 
%	exists(KAIdentifier) ->
%				(
%					kms_db:kms(DbName),
%					atom_concat('data/KA/',KAIdentifier,File),
%					save_module_safely(DbName:KAIdentifier,File)
%				);
%	logger:logAtLevel('I','Tried to save KA which doesnt appear to exist'),fail.
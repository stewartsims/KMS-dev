/* 
 * logger writes out to the KMSdb.log file given log statements with a timestamp and
 * indication of the type of log statement (level)
 * setLogLevel must first be called with one of the following options:
 * E --> only errors will be logged
 * D --> both errors and debug statements will be logged
 * I --> errors, debug statements and information statements will all be logged
 */
:- module(logger, []).


:- dynamic logLevel/1.

setLogLevel(Level) :- logLevel(_) -> retract(logLevel(_));
			    assert(logLevel(Level)).

logAtLevel(Level, Data) :- atom_concat(Level, ' ', Log),
				   atom_concat(Log, Data, LogData),
				   (
					(LogLevel='E',Level='E') -> logStatement(LogData);
				   	(LogLevel='D',(Level='E';Level='D')) -> logStatement(LogData);
				  	(LogLevel='I',(Level='E';Level='D';Level='I')) -> logStatement(LogData)
				   ).

logStatement(Data) :- 	     open('data/KMSdb.log', append, Stream),
				     get_time(Timestamp),
				     convert_time(Timestamp, TimeString),
				     atom_concat(TimeString, ': ',TimeFormatted),
				     atom_concat(TimeFormatted, Data, LogString),
				     atom_concat(LogString, '\n', LogStringFormatted),
			  	     write(Stream, LogStringFormatted),
				     close(Stream).
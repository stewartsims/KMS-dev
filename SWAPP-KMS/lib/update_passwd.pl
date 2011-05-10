:- module(update_passwd,
	  [ add_passwd/3,		% +File, +User, +Password
	    remove_passwd/2
	  ]).
:- use_module(library(crypt)).


add_passwd(File, User, Passwd) :-
    absolute_file_name(File, Path, [access(write)]),
    retractall(authenticate:passwd(User, _OldPath, _OldPasswd)),
    crypt(Passwd, EncryptedPasswd),
    assert(authenticate:passwd(User, Path, EncryptedPasswd)),
	open(File, write, Stream,
	    [ lock(write)
	    ]),    
    forall(authenticate:passwd(User0, _Path, EPasswd0),
        format(Stream, '~p:~@\n', [User0, format(EPasswd0)])),
    close(Stream).


remove_passwd(File, User) :-
    retractall(authenticate:passwd(User, _OldPath, _OldPasswd)),
	open(File, write, Stream,
	    [ lock(write)
	    ]),    
    forall(authenticate:passwd(User0, _Path, EPasswd0),
        format(Stream, '~p:~@\n', [User0, format(EPasswd0)])),
    close(Stream).
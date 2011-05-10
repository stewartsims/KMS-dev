SWAPP-KMS
---------

Licence
-------

In accordance with the wishes of the original authors of SWAPP (Jan Wielemaker and Torbjörn Lager), this part of the development platform is licensed under the LGPL licence.


Basic information
-----------------

This is the an open source tool which contains the Prolog API for the Knowledge Management Services project by Intelligent Architectures.


For more information on this development platform please see http://kms.intelligent-architectures.co.uk/

A Knowledge Agent (KA) is simply a module of Prolog code exposed through the web using the KMS-dev development platform. The system imports all files as KAs that exist in the directory data/KA
 when server.pl is started.

Modifications and additions to SWAPP
------------------------------------

New code includes:
api/kms_db.pl <-- the main modification based on session_db
api/kms_populate_ka.pl
api/ka_manager.pl
api/transport.pl
api/logger.pl

server.pl altered to import modules and setup the logger

The root / endpoint of the kms_db application is at localhost:port/swapp/kms/db


Running the server
------------------

Run server.pl and issue the command 'server(5000).' where 5000 is the desired port number to run the SWAPP-KMS server on.